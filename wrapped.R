# Creating my #rstats wrapped summary

# global ------------------------------------------------------------------

library(janitor)
library(tidyverse)
library(patchwork)
library(showtext)
library(ggtextures)
library(magick)

# functions ---------------------------------------------------------------

# from NCmisc package
list.functions.in.file <- function(filename, alphabetic=TRUE) {
  library(NCmisc)
  # from hrbrmstr, StackExchange 3/1/2015
  if(!file.exists(filename)) { stop("couldn't find file ",filename) }
  if(!get.ext(filename)=="R") { warning("expecting *.R file, will try to proceed") }
  # requireNameSpace("dplyr")
  tmp <- getParseData(parse(filename, keep.source=TRUE))
  # next line does what dplyr used to do!
  nms <- tmp$text[which(tmp$token=="SYMBOL_FUNCTION_CALL")]
  # funs <- unique(if(alphabetic) { sort(nms) } else { nms })
  funs <- if(alphabetic) { sort(nms) } else { nms } # removed unique() here
  #crit <- quote(token == "SYMBOL_FUNCTION_CALL")
  #tmp <- dplyr::filter(tmp, .dots = crit)
  #tmp <- dplyr::filter(tmp,token=="SYMBOL_FUNCTION_CALL")
  #tmp <- unique(if(alphabetic) { sort(tmp$text) } else { tmp$text })
  src <- paste(as.vector(sapply(funs, find)))
  outlist <- tapply(funs, factor(src), c)
  return(outlist)
}

# load files --------------------------------------------------------------

parent <- "/Users/willsimmons/Documents/WCM"
all_r_files <- list.files(parent, recursive = TRUE, full.names = TRUE) |>
  as_tibble() |>
  rename(filename = value) |>
  filter(!str_detect(filename, "renv|wcmtheme") &  # not external package files
           (str_detect(filename, "\\.R$") | str_detect(filename, "\\.Rmd$"))) |>
  mutate(filetype = basename(filename) |> str_extract("\\..*"))

# count functions ---------------------------------------------------------

test <- list.functions.in.file(all_r_files$filename[[1]])

extract_packages_functions <- function(filename){

  extract_r <- function(filename){
    pkgs <- list.functions.in.file(filename) %>%
      names(.) %>%
      .[. != "character(0)"] |>
      str_remove_all("package:") |>
      str_remove_all("c\\(") |>
      str_remove_all("[^A-Za-z0-9 ]") |>
      map(str_split, " ") |>
      unlist()

    fns <- list.functions.in.file(filename) |>
      unlist() |>
      unname()

    return(
      list(pkgs = pkgs,
           fns = fns)
    )
  }

  extract_rmd <- function(filename){
    base_name <- basename(filename)

    output <- glue::glue("{tempdir()}/{base_name}") |>
      str_remove_all(".Rmd") |>
      paste0(".R")

    res <- knitr::purl(
      input = filename,
      output = output,
      quiet = TRUE
    ) |>
      list.functions.in.file()

    extract_r(res)
  }

  filetype <- basename(filename) |>
    str_extract("\\..*")

  stopifnot("filetype not usable!" = filetype %in% c(".R", ".Rmd"))
  if(filetype == ".R")
    res <- extract_r(filename)
  if(filetype == ".Rmd")
    res <- extract_rmd(filename)

  res

}

# some files are giving errors - need to use possibly() to allow map() to continue despite errors
# possibly_extract <- possibly(.f = extract_packages_functions,
#                              otherwise = list(pkgs = "error",
#                                               fns = "error"))

# test
# possibly_extract("/Users/willsimmons/Documents/WCM/Contracts/Flory, James/Documents/readings/comparative ITS/gasparrini_supplemental/smokbansicily.R")

# extract -----------------------------------------------------------------

# run 2022-12-06
# packages_functions <- all_r_files |>
#   mutate(
#     packages = map(.x = filename,
#                     ~possibly_extract(.x)[["pkgs"]]),
#     functions = map(.x = filename,
#                     ~possibly_extract(.x)[["fns"]])
#   )
#
# saveRDS(packages_functions, here::here("data/packages_functions.rds"))

# pull top 5 --------------------------------------------------------------

packages_functions <- readRDS(here::here("data/packages_functions.rds"))

# how many distinct scripts was a given package used in? (counted at most 1x/script)
all_pkgs <- packages_functions |>
  mutate(
    packages = map(packages, unique),  # get unique packages for each script
    l = map_dbl(packages, length)  # remove empty vectors
  ) |>
  filter(l > 0) |> select(-l) |>
  unnest(packages) |>
  filter(packages != "error")  # from possibly()

top_5_packages <- all_pkgs |>
  count(packages, sort = TRUE) |>
  filter(packages != "base") |>  # exclude base package
  slice_max(n, n = 5, with_ties = FALSE) |>
  mutate(img = c("https://raw.githubusercontent.com/rstudio/hex-stickers/main/PNG/dplyr.png",
                 "https://raw.githubusercontent.com/rstudio/hex-stickers/main/PNG/tidyr.png",
                 "https://www.r-project.org/logo/Rlogo.png",
                 "https://raw.githubusercontent.com/rstudio/hex-stickers/main/PNG/purrr.png",
                 "https://raw.githubusercontent.com/sfirke/janitor/main/man/figures/logo_small.png"))

# how many times was a distinct function EVER used? (counted 0 to Inf times per script)
all_fns <- packages_functions |>
  mutate(l = map_dbl(functions, length)) |>  # remove empty vectors
  filter(l > 0) |> select(-l) |>
  unnest(functions) |>
  filter(functions != "error")  # from possibly()

top_5_functions <- all_fns |>
  count(functions, sort = TRUE) |>
  filter(functions != "library") |>
  slice_max(n, n = 5, with_ties = FALSE) |>
  mutate(img = c("https://www.r-project.org/logo/Rlogo.png",
                 "https://raw.githubusercontent.com/rstudio/hex-stickers/main/PNG/dplyr.png",
                 "https://raw.githubusercontent.com/rstudio/hex-stickers/main/PNG/dplyr.png",
                 "https://raw.githubusercontent.com/rstudio/hex-stickers/main/PNG/dplyr.png",
                 "https://raw.githubusercontent.com/rstudio/hex-stickers/main/PNG/RStudio.png"))

# visualize ---------------------------------------------------------------

font_add_google("Raleway", "raleway")
showtext_auto()

p <- ggplot() +
  # add text with the numbers 1 to 5
  geom_text(data = data.frame(),
            mapping = aes(x = rep(1, 5),
                          y = 1:5,
                          label = paste0("#", 1:5)),
            colour = "#1a2e72",
            size = 20,
            fontface = "bold",
            family = "raleway") +
  # add text with the names of the functions, and the number of times its used
  geom_text(data = top_5_functions,
            mapping = aes(x = rep(2.25, 5),
                          y = 1:5,
                          label = paste0(functions, "(), ", n, " times")),
            colour = "#143b1c",
            hjust = 0,
            size = 11,
            fontface = "bold",
            family = "raleway") +
  # add images for each package
  geom_textured_rect(data = top_5_functions,
                     aes(xmin = rep(1.5, 5), xmax = rep(2.1, 5),
                         ymax = 1:5-0.3, ymin = 1:5+0.3, image = img),
                     lty = "blank",
                     fill = "transparent",
                     nrow = 1,
                     ncol = 1,
                     img_width = unit(1, "null"),
                     img_height = unit(1, "null"),
                     position = "identity")  +
  # add title using geom_text() instead of labs()
  geom_text(data = data.frame(),
            aes(x = 2.45, y = 0, label = "My Top Functions"),
            colour = "#1a2e72",
            fontface = "bold",
            hjust = 0.5,
            size = 14,
            family = "raleway") +
  # set axis limits and reverse y axis
  scale_x_continuous(limits = c(0.9, 4)) +
  scale_y_reverse(limits = c(5.5, -0.2)) +
  # add a caption
  labs(caption = "#TidyTuesday") +
  # set the theme
  theme_void() +
  theme(plot.background = element_rect(fill = "#96efb7", colour = "#96efb7"),
        panel.background = element_rect(fill = "#96efb7", colour = "#96efb7"),
        plot.margin = margin(40, 15, 10, 15),
        plot.caption = element_text(colour = "#1a2e72",
                                    margin = margin(t = 15),
                                    face = "bold",
                                    hjust = 1,
                                    size = 30,
                                    family = "raleway"))

p
