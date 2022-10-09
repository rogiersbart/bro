gallery <- function(drive_folder) {
  gallery_init(drive_folder)
  gallery_items(drive_folder)
  gallery_build()
  gallery_favicon(drive_folder)
}

# TODO
# - export only single function in the end? re-inits when project is empty, updates items always, etc.?
# - expose footer text? - take from project yml!!

#' Initiate a new hugrid site for local testing
#'
#' @export
gallery_init <- function(drive_folder) {
  blogdown::new_site(theme = "aerohub/hugrid",
                     theme_example = TRUE,
                     serve = FALSE)
  Sys.sleep(1)
  purrr::walk(
    c("archetypes", "content", "public", "resources", "static",
      "themes/hugrid/exampleSite"),
    fs::dir_delete
  )
  project <- drive_folder %>% fs::path_file()
  yml_file <- tempfile()
  googledrive::drive_download(paste0("~/", drive_folder, "/", project, ".yml"), yml_file)
  yml_contents <- yaml::read_yaml(yml_file)
  title <- yml_contents$title
  subtitle <- yml_contents$subtitle
  fs::file_delete(yml_file)
  id <- googledrive::drive_ls(drive_folder) %>%
    dplyr::filter(stringr::str_detect(name, ".png$")) %>%
    dplyr::pull(id) %>%
    as.character()
  cat(glue::glue('
# Site settings
baseurl = "https://rogiersbart.github.io/{project}/"
languageCode = "en-uk"
title = "{stringr::str_to_sentence(project)}"
theme = "hugrid"
# Enter your tracking code to enable Google Analytics
googleAnalytics = "UA-XXXXXXXX-Y"

contentdir = "content"
datadir = "data"
layoutdir = "layouts"
publishdir = "docs"
ignoreFiles = ["\\\\.Rmd$", "\\\\.Rmarkdown$", "_files$", "_cache$"]

[params]
    # Meta
    title = "<img src=\'{add_prefix_file(id)}\' style=\'height:15rem;\'><br>[{title}](https://rogiersbart.github.io/{project})"
    subtitle = "{subtitle}"
    author = "Bart Rogiers [rogiersbart]"
    description = ""
    keywords = ""

    # Body background color
    bodybgcolor = "#ffffff"

    # Preview container button text
    buttontext = "Source"

    # Footer text
    footertext = \'<center><p xmlns:dct="http://purl.org/dc/terms/" xmlns:cc="http://creativecommons.org/ns#" class="license-text">This work by <a rel="cc:attributionURL dct:creator" property="cc:attributionName" href="https://rogiersbart.github.io/">Bart Rogiers</a> is licensed under <a rel="license" href="https://creativecommons.org/licenses/by/4.0">CC BY 4.0.</a></p></center><br>\'

    # add extra-css
    # custom_css = ["css/extra1.css", "css/extra2.css"]

[markup]
  [markup.goldmark]
    [markup.goldmark.renderer]
      unsafe = true

'), file = "config.toml")

  default_css <- "themes/hugrid/static/css/default.css"
  readLines(default_css) %>%
    purrr::map_chr(~stringr::str_replace(., "family=Lato", "family=Fira+Code")) %>%
    purrr::map_chr(~stringr::str_replace(., "'Lato'", "'Fira Code'")) %>%
    purrr::map_chr(~stringr::str_replace(., "color: #555;", "color: #777;")) %>%
    purrr::map_chr(~stringr::str_replace(
      .,
      "padding: 60px 30px 50px;",
      "padding: 60px 30px 0px;" # this does not seem to work properly yet?
    )) %>%
    writeLines(default_css)

  invisible()
}

#' Write a hugo hugrid items.toml file based on a google drive folder
#'
#' This function allows you to create an image gallery type of website, using
#' the [hugo hugrid theme](https://themes.gohugo.io/hugrid/), with minimal setup
#' in a google drive folder. Files in the folder starting with the
#' folder/project name are omitted. The rest is processed in order, and added to
#' the items.toml file. Everything works by linking to google drive, so files
#' are hosted there, but all files should hence be available through a shared
#' link. Details for setup are provided below.
#'
#' Every **image file** should start with a code, then a name, and finally a
#' type, separated with underscores (*e.g.* `code_name_type.jpg`). Types can be
#' `thumb`, for the thumbnail images on the main page grid, `image` for the
#' image used for display when the thumbnail is clicked, and `url` for the file
#' that is linked to when clicking the button that is foreseen in the theme.
#'
#' If other types are included, these are added as separate buttons, by
#' appending an html <a></a> tag to the description (explained below). For the
#' button text, the type is  used, but underscores are replaced by spaces.
#'
#' A **csv file** should be included as well, with a column `code` that links
#' the rows with the image files, and columns `title` and `description`, which
#' are displayed next to the `image` type file.
#'
#' @param drive_folder full path to google drive folder without trailing slash
#' @param path path to the `items.toml` file to be created
#' @export
gallery_items <- function(
  drive_folder,
  path = "data/items.toml"
) {
  googledrive::drive_auth(TRUE)
  project_name <- drive_folder %>% fs::path_file()
  df <- googledrive::drive_ls(paste0("~/", drive_folder, "/items/"),
                              recursive = TRUE) %>%
    dplyr::select(name, id)
  df <- df %>%
    dplyr::mutate(id = as.character(id)) %>%
    tidyr::separate(name, into = c("code", "type", "extension"),
                    sep = "_|\\.") %>%
    dplyr::select(-extension) %>%
    dplyr::mutate(type = type %>% stringr::str_replace_all("-", "_")) %>%
    dplyr::mutate(type = ifelse(is.na(type), "folder", type)) %>%
    tidyr::spread("type", "id") %>%
    dplyr::arrange(dplyr::desc(code)) %>%
    dplyr::select(code, thumb, image, folder) %>%
    dplyr::filter(! is.na(thumb))
  yml_file <- tempfile()
  googledrive::drive_download(paste0("~/", drive_folder, "/items.yml"), yml_file)
  yml_contents <- yaml::read_yaml(yml_file) %>%
    unlist() %>%
    tibble::enframe() %>%
    dplyr::mutate(id = cumsum(name == "code")) %>%
    tidyr::spread(name, value) %>%
    dplyr::select(-id)
  df2 <- df %>%
    dplyr::left_join(yml_contents) %>%
    tidyr::replace_na(list(title = "", description = ""))

  # # add links extra types
  #
  # extras <- df2 %>%
  #   dplyr::select(-any_of(c("code", "title", "image", "thumb", "url",
  #                           "description", "omit")))
  # columns_to_add <- names(extras) %>% stringr::str_replace_all("_", " ")
  # if (length(columns_to_add) > 0) {
  #   for (i in 1:length(columns_to_add)) {
  #     df2$description <- paste0(
  #       df2$description,
  #       ifelse(
  #         is.na(extras[,i]),
  #         "",
  #         glue::glue("<br><a target='_blank' href='{add_prefix_folder(extras[[i]])}'>{columns_to_add[i]}</a>")
  #       )
  #     )
  #   }
  # }
  rui::begin("Writing {.path {path}} file")
  cat("", file = path)
  for (i in 1:nrow(df)) {
    cat("[[items]]\n", file = path, append = TRUE)
    cat('title = "', df2$title[i], '"\n', file = path, sep = "", append = TRUE)
    cat('alt = "', df2$title[i], '"\n', file = path, sep = "", append = TRUE)
    cat('image = "', add_prefix_file(df2$image[i]), '"\n', file = path, sep = "", append = TRUE)
    cat('thumb = "', add_prefix_file(df2$thumb[i]), '"\n', file = path, sep = "", append = TRUE)
    cat('description = "', df2$description[i], '"\n', file = path, append = TRUE)
    if (df2$title[i] != "About") {
      cat('url = "', add_prefix_folder(df2$folder[i]), '"\n', file = path, sep = "", append = TRUE)
    }
  }
  rui::succeed()
  invisible()
}

#' Title
#'
#' @param ...

#' @export
gallery_build <- function(...) {
  blogdown::build_site(...)
}

gallery_favicon <- function(drive_folder) {
  id <- googledrive::drive_ls(drive_folder) %>%
    dplyr::filter(stringr::str_detect(name, ".ico$")) %>%
    dplyr::pull(id) %>%
    as.character()
  readLines("docs/index.html") %>%
    stringr::str_replace(
      '<link rel="icon" href="favicon.ico" />',
      glue::glue('<link rel="icon" href="{add_prefix_file(id)}" />')
    ) %>%
    writeLines("docs/index.html")
}




#' Title
#'
#' @param ...
#'
#' @export
gallery_serve <- function(...) {
  blogdown::serve_site(...)
}

gallery_resize <- function(path) {
  img <- magick::image_read(path)
  inf <- magick::image_info(img)
  img <- magick::image_scale(img, "1200x1200")
  magick::image_write(
    img,
    paste0(
      fs::path_dir(path),
      "/",
      # TODO rename all files and include number
      # replace part after first underscore with "image"
      stringr::str_sub(
        fs::path_file(path),
        end = str_locate(fs::path_file(path), "_")
      )[1],
      "image.",
      fs::path_ext(path)
    ),
    quality = 90
  )
}



add_prefix_file <- function(url) {
  prefix <- "https://drive.google.com/uc?export=view&id="
  ifelse(is.na(url), "", paste0(prefix, url))
}

add_prefix_folder <- function(url) {
  prefix <- "https://drive.google.com/drive/folders/"
  ifelse(is.na(url), "", paste0(prefix, url))
}
