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
#' that is linked to when clicking the button.
#'
#' A **csv file** should be included as well, with a column `code` that links
#' the rows with the image files, and columns `title` and `description`, which
#' are displayed next to the `image` type file.
#'
#' @param drive_folder full path to google drive folder without trailing slash
#' @param path path to the `items.toml` file to be created
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' write_hugrid_items("my/drive/folder")
#' }
write_hugrid_items <- function(
  drive_folder,
  path = here::here("data/items.toml")
) {
  project_name <- drive_folder %>% fs::path_file()
  df <- googledrive::drive_ls(paste0("~/", drive_folder, "/")) %>%
    dplyr::select(name, id)
  csv <- df %>%
    dplyr::filter(name %>% stringr::str_detect(".csv$"))
  df <- df %>%
    dplyr::filter(name %>% stringr::str_detect("^espresso", TRUE)) %>%
    tidyr::separate(name, into = c("code", "title", "type", "extension"),
                    sep = "_|\\.") %>%
    dplyr::select(-extension) %>%
    tidyr::spread("type", "id") %>%
    dplyr::arrange(dplyr::desc(code))
  prefix <- "https://drive.google.com/uc?export=view&id="
  add_prefix <- function(url) {
    if (is.na(url)) return("")
    paste0(prefix, url)
  }
  csv_file <- tempfile()
  googledrive::drive_download(paste0("~/", drive_folder, "/", csv$name), csv_file)
  df2 <- df %>%
    dplyr::rename(filename_title = title) %>%
    dplyr::left_join(readr::read_csv(csv_file)) %>%
    tidyr::replace_na(list(title = "", description = ""))
  cat("", file = path)
  for (i in 1:nrow(df)) {
    cat("[[items]]\n", file = path, append = TRUE)
    cat('title = "', df2$title[i], '"\n', file = path, sep = "", append = TRUE)
    cat('alt = "', df2$title[i], '"\n', file = path, sep = "", append = TRUE)
    cat('image = "', add_prefix(df2$image[i]), '"\n', file = path, sep = "", append = TRUE)
    cat('thumb = "', add_prefix(df2$thumb[i]), '"\n', file = path, sep = "", append = TRUE)
    cat('description = "', df2$description[i], '"\n', file = path, append = TRUE)
    cat('url = "', add_prefix(df2$url[i]), '"\n', file = path, sep = "", append = TRUE)
  }
  invisible()
}
