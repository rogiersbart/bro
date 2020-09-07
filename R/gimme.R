#' Download high-resolution photos from gimme.eu based on a saved html
#'
#' @param path folder where you saved the album html page, and the individual
#'             photos should be put instead, defaults to current working
#'             directory
#' @param max_size maximum number of pixels for image width and height, defaults
#'                 to 4000
#' @export
gimme_download <- function(path = getwd(), max_size = 4000) {

  # go to album, and save html

  df <- tibble::tibble(path = fs::dir_ls(glue::glue("{path}/Gimme_files"), regex = ".jpeg$"),
                       name = basename(path)) %>%
    dplyr::filter(! stringr::str_detect(name, "\\).jpeg")) %>%
    dplyr::mutate(info = purrr::map(path, ~magick::image_read(.x) %>%
                                      magick::image_info()),
                  width = purrr::map_int(info, ~.x$width[1]),
                  height = purrr::map_int(info, ~.x$height[1]),
                  size = ifelse(width >= height,
                                glue::glue("{max_size}x0"),
                                glue::glue("0x{max_size}")),
                  url = glue::glue("https://img.gimme.eu/unsafe/{size}/",
                                   "storage.googleapis.com/gimme-production-uploads/",
                                   "{name}"))

  purrr::walk2(df$url, df$name, ~ magick::image_read(.x) %>%
                 magick::image_write(glue::glue("{path}/", .y)))

  fs::file_chmod(glue::glue("{path}/Gimme_files"), "rw-")
  fs::file_delete(glue::glue("{path}/Gimme_files"))
  fs::file_delete(glue::glue("{path}/Gimme.html"))

  # alternative, login with rvest::html_session and rvest::html_form
  # get image urls from online album html

  # or try using chrome cookies of gimme.eu?
  # https://stackoverflow.com/questions/31021764/where-does-chrome-store-cookies

  invisible()
}
