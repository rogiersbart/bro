#' Download photos from ouders.broekx.be
#'
#' @param url Email link to a page with images. If NULL (default), the URL is
#'   taken from the clipboard.
#' @param to Directory to save the images. If NULL (default), the current
#'   working directory is used.
#' @return
#' @export
broekx_download <- function(url = NULL, to = NULL) {
  if (is.null(url)) url <- utils::readClipboard()[1]
  if (!is.null(to)) {
    if (!fs::dir_exists(to)) fs::dir_create(to)
  }
  if (is.null(to)) to <- fs::path_wd()
  rui::begin("Looking for images")
  imgs <- rvest::read_html(url)  |>
    rvest::html_element(".center-img") |>
    rvest::html_elements("a") |>
    rvest::html_attr("href")
  rui::inform("Found {length(imgs)} images")
  rui::proceed("Downloading")
  purrr::walk2(
    paste0("https://ouders.broekx.be/", imgs),
    stringr::str_pad(1:length(imgs), nchar(length(imgs)), "left", "0"),
    ~ download.file(
      .x,
      fs::path(to, paste0(.y, ".jpg")),
      method = "curl",
      quiet = TRUE
    )
  )
  rui::succeed()
  invisible()
}
