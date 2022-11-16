#' Download photos from ouders.broekx.be
#'
#' @param url Link in the email to a page with images.
#'
#' @return
#' @export
broekx_download <- function(url) {
  rui::begin("Looking for images")
  imgs <- rvest::read_html(url)  |>
    rvest::html_element(".center-img") |>
    rvest::html_elements("a") |>
    rvest::html_attr("href")
  rui::inform("Found {length(imgs)} images")
  rui::proceed("Downloading")
  purrr::walk2(
    paste0("https://ouders.broekx.be/", imgs),
    str_pad(1:length(imgs), nchar(length(imgs)), "left", "0"),
    ~download.file(.x, paste0(.y, ".jpg"), method = "curl", quiet = TRUE)
  )
  rui::succeed()
  invisible()
}
