#' Toggle between light and dark RStudio solarized themes
#'
#' The {rsthemes} solarized themes are used here, as they provide more
#' consistent use of italics, better highlighting, etc. as the built-in
#' solarized themes.
#'
#' @export
toggle <- function() {
  current <- rstudioapi::getThemeInfo()$editor
  if (grepl("Light", current)) dark()
  if (grepl("Dark", current)) light()
}
#' @export
#' @rdname toggle
light <- function() {
  rstudioapi::applyTheme("Solarized Light {rsthemes}")
}
#' @export
#' @rdname toggle
dark <- function() {
  rstudioapi::applyTheme("Solarized Dark {rsthemes}")
}

#' Edit a file in RStudio
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
edit <- function(path) {
  rstudioapi::navigateToFile(path)
  invisible()
}
