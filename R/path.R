#' Fix backslashes in a windows file path on your clipboard
#'
#' Shift + Right-click in the Windows file Explorer allows you to copy a file
#' path, but it has backslashes. With this function, you get the forward
#' slash version in your clipboard, so it can be used inside your R code.
#'
#' @export
path_fix <- function() {
  path <- readClipboard() %>%
    gsub("\\\\", "/", .)
  path %>%
    writeClipboard()
  rui::approve("Your path has forward slashes now:")
  rui::inform("{.path {path}}")
  invisible()
}
