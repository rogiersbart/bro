#' Setup the .Renviron file
#'
#' @export
setup_r_environ <- function() {
  writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
  invisible()
}
