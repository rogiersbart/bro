#' Setup a .Rprofile file
#'
#' @param scope Edit globally for the current **user**, or locally for the
#'     current **project**
#' @return
#' @export
setup_r_profile <- function(scope = "user") {
  source_file <- system.file("setup", ".Rprofile", package = "br")
  if (scope == "user") {
    fs::file_copy(source_file, "~/.Rprofile")
  }
  if (scope == "project") {
    fs::file_copy(source_file, here::here(".Rprofile"))
  }
  invisible()
}

setup_r_environ <- function(scope = "user") {

}

setup_git_config <- function(scope = "user") {

}

setup_rstudio_snippets <- function() {

}
