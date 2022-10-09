#' Launch git bash
#'
#' This behaves the same way as launching git bash through the start menu,
#' explorer context menu or executable directly. The HOME environment variable
#' is adjusted, as R uses the Documents subfolder of the standard HOME directory
#' in git bash, so ".bash_profile" is correctly sourced on startup. The working
#' directory will correspond to that of the R session.
#'
#' @export
bash <- function() {
  home <- Sys.getenv("HOME")
  on.exit(Sys.setenv(HOME = home))
  username <- Sys.getenv("USERNAME")
  Sys.setenv(HOME = glue::glue('C:/Users/{username}'))
  system(
    glue::glue("C:/Users/{username}/AppData/Local/Programs/Git/git-bash.exe"),
    wait = FALSE,
    invisible = FALSE
  )
  invisible()
}
