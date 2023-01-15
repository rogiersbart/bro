#' Setup dotfiles
#'
#' @param scope Edit globally for the current **user**, or locally for the
#'     current **project**
#' @return Invisible NULL.
#' @export
setup_dotfiles <- function(scope = "user") {
  purrr::walk(
    c(
      ".Rprofile",
      ".bash_profile",
      ".minttyrc",
      ".nanorc",
      ".gitconfig",
      ".gitignore"
    ),
    setup_dotfile,
    scope = scope
  )
  invisible()
}
setup_dotfile <- function(scope, name) {
  if (scope == "user") {
    folder <- if (name == ".Rprofile") "~" else Sys.getenv("USERPROFILE")
    path <- glue::glue("{folder}/{name}")
    if (normalizePath("~") != Sys.getenv("USERPROFILE")) {
      rui::warn("HOME folder is {.path {normalizePath(\"~\")}} and",
                "not {.path {Sys.getenv(\"USERPROFILE\")}}!")
    }
  }
  if (scope == "project") path <- here::here(name)
  if (!exists("path", inherits = FALSE)) rui::error("Wrong scope")
  download.file(
    glue::glue("https://raw.githubusercontent.com/rogiersbart/dotfiles/main/{name}"),
    path,
    quiet = TRUE
  )
}

#' Setup PowerToys
#'
#' @return Invisible NULL.
#' @export
setup_powertoys <- function() {
  purrr::walk(
    c(
      "FancyZones/custom-layouts.json",
      "FancyZones/layout-hotkeys.json",
      "FancyZones/settings.json",
      "Keyboard Manager/default.json"
    ),
    setup_powertoys_json
  )
  invisible()
}
setup_powertoys_json <- function(json) {
  path <- glue::glue("C:/Users/{Sys.getenv('USERNAME')}/AppData/Local/Microsoft/PowerToys/{json}")
  json <- gsub(" ", "%20", json)
  download.file(
    glue::glue("https://raw.githubusercontent.com/rogiersbart/dotfiles/main/PowerToys/{json}"),
    path,
    quiet = TRUE
  )
}

sitrep <- function() {

  # report user & machine name

  # check the above dotfiles & utils

  # check installed applications

    # google drive?
    # google chrome
    # keepassxc?
    # r
    # rtools
    # rstudio
    # python/r-miniconda?
    # imageglass
    # krita
    # gimp
    # goxel/magicavoxel?
    # contact sync
    # ogcalc sync
    # jabref
    # vlc?
    # inkscape?

  # check git config?

}

setup_rstudio_snippets <- function() {

}
