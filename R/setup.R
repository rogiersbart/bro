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
      ".gitignore",
      ".ahk"
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

#' Setup utils
#'
#' @return Invisible NULL.
#' @export
setup_utils <- function() {
  setup_util(
    "https://www.autohotkey.com/download/ahk.zip",
    "AutoHotkeyU64.exe"
  )
  setup_util(
    "https://www.zhornsoftware.co.uk/caffeine/caffeine.zip",
    "caffeine64.exe"
  )
  setup_util(
    "https://dennisbabkin.com/php/download.php?go=ctm",
    "Compact Tray Meter.exe"
  )
  invisible()
}
setup_util <- function(zip, file) {
  path <- tempfile(fileext = ".zip")
  if (grepl("php", zip)) {
    download.file(zip, path, method = "curl", extra = "-L")
  } else {
    download.file(zip, path)
  }
  unzip(path, file, exdir = fs::path_home("bin"))
  fs::file_delete(path)
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
