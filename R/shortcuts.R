#' Shortcuts for interactive use
#'
#' @name shortcuts
NULL

#' @param name Package name to check
#' @export
#' @rdname shortcuts
pkg <- function(name) {
  length(find.package(name, quiet = TRUE)) != 0
}

#' @export
#' @rdname shortcuts
last <- function() {
  .Last.value
}

#' @export
#' @rdname shortcuts
serve <- function() {
  if ("index.Rmd" %in% (dir(here::here())) &&
      any(grepl("blogdown", readLines("index.Rmd")))) {
    blogdown::serve_site()
  } else if ("docs" %in% (dir(here::here()))) {
    servr::httw("docs")
  } else {
    servr::httw()
  }
}

#' @export
#' @rdname shortcuts
git <- function() {
  usethis::git_vaccinate()
  usethis::use_git_config(user.name = "rogiersbart",
                          user.email = "rogiers.bart@gmail.com")
}
