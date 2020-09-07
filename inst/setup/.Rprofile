if (interactive()) {
  .light <- function() rstudioapi::applyTheme("solarized light")
  .dark <- function() rstudioapi::applyTheme("solarized dark")
  .last <- function() .Last.value
  .serve <- function() {
    if ("index.Rmd" %in% (dir(here::here())) &&
        any(grepl("blogdown", readLines("index.Rmd")))) {
      blogdown::serve_site()
    } else if ("docs" %in% (dir(here::here()))) {
      servr::httw("docs")
    } else {
      servr::httw()
    }
  }
  if (length(find.package("usethis", quiet = TRUE)) != 0) {
    usethis::use_git_config(user.name = "rogiersbart",
                            user.email = "rogiers.bart@gmail.com")
  }
}
options(
  # usethis
  usethis.full_name = "Bart Rogiers",
  usethis.description = list(
    `Authors@R` = 'person("Bart",
                          "Rogiers",
                          email = "rogiers.bart@gmail.com",
                          role = c("aut", "cre"),
                          comment = c(ORCID = "0000-0002-8836-0988",
                                      url = "<https://rogiersbart.github.io>"))',
    License = "MIT + file LICENSE",
    Version = "0.0.0.9000"
  ),
  usethis.protocol = "https",

  # RMODFLOW
  RMODFLOW.path = "D:/bin/RMODFLOW",

  # themeswitcher
  themeswitcher.dark = "solarized dark",
  themeswitcher.light = "solarized light"

  # # other
  # error = rlang::entrace, # trimmed tracebacks when using pipes
  # rlang_backtrace_on_error = "branch" # trimmed tracebacks when using pipes
)
