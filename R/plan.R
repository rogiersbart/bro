#' Plain text year planning
#'
#' @param file File to write to, or "clipboard".
#' @param year The year to use, defaults to the current year.
#'
#' @return The text, invisibly if written first to a file.
#' @export
plan_year <- function(
    file = NULL,
    year = lubridate::year(lubridate::today())
  ) {
  start <- lubridate::date(paste0(year, "-01-01"))
  end <- lubridate::date(paste0(year, "-12-31"))
  days <- seq.Date(start, end, by = "day")
  weeks <- lubridate::isoweek(days)
  start2 <- which(weeks == 1)[1]
  days <- days[start2:length(days)]
  weeks <- weeks[start2:length(weeks)]
  txt <- tibble::tibble(days = days, weeks = weeks) |>
    dplyr::group_by(weeks) |>
    dplyr::slice(1) |>
    dplyr::mutate(text = paste0(stringr::str_pad(weeks, 2, pad = "0"), "|", stringr::str_sub(days, 6), " ")) |>
    dplyr::pull(text) |>
    paste0(collapse = "\n")
  txt <- glue::glue(txt)
  if (is.null(file)) return(txt)
  con <- file(file)
  writeLines(txt, con)
  close(con)
  invisible(txt)
}

#' Plain text week planning
#'
#' @param file File to write to, or "clipboard".
#'
#' @return The text, invisibly if written first to a file.
#' @export
plan_week <- function(
  file = NULL
) {
  # TODO add week argument, which is current one by default to add dates?
  days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  txt <- glue::glue(
    "## {days}\n",
    "- [ ] plan\n",
    "- [ ] ...\n",
    "- [ ] mail\n",
    "- [ ] teams\n",
    "- [ ] admin{ifelse(days == 'Friday', '', '\n')}"
  )
  if (is.null(file)) return(txt)
  con <- file(file)
  writeLines(txt, con)
  close(con)
  invisible(txt)
}
