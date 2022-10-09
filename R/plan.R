text_plan <- function(year = lubridate::year(lubridate::today())) {
  start <- lubridate::date(paste0(year, "-01-01"))
  end <- lubridate::date(paste0(year, "-12-31"))
  days <- seq.Date(start, end, by = "day")
  weeks <- lubridate::isoweek(days)
  start2 <- which(weeks == 1)[1]
  days <- days[start2:length(days)]
  weeks <- weeks[start2:length(weeks)]
  tibble::tibble(days = days, weeks = weeks) %>%
    dplyr::group_by(weeks) %>%
    dplyr::slice(1) |>
    dplyr::mutate(text = paste0(stringr::str_pad(weeks, 2, pad = "0"), "|", stringr::str_sub(days, 6), " ")) %>%
    dplyr::pull(text) %>%
    paste0(collapse = "\n") |>
    writeLines(file("clipboard"))
}
