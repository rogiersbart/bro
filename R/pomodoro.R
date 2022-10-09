#' Title
#'
#' @param minutes
#'
#' @return
#' @export
#'
#' @examples
pomodoro <- function(minutes = 25) {
  old_title <- utils::getWindowTitle()
  on.exit({
    utils::setWindowTitle(title = old_title)
    rui::clear()
  })
  # TODO make tick timing more steady?
  # IDEA maybe combine multiple in single audio play call?
  start <- Sys.time()
  now <- Sys.time()
  tick <- audio::load.wave("C:/Windows/Media/Windows Navigation Start.wav")
  tick <- cbind(tick, tick * 0) # not sure why this is required
  done <- audio::load.wave("C:/Windows/Media/Windows Notify Calendar.wav")
  rui::begin("{minutes}:00")
  while (as.numeric(now - start, "mins") < minutes) {
    audio::play(tick)
    Sys.sleep(1)
    now <- Sys.time()
    passed <- as.numeric(now - start, "secs")
    mins <- stringr::str_pad(max(0, minutes - (passed %/% 60 + 1)), width = 2, pad = "0")
    secs <- round(passed %% 60)
    if (secs != 0) secs <- 60 - secs
    if (passed >= minutes * 60) secs <- 0
    secs <- stringr::str_pad(secs, width = 2, pad = "0")
    rui::proceed("{mins}:{secs}")
    utils::setWindowTitle(title = paste0(mins, ":", secs, " ..."))
  }
  audio::play(done)
  rui::succeed()
  invisible()
}
