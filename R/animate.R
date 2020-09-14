#' Animate a set of images
#'
#' @param dir
#' @param gif
#' @param regexp
#' @param glob
#' @param delay
#' @param loop
#' @param max_size
#' @param time_last
#' @export
animate <- function(dir, gif = "animation.gif", regexp = NULL, glob = NULL,
                    delay = 1, loop = TRUE,
                    max_size = 1200, time_last = 2) {
  png_files <- fs::dir_ls(dir, glob = glob, regexp = regexp)
  rui::approve("Found {length(png_files)} .{fs::path_ext(png_files[1])} images")
  rui::begin("Checking dimensions")
  info <- magick::image_read(png_files[length(png_files)]) %>%
    magick::image_info()
  original_max_size <- max(info$width, info$height)
  if (original_max_size > max_size) {
    info$width <- round(info$width / original_max_size * max_size)
    info$height <- round(info$height / original_max_size * max_size)
  }
  rui::succeed()
  if (time_last != 1) {
    png_files <- c(png_files, rep(png_files[length(png_files)], time_last))
  }
  rui::begin("Combining frames")
  gifski::gifski(
    png_files,
    gif_file = fs::path(dir, gif_file),
    width = info$width,
    height = info$heigth,
    delay = delay,
    loop = loop,
    progress = FALSE
  )
  rui::succeed()
  invisible()
}
