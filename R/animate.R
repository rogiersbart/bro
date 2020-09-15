#' Animate a set of pngs
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
                    delay = 2, loop = TRUE,
                    max_size = 1200, time_last = 2) {
  image_files <- fs::dir_ls(dir, glob = glob, regexp = regexp)
  rui::approve("Found {length(image_files)} {.path .{fs::path_ext(image_files[1])}} images")
  if (fs::path_ext(image_files[1]) == "jpg") {
    rui::inform("Note that {.code gifski::gifski()} only works with {.path .png} files")
    rui::inform("I will attempt converting your images now")
    rui::begin("Converting {.path .jpg} to {.path .png}")
    convert_jpg_to_png(image_files)
    rui::succeed()
    png_files <- image_files %>% fs::path_ext_set("png")
  } else {
    png_files <- image_files
  }

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
    gif_file = fs::path(dir, gif),
    width = info$width,
    height = info$height,
    delay = delay,
    loop = loop,
    progress = FALSE
  )
  rui::succeed()
  if (fs::path_ext(image_files[1]) == "jpg") {
    rui::begin("Removing {.path .png} files")
    fs::file_delete(unique(png_files))
    rui::succeed()
  }
  rui::inform("You can find the animation at:")
  rui::inform("{.path {fs::path(dir, gif)}}")
  invisible()
}

convert_jpg_to_png <- function(paths) {
  new_paths <- paths %>%
    fs::path_ext_set("png")
  purrr::walk2(
    paths,
    new_paths,
    ~ magick::image_read(.x) %>%
      magick::image_convert("png") %>%
      magick::image_write(.y)
  )
}
