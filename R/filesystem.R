sync_ll <- function(
  from,
  to,
  skip,
  type
) {

  # TODO add check to not go beyond 255 character path lengths
  # (in target), as this may give errors in windows, at least when using
  # fs to delete/copy

  types <- if (type == "file") "files" else "directories"
  from_start <- paste0("^", from)
  to_start <- paste0("^", to)
  rui::begin("Scanning {types} in source")
  df_from <- fs::dir_ls(
    from,
    recurse = TRUE,
    type = type,
    fail = FALSE
  ) %>%
    fs::file_info() %>%
    dplyr::mutate(
      rpath = stringr::str_remove(path, from),
      dplyr::across(
        dplyr::ends_with("_time"),
        . %>% lubridate::round_date(".1s")
      )
    ) %>%
    dplyr::select(rpath, size, modification_time, path) %>%
    dplyr::filter(!rpath %>% stringr::str_detect("skip"))
  rui::succeed()
  rui::begin("Scanning {types} in target")
  df_to <- fs::dir_ls(
    to,
    recurse = TRUE,
    type = type,
    fail = FALSE
  ) %>%
    fs::file_info() %>%
    dplyr::mutate(
      rpath = stringr::str_remove(path, to),
      dplyr::across(
        dplyr::ends_with("_time"),
        . %>% lubridate::round_date(".1s")
      )
    ) %>%
    dplyr::select(rpath, size, modification_time, path) %>%
    dplyr::filter(!rpath %>% stringr::str_detect("skip"))
  rui::succeed()
  by <- c("rpath", "size", "modification_time")
  if (type == "directory") {
    by <- "rpath"
  }
  to_transfer <- df_from %>% dplyr::anti_join(df_to %>% dplyr::select(-path), by = by)
  to_remove <- df_to %>%
    dplyr::anti_join(df_from %>% dplyr::select(-path), by = "rpath") %>%
    dplyr::arrange(dplyr::desc(rpath))
  if (nrow(to_transfer) > 0) {
    path_to <- to_transfer$path %>% stringr::str_replace(from_start, to)
    path_from <- to_transfer$path
    rui::begin("Copying")
    if (type == "directory") {
      fs::dir_create(path_to)
    } else {
      fs::file_copy(path_from, path_to, overwrite = TRUE)
    }
    rui::succeed()
    rui::inform("Added {length(path_to)} {ifelse(length(path_to)==1,type,types)}")
  } else {
    rui::inform("No {type} to add")
  }
  if (nrow(to_remove) > 0) {
    path_to <- to_remove$path %>% stringr::str_replace(to_start, to)
    rui::begin("Deleting")
    # unlink instead of fs::file_delete() as the latter errors when files/dirs
    # are not there
    success <- unlink(to_remove$path, recursive = TRUE, force = TRUE, expand = FALSE)
    if (success == 0) {
      rui::succeed()
      rui::inform("Removed {length(path_to)} {ifelse(length(path_to)==1,type,types)}")
    } else {
      rui::fail()
      rui::alert("File deletion was not succesfull", warn = TRUE)
    }
  } else {
    rui::inform("No {type} to remove")
  }
}

#' Title
#'
#' @param from
#' @param to
#'
#' @return
#' @export
#'
#' @examples
sync <- function(from, to, skip) {
  sync_ll(from, to, skip, "directory")
  sync_ll(from, to, skip, "file")
}


file_diff <- function(x, y) {
  x <- fs::file_info(x)
  y <- fs::file_info(y)
  diffs <- which(x != y)
  rbind(x[,diffs], y[,diffs])

  # NOTE expectation may be diffr::diffr() -like output ... maybe useful too?
}
dir_diff <- function(x, y) {
  # TODO insert sync diff code? and use this there?
}

#' Fix backslashes in a windows file path
#'
#' <kbd>Shift</kbd> + Right-click in the Windows file Explorer allows you to copy a file
#' path, but it has backslashes. With this function, you get the forward
#' slash version, so it can be used inside your R code.
#'
#' @param path Path to process. If missing, the path is read from the clipboard.
#'
#' @return The fixed path, invisibly.
#'
#' @export
path_fix <- function(path) {
  clipboard <- FALSE
  if (missing(path)) {
    clipboard <- TRUE
    path <- readClipboard()
  }
  path <- path %>%
    gsub("\\\\", "/", .)
  if (clipboard) path %>% writeClipboard()
  rui::approve("Your path has forward slashes now:")
  rui::inform("{.path {path}}")
  invisible(path)
}
