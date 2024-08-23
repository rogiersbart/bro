#' Title
#'
#' @param id Google drive file id
#'
#' @return
#' @export
#'
#' @examples
drive_view <- function(id) {
  glue::glue("https://drive.google.com/uc?export=view&id={id}")
}

drive_folder <- function(id) {
  glue::glue("https://drive.google.com/drive/folders/{id}")
}

#' Title
#'
#' @param id Google drive file id
#'
#' @return
#' @export
#'
#' @examples
drive_download <- function(id) {
  glue::glue("https://drive.google.com/uc?export=download&id={id}")
}

#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
drive_favicon <- function(id) {
  glue::glue('<link href="{bro::drive_view(id)}" rel="icon" type="image/png">')
}

#' Title
#'
#' @param folder
#'
#' @return
#' @export
#'
#' @examples
drive_gallery <- function(folder) {
  drive_folder <- folder
  googledrive::drive_auth(TRUE)
  project_name <- drive_folder |> fs::path_file()
  df <- googledrive::drive_ls(
    paste0("~/", drive_folder, "/items/"),
    recursive = TRUE
  ) |>
    dplyr::select(name, id)
  df <- df |>
    dplyr::mutate(id = as.character(id)) |>
    tidyr::separate(name, into = c("code", "type", "extension"), sep = "_|\\.") |>
    dplyr::select(-extension) |>
    dplyr::mutate(type = type |> stringr::str_replace_all("-", "_")) |>
    dplyr::mutate(type = ifelse(is.na(type), "folder", type)) |>
    tidyr::spread("type", "id") |>
    dplyr::arrange(dplyr::desc(code))
  df <- df |> dplyr::select(code, image, folder)
  yml_file <- tempfile()
  googledrive::drive_download(paste0("~/", drive_folder, "/items.yml"), yml_file)
  yml_contents <- yaml::read_yaml(yml_file) |>
    unlist() |>
    tibble::enframe() |>
    dplyr::mutate(id = cumsum(name == "code")) |>
    tidyr::spread(name, value) |>
    dplyr::select(-id)
  df <- df |>
    dplyr::left_join(yml_contents) |>
    tidyr::replace_na(list(title = "", notes = "")) |>
    dplyr::arrange(dplyr::desc(code))
  df$notes <- glue::glue('{df$title}<br><br>{df$notes}<br><br><a href="{drive_folder(df$folder)}">Download page.</a>')
  cache_image <- function(id) {
    img <- googledrive::drive_get(id = id)
    target <- glue::glue("images/{img$name}")
    if (!fs::file_exists(target)) googledrive::drive_download(img, target)
    img$name
  }
  purrr::pwalk(
    df |>
      dplyr::select(image, title, notes),
    function(image, title, notes) {
      img_name <- cache_image(image)
      cat(
        glue::glue(
          '## {title} {{background-image="images/{img_name}" background-size=contain}}

::: {{.notes}}
{notes}
:::

\n\n\n'
        )
      )
    }
  )
}
