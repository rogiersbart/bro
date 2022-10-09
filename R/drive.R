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
  if ("scan" %in% names(df)) df <- df |> dplyr::select(code, image, download = scan)
  if ("card" %in% names(df)) df <- df |> dplyr::select(code, image, download = card)
  if ("photo" %in% names(df)) df <- df |> dplyr::select(code, image, download = photo)
  yml_file <- tempfile()
  googledrive::drive_download(paste0("~/", drive_folder, "/items.yml"), yml_file)
  yml_contents <- yaml::read_yaml(yml_file) |>
    unlist() |>
    tibble::enframe() |>
    dplyr::mutate(id = cumsum(name == "code")) |>
    tidyr::spread(name, value) |>
    dplyr::select(-id)
  df2 <- df |>
    dplyr::left_join(yml_contents) |>
    tidyr::replace_na(list(title = "", description = "")) |>
    dplyr::arrange(dplyr::desc(code))
  df2$description <- ifelse(
    !is.na(df2$download),
    glue::glue('{df2$title}<br><br>{df2$description}<br><br><a href="{drive_download(df2$download)}">Download.</a>'),
    glue::glue('{df2$title}<br><br>{df2$description}')
  )
  purrr::pwalk(
    df2 |> dplyr::slice(which.max(as.numeric(code))) |> dplyr::select(image, title, description),
    function(image, title, description) {
      cat(
        glue::glue(
          '## {title} {{background-image="{drive_download(image)}" background-size=cover background-opacity=0.5}}

{description}

::: aside
This is a
[Quarto Revealjs presentation](https://quarto.org/docs/presentations/revealjs/)
with auto-play. Press \"a\" to start/pause the slideshow and \"f\" for
fullscreen mode. You can find my notes and the download links in the speaker\'s
view by pressing \"s\". Use \"?\" for an overview of keyboard shortcuts.
:::

\n\n\n'
        )
      )
    }
  )
  purrr::pwalk(
    df2 |>
      dplyr::slice(-which.max(as.numeric(code))) |>
      dplyr::select(image, title, description),
    function(image, title, description) {
      cat(
        glue::glue(
          '## {title} {{background-image="{drive_view(image)}" background-size=contain}}

::: {{.notes}}
{description}
:::

\n\n\n'
        )
      )
    }
  )
}
