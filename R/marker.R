

ls_todo <- function(path = here::here("R")) {
  todor::todor(
    todo_types = "TODO",
    search_path = path
  )
}

ls_fixme <- function(path = here::here("R")) {
  todor::todor(
    todo_types = "FIXME",
    search_path = path
  )
}

ls_idea <- function(path = here::here("R")) {
  todor::todor(
    todo_types = "IDEA",
    search_path = path
  )
}

ls_note <- function(path = here::here("R")) {
  todor::todor(
    todo_types = "NOTE",
    search_path = path
  )
}

ls_all <- function(path = here::here("R")) {
  todor::todor(
    todo_types = c("TODO", "FIXME", "IDEA", "NOTE"),
    search_path = path
  )
}
