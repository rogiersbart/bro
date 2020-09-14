.onAttach <- function(libname, pkgname) {
  rui::alert("{{br}} is still in its experimental lifecycle stage.")
  rui::alert("Use at your own risk, and submit issues here:")
  rui::alert("{.url https://github.com/rogiersbart/br/issues}")
  invisible()
}
