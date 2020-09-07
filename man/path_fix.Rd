% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/path.R
\name{path_fix}
\alias{path_fix}
\title{Fix backslashes in a windows file path on your clipboard}
\usage{
path_fix()
}
\description{
Shift + Right-click in the Windows file Explorer allows you to copy a file
path, but it has backslashes. With this function, you get the forward
slash version in your clipboard, so it can be used inside your R code.
}