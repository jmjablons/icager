% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readfile.R
\name{importfile}
\alias{importfile}
\title{Import Any Tabelar File}
\usage{
importfile(.form, target = file.choose())
}
\arguments{
\item{.form}{List inherited from \code{digformat(version)}}

\item{target}{Character. Path to file; Default
calls widget to choose a file WIN by \code{file.choose()}}
}
\value{
Data frame
}
\description{
\code{importfile} imports a single file based on
standards and returns a data table
}
