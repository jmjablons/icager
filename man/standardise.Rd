% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standardise.R
\name{standardise}
\alias{standardise}
\title{Standardise a Dataset}
\usage{
standardise(A)
}
\arguments{
\item{A}{data frame}
}
\value{
data frame with variables:
\describe{
\item{deviceid:chr}{}
\item{start:dttm}{}
\item{end:dttm}{}
\item{corner:chr}{}
\item{condition:chr}{}
\item{tag:chr}{}
\item{temperature:dbl}{}
\item{illluminaton:int}{}
\item{nlick:dbl}{}
\item{durationlick:dbl}{}
\item{nnosepoke:int}{}
\item{dooropened:chr}{}
}
}
\description{
\code{standardise()} selects a dataset variables and
changes their type to provide a minimal size comprehensible
dataset
}
\examples{
mydata <- standardise(dx)

}
