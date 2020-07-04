#' Standardise a Dataset
#'
#' \code{standardise()} selects a dataset variables and
#' changes their type to provide a minimal size comprehensible
#' dataset
#'
#' @param A data frame
#'
#' @return
#' data frame with variables:
#' \describe{
#' \item{deviceid:chr}{}
#' \item{start:dttm}{}
#' \item{end:dttm}{}
#' \item{corner:chr}{}
#' \item{condition:chr}{}
#' \item{tag:chr}{}
#' \item{temperature:dbl}{}
#' \item{illluminaton:int}{}
#' \item{nlick:dbl}{}
#' \item{durationlick:dbl}{}
#' \item{nnosepoke:int}{}
#' \item{dooropened:chr}{}
#' }
#'
#' @examples
#' mydata <- standardise(dx)
#'
#' @export
standardise <- function(A){
  deviceid<-
  start<-
  end<-
  corner<-
  condition<-
  tag<-
  temperature<-
  illumination<-
  nlick<-
  durationlick<-
  nnosepoke<-
  dooropened <- NULL
A = rawrename(A)
  A %>%
    dplyr::select(
      deviceid,
      start,
      end,
      corner,
      condition,
      tag,
      temperature,
      illumination,
      nlick,
      durationlick,
      nnosepoke,
      dooropened) %>%
    dplyr::mutate(
      deviceid = as.character(deviceid),
      deviceid = ifelse(
        !grepl(x = deviceid, "Cage "),
        paste0("Cage ",deviceid),
        deviceid),
      nlick = as.numeric(nlick),
      nnosepoke = as.numeric(nnosepoke),
      corner = as.character(corner),
      condition = as.character(condition),
      tag = as.character(tag),
      illumination = as.numeric(illumination),
      temperature = as.numeric(temperature),
      dooropened = as.integer(dooropened))}
