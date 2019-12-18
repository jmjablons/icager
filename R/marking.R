#' What Did I Set While Recording
#'
#' \code{hint()} prints a list with basic information
#' about the dataset formatting
#'
#' @param A data frame
#' @return list
#' \describe{
#' \item{conditiondevice}{number of visits performed depending
#' on condition label and device used}
#' \item{cornercondition}{number of visits performed split
#' by corner-condition assignment}
#' \item{corneropen}{number of visits while door got opened (
#' access granted)}
#' \item{hint}{Fraction on real access being granted under
#' all the condition labels}
#' \item{outlier}{number of visits split by participants and
#' condition label; it's helping to spot outlier}
#' }
#' @examples
#' dx <- hint(dx)
#' @export
hint <- function(A){
  A = rawrename(A)
  with(A, {
    list(
    conditiondevice = table(condition, deviceid),
    cornercondition = table(corner, condition),
    corneropen = table(corner, dooropened),
    hint = table(condition, dooropened) %>%
      as.data.frame(row.names = NULL) %>%
      tidyr::spread(dooropened, Freq) %>%
      dplyr::mutate(fractionopen = `1`/ (`0`+`1`)),
    outlier = table(tag, condition))})}

#' Is That The Dataset I'm Looking For?
#'
#' \code{specify()} provides basic information
#' about the dataset
#'
#' @param A data frame
#' @return list
#' \describe{
#' \item{time}{when the experiment were performed}
#' \item{nanimal}{number of participants}
#' \item{animal}{identifiers of participants}
#' \item{device}{list house cages used}
#' \item{condition}{list condition labels}
#' \item{temperature}{range of temperatures noted}
#' \item{illumination}{range of illumination noted}
#' \item{ifopened}{marks for door-openings (=reward access);
#' checks if the dataset is proper}
#' }
#' @examples
#' dx <- specify(dx)
#' @export
specify <- function(A){
  A = rawrename(A)
  with(A, {
    list(
      time = range(start),
      nanimal = length(unique(tag)),
      animal = unique(tag),
      device = unique(deviceid),
      condition = unique(condition),
      temperature = range(as.numeric(temperature), na.rm = T),
      illumination = range(sort(as.numeric(illumination)), na.rm = T),
      ifopened = unique(dooropened, na.rm= F))})}
