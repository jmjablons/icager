#' Import Any Tabelar File
#'
#' \code{importfile} imports a single file based on
#' standards and returns a data table
#'
#' @param .form List inherited from \code{digformat(version)}
#' @param target Character. Path to file; Default
#' calls widget to choose a file WIN by \code{file.choose()}
#' @return Data frame
importfile <- function(
  .form, target = file.choose()){
  decimalmark <- .form$decimalmark
    readr::read_delim(
      file = target,
      "\t",
      escape_double = FALSE,
      col_types = readr::cols(), #same as string_as_factor = F
      locale = readr::locale(decimal_mark = decimalmark),
      trim_ws = TRUE, progress = FALSE)}

#' Get The Door Openings Info
#'
#' \code{importfile} reads a single file word by word on,
#' selects words on the basis of standards,
#' parses and returns a data table
#'
#' Files containing all hardware info usually are clumsy,
#' so here the needed variables are get by position
#' rather than by name. Please note: A door opening is equal
#' to access to a reward.
#'
#' @param .form List inherited from \code{digformat(version)}
#' @param target Character. Path to file; Default
#' calls widget to choose a file WIN by \code{file.choose()}
#' @return Data frame with 3 variables:
#' \describe{
#'   \item{TimeDoorOpened}{exact time of door opening; <dttm>}
#'   \item{deviceid}{name of the house cage; <chr>}
#'   \item{Corner}{where the door got opened; <int>}
#' }
importdoor <- function(
  .form, target = file.choose()){
  tryCatch({
    if(file.exists(target)){
      tdoor = unlist(readLines(target))
      tdoor = unlist(strsplit(tdoor, split = "\\\t"))
      point = which(tdoor == "SetDoor")
      index = .form$doorIndex(ind = point, a = tdoor)
      deviceid = .form$doorCage(ind = index, a = tdoor)
      return(data.frame(
        TimeDoorOpened = as.POSIXct(tdoor[index - 2]),
        deviceid = deviceid,
        Corner = as.integer(tdoor[index + 2]),
        stringsAsFactors = F))
    } else { #fake it
      warning(paste("file not found:",target))
      return(
        data.frame(
          TimeDoorOpened = as.POSIXct(Sys.Date()),
          deviceid = NA,
          Corner = NA))}})}
