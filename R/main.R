#' Provide Formatting Standards On Call
#'
#' \code{digform()} returns a list of variables specifying
#' experimental files formatting,
#' which varies dependenting on software version
#'
#' It's called also inside other package's functions.
#'
#' @param version Character or integer. Version of software:
#' \describe{
#' \item{#1}{"new", "star", "newer", 1}
#' \item{#2}{"old", "plus", "older, "proper", 2}
#' }
#'
#' @return
#' a named list
#'
#' @examples
#' digform(1)
#'
#' @export
digform <- function(version){
  if(version %in% c("star", "new", "newer", "shitty", 1)){
    out <- list(
      key = 1,
      decimalmark = ".",
      visit = "/Visit.txt",
      nosepoke = "/Nosepoke.txt",
      output = "/Output.txt",
      light = "/Environment.txt",
      namenose = c(
        "VisitId" = "id",
        "LicksNumber" = "nlick",
        "LicksDuration" = "durationlick",
        "DeviceId" = "deviceid",
        "LicksContactTime" = "contacttimelick"),
      namevisit = c(
        "VisitId" = "id",
        "DeviceId" = "deviceid",
        "StartDateTime" = "start",
        "EndDateTime" = "end",
        "Tag" = "tag"),
      nameenvironment = c(
        "DeviceId" = "deviceid"),
      doorIndex = function(ind, a){
        ind[(a[ind + 4] %in% 0) &
              (a[ind + 3] == 'Right')]},
      doorCage = function(ind, a){
        paste0("Cage ", a[ind - 1])})}
  if(version %in% c("plus", "old", "older", "proper", 2)){
    out <- list(
      visit = "/IntelliCage/Visits.txt",
      nosepoke = "/IntelliCage/Nosepokes.txt",
      output = "/IntelliCage/Output.txt",
      light = "/IntelliCage/Environment.txt",
      namenose = c(
        "VisitID" = "id",
        "LickNumber" = "nlick",
        "LickDuration" = "durationlick",
        "LickContactTime" = "contacttimelick"),
      namevisit = c(
        "Start" = "start",
        "End" = "end",
        "Cage" = "deviceid",
        "VisitID" = "id",
        "AnimalTag" = "tag"),
      nameenvironment = c(
        "Cage" = "deviceid"),
      key = 2,
      decimalmark = ",",
      doorIndex = function(ind, a){
        ind[(a[ind + 2] %in% c(1:4)) &
              (a[ind + 3] == 0)]},
      doorCage = function(ind, a){
        as.character(a[ind - 1])})}
    out$meta = c(
          "0" = 0,
          "1" = .9,
          "-1" = .3,
          "White" = 0,
          "Red" = .9,
          "Purple" = .9,
          "Green" = .3,
          "Blue" = 1)
    return(out)}

#' Preprocess Experiment's Files
#'
#' \code{import} imports all experimentals files
#' stored in given directories, parses on the basis on
#' prespecified standards and returns a list
#'
#' Warning: The output object (list of data frames)
#' can be large in size. It depends entirely on number
#' of files provided. Loading one experiment dataset
#' per time is recomended.
#'
#' @param .dir Character vector. Paths to one single directory with files
#' @param version Character or integer. Version of software:
#' #1  {"new", alteratively: "star", "newer", 1}
#' #2  {"old", alteratively: "plus", "older, "proper", 2}
#' @param .list.form Optional list modifying defaults of \code{digform()}.
#' For details call \code{?digform(2)}
#'
#' @return list [data frame]
#'
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom utils modifyList
#'
#' @export
import <- function(
  .dir = utils::choose.dir(), version = NULL, .list.form = NULL) {
  tryCatch({
    id <- start <- end <- NULL
    if(!is.null(version)){form <- digform(version)}
    if(!is.null(.list.form)){utils::modifyList(form, .list.form)}
    mydirs <- list.files(path = .dir,
      recursive = F, full.names = T)
    nfiles = length(mydirs)
    stopifnot(nfiles > 1)
    out <- list(rep(NA, nfiles))
    pb <- txtProgressBar(
      min = 0, max = nfiles, char = "*", style = 3)
    for(i in seq_along(mydirs)){
      setTxtProgressBar(pb, i)
      x = mydirs[i]
      if(is.null(version)){form <- digform(checksoft(x))} ##dummy users
      if(!is.null(.list.form)){modifyList(form, .list.form)} ##ever dummier
      pathfile = paste0(x, form$visit)
      if(!file.exists(pathfile)){
        warning(paste("file not found:", pathfile))}
      if(file.exists(pathfile)){
        tvisit = importfile(form, paste0(x, form$visit)) %>%
          plyr::rename(form$namevisit) %>%
          dplyr::mutate(id = as.character(id))
        tnosepoke = parsenosepoke(tform = form, paste0(x, form$nosepoke))
        tdoor = importdoor(form, paste0(x, form$output))
        main = tvisit
        if(file.exists(paste0(x, form$light))){
        tlight = importfile(form, paste0(x, form$light)) %>%
          plyr::rename(form$nameenvironment)
        main = combineenvironment(main, tlight)}
        main = merge(x = main, y = tnosepoke,
                     all.x = TRUE, message = F)
        main = dplyr::mutate(main,
                      start = as.POSIXct(start),
                      end = as.POSIXct(end))
        main = combinedoor(main, tdoor)
        names(main) <- tolower(names(main))
        out[[i]] <- main
        out = emptyout(out)}}
    close(pb)
    return(out)},
    finally = message('done'))}

#' Preprocess Experiment's Files ++
#'
#' \code{simple} serves the purpose of making the
#' data preprocessing as automatic as possible.
#' The function is a wrapper simplifing
#' basics of the data import.
#'
#' Warning: The input must be able to get
#' passed to dplyr::bind_rows meaning ex
#' names of each tibble in list must match.
#'
#' @param a result of \code{import()} or simply eval of it
#' @param ... arguments to the \code{contingenise()}
#'
#' @return tibble
#'
#' @export
simple <- function(a = import(), ...){
  dplyr::bind_rows(a) %>%
    dplyr::distinct() %>%
    standardise() %>%
    contingencise(...)}
