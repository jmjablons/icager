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
#' digform(version = 1)
#'
#' @export
digform <- function(version = NULL){
  stopifnot(!is.null(version), !is.na(version), is.numeric(version))
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
#' @param .version Character or integer. Version of software:
#' #1  {"new", alteratively: "star", "newer", 1}
#' #2  {"old", alteratively: "plus", "older, "proper", 2}
#' @param .list.form Optional list modifying defaults of \code{digform()}.
#' For details call \code{?digform(2)}
#'
#' @return list of data frames
#'
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom utils modifyList
#' @importFrom dplyr mutate
#' @importFrom plyr rename
#' @importFrom dplyr %>%
#'
#' @export
import <- function(
  .dir = utils::choose.dir(), .version = NULL, .list.form = NULL) {
  tryCatch({
    id <- start <- end <- NULL
    if(!is.null(.version)){form <- digform(.version)}
    if(!is.null(.version) & !is.null(.list.form)){
      form <- utils::modifyList(form, .list.form)}
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
      if(is.null(.version)){
        form <- digform(checksoft(x))} ##dummy users
      if(!is.null(.list.form)){
        form <- utils::modifyList(form, .list.form)} ##ever dummier
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
    return(out)})}

#' Preprocess Experiment's Files ++
#'
#' \code{simple} is a wrapper simplifing
#' basics of the data import. Get through loading
#' a dataset, standardise and assign
#' experimental contingencies.
#'
#' @param a dataset or call of \code{import(path-to-the-data-directory)}
#' @param ... arguments to the \code{contingenise()}.
#' Each corner condition is coupled with user-specified reward access
#' probability, yet the the probabilities are not
#' directly named in data. Coding for them must be provided.
#' In case of problems please import the dataset with
#'  \code{import()} and then call \code{hint(data)} or \code{digcondition(data)}.
#'
#' @return tibble
#'
#' @export
simple <- function(a = import(), ...){
  dplyr::bind_rows(a) %>%
    dplyr::distinct() %>%
    standardise() %>%
    contingencise(...)}
