combineenvironment <- function(main, environment){
  DateTime <- deviceid <- TimeMinute <- start <- NULL
  environment = environment %>%
    dplyr::mutate(
      TimeMinute = as.character(
          trunc(as.POSIXct(DateTime),
            units="mins"))) %>%
    dplyr::arrange(DateTime) %>% ##hmm
    dplyr::select(-DateTime) %>%
    dplyr::group_by(deviceid, TimeMinute) %>%
    dplyr::slice(1) %>% ##fixed
    dplyr::ungroup()
  main = main %>%
    dplyr::mutate(
      TimeMinute = as.character(
          trunc(as.POSIXct(start),
            units="mins")))
  merge(main, environment, all.x = TRUE) %>%
    dplyr::select(-TimeMinute)}

combinedoor <- function(main, minor){
  TimeDoorOpened <- start <- id <- end <- Corner <- deviceid <- NULL
  minor = minor %>%
    dplyr::mutate(TimeDoorOpened = as.POSIXct(TimeDoorOpened)) %>%
    dplyr::as_tibble() %>%
    dplyr::arrange(TimeDoorOpened)
  tryCatch(
  return(main %>%
    dplyr::arrange(start) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(
      DoorOpened =
        ifelse(nrow(minor[which(
          (minor$TimeDoorOpened > start) &
            (minor$TimeDoorOpened < end) &
            (minor$Corner == Corner) &
            (minor$deviceid == deviceid)),]) > 0, 1, 0)) %>%
    dplyr::ungroup()))}
