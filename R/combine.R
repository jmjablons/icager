combineenvironment <- function(main = tvisit, environment = tlight){
  environment = environment %>%
    dplyr::mutate(
      TimeMinute = as.character(
          trunc(as.POSIXct(DateTime),
            units="mins")))
  main = main %>%
    dplyr::mutate(
      TimeMinute = as.character(
          trunc(as.POSIXct(start),
            units="mins")))
  merge(main, dplyr::select(environment, -DateTime),
        all.x = TRUE) %>%
    dplyr::select(-TimeMinute)}

combinedoor <- function(main = tvisit, minor = tdoor){
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
