parsenosepoke <- function(
  tform, target = file.choose()){
  contacttimelick <- durationlick <- id <- n <- nlick <- NULL
  tryCatch(
    if(file.exists(target)){
      temp <- tform$namenose
      out <- importfile(.form = tform, target)
      names(out)[!is.na(match(names(out),names(temp)))] =
        temp[names(out)[!is.na(match(names(out),names(temp)))]] #barbarian rename
      out = out %>%
        dplyr::group_by(id) %>%
        dplyr::summarise(
          nlick = sum(as.numeric(nlick)),
          durationlick = sum(as.numeric(durationlick)),
          contacttimelick = sum(as.numeric(contacttimelick)),
          nnosepoke = dplyr::n()) %>%
        dplyr::ungroup()
    } else { #fake it
      warning(paste0("file not found: ",target))
      out <- data.frame(
        id = "dummy",
        nlick = NA,
        timelick = NA,
        nnosepoke = NA)})
  out}
