parsenosepoke <- function(
  version, target = file.choose()){
  tryCatch(
    if(file.exists(target)){
      form <- digform(version)$namenose
      out <- importfile(version, target)
      names(out)[!is.na(match(names(out),names(form)))] =
        form[names(out)[!is.na(match(names(out),names(form)))]] #barbarian rename
      out = out %>%
        dplyr::group_by(id) %>%
        dplyr::summarise(
          nlick = sum(as.numeric(nlick)),
          durationlick = sum(as.numeric(durationlick)),
          contacttimelick = sum(as.numeric(contacttimelick)),
          nnosepoke = n()) %>%
        dplyr::ungroup()
    } else { #fake it
      warning(paste0("file not found: ",target))
      out <- data.frame(
        id = "dummy",
        nlick = NA,
        timelick = NA,
        nnosepoke = NA)})
  out}
