# standard:
#   deviceid <chr>
#   start <dttm>
#   end <dttm>
#   corner <chr>
#   condition <chr>
#   tag <chr>
#   temperature <dbl>
#   illluminaton <int>
#   nlick <dbl>
#   durationlick <dbl>
#   nnosepoke <int>
#   dooropened <chr>

standardise <- function(A){
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
      corner = as.character(corner),
      condition = as.character(condition),
      tag = as.character(tag),
      dooropened = as.character(dooropened))}
