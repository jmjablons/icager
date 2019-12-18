# prepare instance data
# {temp <- unique(dxstar$tag)
# dx <- dxstar %>%
#   filter(tag %in% sample(temp, 3)) %>%
#   contingencise() %>%
#   filter(contingency %in% 0:10) %>%
#   group_by(tag, contingency) %>%
#   arrange(start, .by_group = T) %>%
#   dplyr::slice(1:10, .preserve = TRUE) %>%
#   ungroup() %>%
#   select(-rp, -contingency)}

#' Sample experimental data
#'
#' A dataset of randomly selected choices
#' made by sample agents performing
#' probabilistic reversal learning task.
#' Includes actions /visits/ performed by 3 random agents,
#' first 10 per each of 11 experimental stages.
#'
#' @format A tibble with 330 rows and 16 variables:
#' \describe{
#'   \item{id}{unique visit id, not really handy; <chr>}
#'   \item{deviceid}{house cage; <chr>}
#'   \item{start}{time of visit start; <dttm>}
#'   \item{end}{time of visit end; <dttm>}
#'   \item{corner}{which corner given visit took plase; <int>}
#'   \item{condition}{indicate reward probability settup; <chr>}
#'   \item{solution}{settup issue, not handy; <chr>}
#'   \item{paradigm}{name of paradigm set while recording; <chr>}
#'   \item{tag}{participant unique identificator; <dbl>}
#'   \item{temperature}{temperature noted; <dbl>}
#'   \item{illumination}{light detected level; <int>}
#'   \item{nlick}{number of licks during given visit; <dbl>}
#'   \item{durationlick}{total time spent licking; <dbl>}
#'   \item{contacttimelick}{similar to total licking time detected; <dbl>}
#'   \item{nnosepoke}{total number of nosepokes per given visit; <int>}
#'   \item{dooropened}{binary indicator of access to reward being granted 1 - yes, 0 -no; <dbl>}
#' }
#'
#' @keywords datasets
#'
#' @usage
#' dx
#'
#' @examples
#' class(dx) #similar to data frame
#' dim(dx) #nrow, ncol
#' head(dx) #couple of first rows
#'
#' @source experiment at Mai IF PAN
"dx"
