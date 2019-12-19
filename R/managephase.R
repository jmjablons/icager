#' Assign Contingency of Experimental Scheme
#'
#' \code{contingencise()} assigns a contingency mark
#' (number of experimntal stage) to every visit in dataset
#' on the basis on reward probabilities.
#'
#' @param A data frame
#' @param metadata named vector. Numeric vector of reward probability
#' assigned to corner-condition
#' \describe{
#' \item{values}{numeric values of reward probabilities}
#' \item{names()}{character vector of labels present in data}
#' }
#' Default: call \code{digform(version)$meta}
#' @return
#' data frame with added columns:
#' \describe{
#' \item{rp}{numeric. reward probability}
#' \item{contingency}{integer. number of experimental stage (=contingency)}
#' }
#' @examples
#'
#' dx <- contingencise(dx)
#'
#' @export
contingencise <-
  function(A, metadata = digform(1)$meta){
    message("Assigning stage contingency...")
    A = A %>%
      rawrename() %>%
      dplyr::mutate(
        condition = as.character(condition),
        rp = metadata[match(condition, names(metadata))]) %>%
      dplyr::arrange(start)
      A$contingency = NA
      assignment <- as.double(c(NA, NA, NA, NA))
      phase <- 0L
      for(i in 1:nrow(A)){
        corner <- as.integer(A$corner[i])
        rp <- as.double(A$rp[i])
        if(is.na(assignment[corner])){
          assignment[corner] <- rp
        } else {
          if(rp != assignment[corner]){
            phase <- as.integer(phase + 1)
            assignment[corner] <- rp
            assignment[-corner] <- as.double(c(NA, NA, NA))}}
        A$contingency[i] <- phase}
      message("done")
    return(A)}

#' Provide Experimental Scheme
#'
#' \code{printscheme()} returns a data frame of concurrent
#' experimental contingencies with variables describing them.
#'
#' An experimental task consists of subsequent changes
#' of reward probabilisty to corner assignment (contingency).
#' Here the total set of data-driven contingencies are stored
#' in a single data frame.
#'
#' @param A data frame
#' @param sorted logical. indicates whether a contingency
#' label should consist of sorted values
#' @param sep character. a separator used in a contingency label
#' @return
#' data frame
#' \describe{
#' \item{contingency}{number of contingency <int>}
#' \item{`1/2/3/4`}{corners reward proabilities <dbl>}
#' \item{start}{time of first visit made under the contingency <dttm>}
#' \item{end}{time od last visit made under the contingency <dttm>}
#' \item{duration}{difference in time between start and end <difftime>}
#' \item{label}{label of the contingency}
#' }
#' @examples
#'
#' printscheme(dx)
#'
#' @export
printscheme <-
  function(A, sorted = T, sep = 'x'){
    if(!any(grepl("contingency",names(A)))){
      stop("`contingency` not found.
           \nHave you assigned the contingency with `contingencise(data)`?")}
    phases <- A %>%
      dplyr::group_by(contingency, corner, rp) %>%
      dplyr::summarise(
        start = min(start),
        end = max(end)) %>%
      dplyr::group_by(contingency) %>%
      dplyr::mutate(
        start = min(start),
        end = max(end),
        duration = difftime(end, start, units = 'hours') %>%
          round(digits = 0)) %>%
      dplyr::ungroup() %>%
      tidyr::spread(corner,rp)
    phases$label <-
      apply(X = phases[5:8],1,
            function(x){
        label = character()
        rp = as.double(x)
        uni = unique(rp)
        if(sorted){label = paste(sort(uni), collapse = sep)
        } else {label = paste(uni,collapse = sep)}})
    phases}

#' Visualise Experimental Scheme
#'
#' \code{schematize()} shows colorful abstract representation of
#' experimental scheme
#'
#' @param A data frame. a dataset or experimental scheme (see: \code{printscheme()})
#'
#' @examples
#' dx <- schematize(contingencise(dx))
#'
#' @export
plotscheme <-
  function(A, ...){
    if(!any(grepl("contingency",names(A)))){
      stop("`contingency` not found.
           \nHave you assigned the contingency with `contingencise(data)`?")}
    if(!any(grepl("label",names(A)))){
      A = printscheme(A)}
    image(as.matrix(A[,5:8]),
          ylab = 'corner [1:4]',
          xlab = paste0(
            'phase ',
            '[ ',min(A$contingency, na.rm = T),
            ':',max(A$contingency, na.rm = T),' ]'),
          axes = F, col = topo.colors(4), ...)}

#' Assign Experimental Module
#'
#' \code{schematize()} assigns user-provided labels
#' to data, in relation on contingency.
#'
#' Various contingencies mat belong to different categories
#' of experimental scheme, e.x. module of adaptation consists
#' of contingencies labelled as "0x0.9" or/and "0x0", whereas
#' the reversals stage of "0x0.3x0.9" or/and "0.3x0.9".
#' Defaults are set internally (call \code{schematize(help=TRUE)}),
#' but if the \code{info} data frame is provided the current
#' values get assigned to the dataset.
#'
#' @param A data frame
#' @param info data frame. Character vector of labels
#' assigned to contingency (also known as module name to experimental phase)
#' \describe{
#' \item{continency}{character vector or integer. Label of contingency}
#' \item{info}{character vector. Label of module}
#' }
#' Please ensure the column order is correct!
#' Default internal values: call \code{schematize(help = TRUE)}
#'
#' @param help logical.
#' Set to TRUE to supress execution and get help
#'
#' @return
#' data frame with added column:
#' \describe{
#' \item{info}{character. Label of module}
#' }
#' @examples
#' dx <- contingencise(dx) %>% schematize()
#'
#' dx <- contingencise(dx) %>% schematize(
#' info = data.frame(cont = 1:10,
#' mod = c("preadapt", rep("test",10))))
#'
#' @export
schematize <- function(A, info = NA, help = F){
  message("To ensure call `printscheme(data)`")
  if(!help){
    if(!any(grepl("contingency",names(A)))){
      A = contingencise(A)}
    if(!any(grepl("info",names(A)))){
      scheme = printscheme(A)
      if(is.na(info)){
        temp <- c(
          welcome = "0",
          adaptation = "0x0.9",
          reversal = "0x0.3x0.9",
          finish = "0x1")
        scheme = scheme %>%
          dplyr::mutate(info = names(temp)[match(label, temp)])
      } else {
        stopifnot(!is.null(info))
        scheme = info
        names(scheme) = c("contingency", "info")
        scheme = scheme %>%
          mutate(
            contingency = as.integer(contingency),
            info = as.character(info))}
    } else {stop("`info` already present in data")}
    A %>% merge(dplyr::select(scheme, contingency, info)) %>%
      dplyr::as_tibble() } else {
        print(c(
          welcome = "0",
          adaptation = "0x0.9",
          reversal = "0x0.3x0.9",
          finish = "0x1"))}}
