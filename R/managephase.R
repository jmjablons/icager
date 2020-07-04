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
    condition <- start <- NULL
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
#' printscheme(contingencise(dx))
#'
#' @export
printscheme <-
  function(A, sorted = T, sep = 'x'){
    if(!any(grepl("contingency",names(A)))){
      stop("`contingency` not found.
           \nHave you assigned the contingency with `contingencise(data)`?")}
    contingency <- corner <- rp <- start <- end <- NULL
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
#' \code{plotscheme()} shows colorful abstract representation of
#' experimental scheme
#'
#' @param A data frame. a dataset or experimental scheme (see: \code{printscheme()})
#' @param ... arguments passed to image()
#'
#' @importFrom graphics image
#' @importFrom grDevices topo.colors
#'
#' @examples
#' dx <- plotscheme(contingencise(dx))
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
