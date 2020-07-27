#' Provide default coding for corner condition
#'
#' Default values passed to \code{contingencise()}
#' @return
#' a named atomic vector
#' \describe{
#' \item{values}{numeric values of reward probabilities}
#' \item{names()}{character vector of labels present in data}
#' }
#'
#' @export
digcondition <- function(){
  c("0" = 0,
    "1" = .9,
    "-1" = .3,
    "White" = 0,
    "Red" = .9,
    "Purple" = .9,
    "Green" = .3,
    "Blue" = 1)}

#' Assign Contingency of Experimental Scheme
#'
#' \code{contingencise()} assigns a contingency mark
#' (number of experimntal stage) to every visit in dataset
#' on the basis on reward probabilities.
#'
#' @param a data frame
#' @param metadata named vector
#' \describe{
#' \item{values}{numeric values of reward probabilities}
#' \item{names()}{character vector of labels present in data}
#' }
#' Default: call \code{digcondition}
#' @return
#' dataset with added columns:
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
  function(a, metadata = digcondition()){
    start <- end <- label <- corner <- rp <- contingency <- duration <- NULL
    if(is.null(metadata)){
      stop(paste0(c("Coding of corner conditions not provided.
                    Please use the levels found in dataset:\n",
                    unique(a$condition)), collapse = "\t"))}
    message("Assigning contingencies...")
    condition <- start <- NULL
    a = rawrename(A = a) %>%
      dplyr::mutate(
        condition = as.character(condition),
        rp = metadata[match(condition, names(metadata))]) %>%
      dplyr::arrange(start)
      a$contingency = NA
      assignment <- as.double(c(NA, NA, NA, NA))
      phase <- 0L
      for(i in 1:nrow(a)){
        corner <- as.integer(a$corner[i])
        rp <- as.double(a$rp[i])
        if(is.na(assignment[corner])){
          assignment[corner] <- rp
        } else {
          if(rp != assignment[corner]){
            phase <- as.integer(phase + 1)
            assignment[corner] <- rp
            assignment[-corner] <- as.double(c(NA, NA, NA))}}
        a$contingency[i] <- phase}
    return(a)}

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
#'
#' @export
plotscheme <-
  function(A, ...){
    .Deprecated("plotit")
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

#' Visualise Experimental Scheme
#'
#' \code{plotit()} presents the actual
#' experimental scheme with contingencies
#' durations in a default form
#'
#' @param A a dataset
#'
#' @examples
#' plotit(contingencise(dx))
#'
#' @export
plotit <- function(A){
  start <- end <- label <- corner <- rp <- contingency <- duration <- NULL
  temp <- icager::printscheme(A) %>%
    dplyr::select(-start, -end, -label) %>%
    tidyr::gather(corner, rp, -contingency, -duration) %>%
    dplyr::mutate(duration = ifelse(corner %in% 1, duration, NA),
                  rp = as.factor(rp),
                  contingency = as.integer(contingency),
                  corner = as.integer(corner))
    ggplot2::ggplot(temp, ggplot2::aes(x = contingency,
                                       y = corner, colour = rp)) +
    ggplot2::geom_point(size = 5, pch = 15) +
    ggplot2::geom_label(ggplot2::aes(label = duration),
                        color = "black", nudge_y = -1.5)+
    ggplot2::scale_y_continuous(breaks = 1:4, expand = c(.1, 0))+
    ggplot2::scale_x_continuous(position = "top",
                                breaks = unique(temp$contingency),
                                expand = c(0,.5))+
    ggplot2::theme_void()+
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(),
      axis.text.y = ggplot2::element_text())+
    ggplot2::scale_color_grey()}
