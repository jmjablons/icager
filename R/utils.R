# Defaults for NULL values
# `%||%` <- function(a, b) if (is.null(a)) b else a

#' @importFrom magrittr %>%

#change names
rawrename <- function(A, pattern = "cornercondition", replacement = "condition"){
  names(A) = tolower(names(A))
  names(A)[grep(x = names(A), pattern = pattern)] <- replacement
  A}

#' @importFrom purrr compact
#omit empty elements of a list
emptyout <- function(list){purrr::compact(list) %>%
    purrr::keep(~ nrow(.) > 0)}

checksoft <- function(.mydir){
  ifelse(any(grepl("ActivityBox", list.files(.mydir))), 2, 1)}
