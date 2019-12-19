#small pieces
# not for export purpouses

# Defaults for NULL values
# `%||%` <- function(a, b) if (is.null(a)) b else a

# Remove NULLs from a list
# compact <- function(x) {
#   x[!vapply(x, is.null, logical(1))]
# }

#' @importFrom magrittr %>%

# is.homogenous <-
#   function(x, na.rm = T){
#     stopifnot(length(x) > 1, is.numeric(x))
#     if(na.rm) x = x[!is.na(x)]
#       (!any(x != x[1]))}

#homogenous vector
# shorten <-
#   function(x, na.rm = T){
#     if(na.rm){x = na.omit(x)}
#     if(all(x[1] == x)){
#       return(x[1])
#     } else {stop('vector not homogenous')}}

#change names
rawrename <- function(A, pattern = "cornercondition", replacement = "condition"){
  names(A) = tolower(names(A))
  names(A)[grep(x = names(A), pattern = pattern)] <- replacement
  A}

#omit empty elements of a list
emptyout <- function(list){
  list[lapply(list,length)>0]
}
