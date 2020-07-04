.onAttach <- function(libname, pkgname) {}

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.devtools <- list(
    devtools.path = "~/R-dev",
    devtools.install.args = "",
    devtools.name = "",
    devtools.desc.author = "Judyta Jablonska <jablon@if-pan.krakow.pl> [aut, cre]",
    devtools.desc.license = "licence",
    devtools.desc.suggests = NULL,
    devtools.desc = list())
  toset <- !(names(op.devtools) %in% names(op))
  if(any(toset)) options(op.devtools[toset])
  invisible()}

# .onUnload() <- function(){}
