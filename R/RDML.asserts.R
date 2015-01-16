
#' @import assertthat
is.opt.string <- function(x) {
  if(is.null(x)) return(TRUE)
  is.string(x)
}
on_failure(is.opt.string) <- function(call, env) {
  paste0(deparse(call$x), " is present but not a string ")
}

has.only.names <- function(x, only.names) {
  !(FALSE %in% (names(x) %in% only.names))
}
on_failure(has.only.names) <- function(call, env) {
  paste0(deparse(call$x), " has names not in: ", paste(call$only.names, collapse = ", "))
}

is.id <- function(x) {
  is.count(x) || is.string(x)
}
on_failure(is.id) <- function(call, env) {
  paste0(deparse(call$x), " is not an id (a string or a count)")
}