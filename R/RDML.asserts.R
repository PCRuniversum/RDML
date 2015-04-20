# library(assertthat)
is.opt.string <- function(x) {
  if(is.null(x) || is.na(x)) return(TRUE)
  is.string(x)
}
on_failure(is.opt.string) <- function(call, env) {
   paste0(deparse(call$x), " is present but not a string")
}

is.opt.list <- function(x) {
  if(is.null(x)) return(TRUE)
  is.list(x)
}
on_failure(is.opt.list) <- function(call, env) {
  paste0(deparse(call$x), " is present but not a list")
}

is.opt.list.one.el <- function(x) {
  if(is.null(x)) return(TRUE)
  is.list(x) && length(x) == 1  
}
on_failure(is.opt.list.one.el) <- function(call, env) {
  paste0(deparse(call$x), " is present but not a list or length > 1")
}

is.opt.logical <- function(x) {
  if(is.null(x) || is.na(x)) return(TRUE)
  is.logical(x) && length(x) == 1  
}
on_failure(is.opt.logical) <- function(call, env) {
  paste0(deparse(call$x), " is present but not a logical or length > 1")
}

is.opt.double <- function(x) {
  if(is.null(x) || is.na(x)) return(TRUE)
  is.double(x) && length(x) == 1  
}
on_failure(is.opt.double) <- function(call, env) {
  paste0(deparse(call$x), " is present but not a double or length > 1")
}

is.opt.double.matrix <- function(x) {
  if(is.null(x) || is.na(x)) return(TRUE)
  is.double(x) && is.matrix(x) 
}
on_failure(is.opt.double.matrix) <- function(call, env) {
  paste0(deparse(call$x), " is present but not a double matrix")
}

is.opt.integer <- function(x) {
  if(is.null(x) || is.na(x)) return(TRUE)
  is.integer(x) && length(x) == 1  
}
on_failure(is.opt.integer) <- function(call, env) {
  paste0(deparse(call$x), " is present but not a integer or length > 1")
}

is.opt.count <- function(x) {
  if(is.null(x) || is.na(x)) return(TRUE)
  is.count(x) && length(x) == 1  
}
on_failure(is.opt.integer) <- function(call, env) {
  paste0(deparse(call$x), " is present but not a count or length > 1")
}

has.only.names <- function(x, only.names) {
  !(FALSE %in% (names(x) %in% only.names))
}
on_failure(has.only.names) <- function(call, env) {
  paste0(deparse(call$x), " has names not in: ", paste(call$only.names, collapse = ", "))
}

is.to.remove.list <- function(x) {  
  for(el in x[-which(names(x) == "id")]) { 
    if(!is.na(el)) return(FALSE)        
  }
  TRUE
}
on_failure(is.to.remove.list) <- function(call, env) {
  paste0(deparse(call$x), " is not a command to remove from list")
}