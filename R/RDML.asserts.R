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

is.to.remove.list <- function(x) {  
  for(el in x[-which(names(x) == "id")]) { 
    if(!is.na(el)) return(FALSE)        
  }
  TRUE
}
on_failure(is.to.remove.list) <- function(call, env) {
  paste0(deparse(call$x), " is not a command to remove from list")
}