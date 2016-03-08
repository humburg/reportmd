#' @export
`%||%` <- function(x, y){
  if(is.null(x)) y else x
}

#' @export
merge_list <- function(x,y){
  if(!is.null(y) && length(y)){
    x[names(y)] <- y
  }
  x
}
