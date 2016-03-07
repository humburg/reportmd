#' Pretty printing of R objects for Markdown output
#' @param x Object to be printed
#' @param ... Additional arguments
#'
#' @return String representation of x formatted for printing.
#' @author Peter Humburg
#' @export
#' @rdname printMD
printMD <- function(x, ...) UseMethod('printMD')

#' @examples printMD(head(cars))
#' @export
#' @method printMD default
#' @rdname printMD
printMD.default <- function(x, ...) pander::pander(x, ...)

#' @param big.mark Separator used to mark intervals before the decimal point.
#' @examples printMD(10000)
#' @method printMD double
#' @export
#' @rdname printMD
printMD.double <- function(x, big.mark=',', ...){
  base::prettyNum(x, big.mark=big.mark, ...)
}

#' @examples printMD(10000L)
#' @method printMD integer
#' @export
#' @rdname printMD
printMD.integer <- function(x, big.mark=',', ...){
  base::prettyNum(x, big.mark=big.mark, ...)
}
