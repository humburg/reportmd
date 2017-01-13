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
#' @param digits Number of sinificant digits to display, passed to \code{format}
#' @examples printMD(10000)
#'           printMD(10^6)
#' @method printMD double
#' @export
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom stringr fixed
#' @rdname printMD
printMD.double <- function(x, big.mark=',', digits=pander::panderOptions('digits'), ...){
  pretty <- base::prettyNum(x, big.mark=big.mark, digits=digits, ...)
  if(stringr::str_detect(pretty, stringr::fixed('e'))){
    pretty <- fix_exponent(pretty)
  }
  pretty
}

#' @examples printMD(10000L)
#' @method printMD integer
#' @export
#' @rdname printMD
printMD.integer <- function(x, big.mark=',', ...){
  pretty <- base::prettyNum(x, big.mark=big.mark, ...)
  if(stringr::str_detect(pretty, stringr::fixed('e'))){
    pretty <- fix_exponent(pretty)
  }
  pretty
}

#' @param format Format type to use.
#' @method printMD Dependency
#' @export
#' @rdname printMD
printMD.Dependency <- function(x, format=c('markdown', 'html', 'reference', 'md reference'), ...){
  format <- tolower(format)
  format <- match.arg(format)
  rel_path <- basename(x$document)
  if(format == 'markdown'){
    link <- paste0('[', x$title, '](', rel_path, ')')
  } else if(format == 'html'){
    link <- paste0('<a href=', rel_path, '>', x$title, '</a>')
  } else if(format == 'reference'){
    link <- paste0('[', x$label, ']: ', rel_path)
    if(x$title != 'Untitled'){
      link <- paste0(link, ' (', x$title, ')')
    }
  } else if(format == 'md reference'){
    link <- paste0('[', x$title, '][', x$label, ']')
  }
  link
}
