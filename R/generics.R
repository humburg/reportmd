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
  need_fix <- stringr::str_detect(pretty, stringr::fixed('e'))
  if(any(need_fix)){
    pretty[need_fix] <- fix_exponent(pretty[need_fix])
  }
  pretty
}

#' @examples printMD(10000L)
#' @method printMD integer
#' @export
#' @rdname printMD
printMD.integer <- function(x, big.mark=',', ...){
  pretty <- base::prettyNum(x, big.mark=big.mark, ...)
  need_fix <- stringr::str_detect(pretty, stringr::fixed('e'))
  if(any(need_fix)){
    pretty[need_fix] <- fix_exponent(pretty[need_fix])
  }
  pretty
}

#' @param format Format type to use.
#' @param target Specify an anchor in the output document for \code{x} that the
#' link should point to.
#' @param text Text that should be displayed as part of the link.
#' @method printMD Dependency
#' @export
#' @rdname printMD
printMD.Dependency <- function(x, target, format=c('markdown', 'html', 'reference', 'md reference'),
                               text=x$title, ...){
  format <- tolower(format)
  format <- match.arg(format)
  rel_path <- basename(x$document)
  if(!missing(target)){
    rel_path <- paste(rel_path, target, sep='#')
  }
  if(format == 'markdown'){
    link <- paste0('[', text, '](', rel_path, ')')
  } else if(format == 'html'){
    link <- paste0('<a href=', rel_path, '>', text, '</a>')
  } else if(format == 'reference'){
    link <- paste0('[', x$label, ']: ', rel_path)
    if(!missing(text) || text != 'Untitled'){
      link <- paste0(link, ' (', text, ')')
    }
  } else if(format == 'md reference'){
    link <- paste0('[', text, '][', x$label, ']')
  }
  link
}
