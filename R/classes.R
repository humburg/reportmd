#' Create objects of class \code{Dependency}
#'
#' Objects of the class are used to store information
#' about child docxuments.
#'
#' @param label The label associated with the document.
#' @param document Path to the output document. This should be
#' relative to the location of the main document.
#' @param source Path to source (i.e. RMarkdown) file used to generate \code{document}.
#' @param title The title of the document.
#' @param cache Path to cache directory. If this is missing an attempt will be made to guess
#' the location if \code{source} is present.
#' @param ... Additional arguments are ignored.
#'
#' @details The \code{source} and \code{title} arguments may be omitted. If
#' no \code{title} is provided an attempt is made to extract it from the header
#' of the source file or, if that is unavailable (or no title was found),
#' the output document (but currently only for html output).
#' @return An object of class \code{Dependency}. Such objects have fields
#'  \item{label}{The label used to refer to this document.}
#'  \item{document}{The path to the output document.}
#'  \item{source}{The source document used to generate the output (may be \code{NULL}).}
#'  \item{title}{The document title. If no title is available this will be 'Untitled'}
#'  \item{cache}{Location of the cache directory.}
#' @export
#' @importFrom stringr str_extract
Dependency <- function(label, document, source, title, cache, ...){
  dep <- list(label=label, document=document)
  doc_format <- tolower(stringr::str_extract(document, '[^.]+$'))
  if(!missing(source)){
    dep$source <- source
  }

  if(!missing(title)){
    dep$title <- title
  } else {
    extracted_title <- ''
    if(!is.null(dep$source)){
      extracted_title <- rmd_title(dep$source)
    } else if(doc_format == 'html'){
      extracted_title <- html_title(dep$document)
    }
    if(is.null(extracted_title) || extracted_title == ''){
      extracted_title <- 'Untitled'
    }
    dep$title <- extracted_title
  }

  if(missing(cache)){
    if(!is.null(dep$source)){
      prefix <- sub("\\.[^.]+$", "", dep$source)
      cache <- file.path(paste(prefix, 'cache', sep="_"), opts_knit$get("rmarkdown.pandoc.to"))
    } else{
      cache <- NULL
    }
  }
  dep$cache <- cache

  class(dep) <- 'Dependency'
  dep
}
