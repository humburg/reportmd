#' Register a new download
#'
#' @param x An object that should be made available for download. If this is
#' a character vector of length one it is interpreted as the name of the object.
#' @param label A short, descriptive label
#' @param writer A function used to create the downloadable file from \code{x}.
#' This function needs to take at least two arguments, the first of which should
#' be the R object that is to be written to a file and the second of which should
#' be the file name (without path).
#' @param description A longer description.
#' @param ext Extension to use for the file.
#' @param create Logical indicating wether the downloadable file should be created
#' immediately.
#' @param ... Additional arguments are passed to \code{writer}.
#'
#' @details If \code{create} is \code{FALSE} the downloadable file can be created
#' later by a call to \code{create_download}. This allows objects to be registered
#' for download when they are created while ensuring that they are not written to
#' a file until any possible changes to their data have occured.
#' @return Called for its side effect.
#' @author Peter Humburg
#' @export
#' @importFrom knitr opts_knit
#' @importFrom stringr str_replace
#' @importFrom utils write.csv
add_download <- function(x, label, writer=write.csv, description="", ext='csv',
                         create=TRUE, ...){
  if(is.character(x) && length(x) == 1){
    name <- x
    x <- get(x)
  } else{
    name <- deparse(substitute(x))
  }
  file_name <- name
  file_name <- stringr::str_replace(file_name, '\\s', '_')
  file_name <- paste(file_name, ext, sep='.')

  downloads <- knitr::opts_knit$get('.downloads')
  exists <- FALSE
  if(name %in% names(downloads)){
    exists <- TRUE
  }
  dwnld <- Download(writer=writer, file_name=file_name,
                    label=label, description=description, ...)
  if(!exists && dwnld$target %in% sapply(downloads, '[[', 'target')){
    stop('Download with file name ', file_name, ' already exists.')
  }
  if(create && !exists){
    assign(name, x)
    create_download(name, dwnld)
  }

  downloads[[name]] <- dwnld
}

#' Create a file for download
#'
#' @param x R object to be saved for download
#' @param download Object of class \code{Download}. This may be omitted if
#' \code{x} was registered for download earlier.
#'
#' @return Called for its side effect.
#' @export
#' @author Peter Humburg
create_download <- function(x, download){
  downloads <- knitr::opts_knit$get('.downloads')
  if(is.character(x) && length(x) == 1){
    name <- x
    x <- get(x)
  } else{
    name <- deparse(substitute(x))
  }
  retrieve <- missing(download)
  if(retrieve){
    if(name %in% names(downloads)){
      download <- downloads[[name]]
    } else {
      stop('Unable to locate download for ', name)
    }
  }
  if(!dir.exists(dirname(download$target))){
    dir.create(dirname(download$target), recursive=TRUE)
  }
  do.call(download$writer, c(list(x, download$target), download$args))
  download$written <- TRUE
  downloads[[name]] <- download
}

#' Create download link
#'
#' @param download An object of class \code{Download}.
#' @param text Character vector of length 1 providing the text to display for the link.
#' @param format Output format for the link.
#'
#' @return Either a string providing the link in the requested format, or, for
#' \code{format='table'}, a data frame with a single row and two columns.
#' @export
#' @importFrom methods is
download_link <- function(download, text=download$label,
                          format=c('html', 'markdown', 'table')){
  format <- match.arg(format)
  if(!is(download, 'Download')){
    if(!(is.character(download) && length(download) == 1)){
      download <- deparse(substitute(download))
    }
    download <- knitr::opts_knit$get('.downloads')[[download]]
  }
  switch(format,
         markdown=paste0('[', text, '](', download$target, ')'),
         html=paste0('<a download href=', download$target,'>', text, '</a>'),
         table=make_ref_links(data.frame(File=download_link(download, text, 'markdown'),
                          Description=download$description, stringsAsFactors=FALSE)))
}
