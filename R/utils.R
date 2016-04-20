#' Replace null values with default
#'
#' @param x Variable to be tested.
#' @param y Default to use if \code{x} is \code{NULL}.
#' @return If \code{x} is \code{NULL}, \code{y} is returned, otherwise \code{x}.
#' @name null-or
#' @author Peter Humburg
#' @export
`%||%` <- function(x, y){
  if(is.null(x)) y else x
}

#' Merge two named lists
#'
#' @param x Base list
#' @param y Additional entries to be merged into \code{x}
#'
#' @note Entries in \code{x} that are also present in \code{y}
#' will use the values from \code{y}.
#' @return A list with entries from \code{x} and \code{y}.
#' @export
merge_list <- function(x,y){
  if(!is.null(y) && length(y)){
    x[names(y)] <- y
  }
  x
}

#' @importFrom stringr str_extract_all
#' @importFrom stringr str_replace
#' @importFrom stringr regex
#' @importFrom yaml yaml.load
#'
#' @author Peter Humburg
extract_yaml <- function(input, ...){
  contents <- readLines(input, ...)
  header <- stringr::str_extract_all(paste(contents, collapse="\n"),
                                     stringr::regex("^---.+?^(---|\\.\\.\\.)",
                                                    multiline=TRUE, dotall=TRUE))
  header <- lapply(header, stringr::str_replace_all, c('^---+$'='', '(\\.\\.\\.+$)|---+$'=''))
  header <- lapply(header, yaml::yaml.load)
  header <- Reduce(merge_list, header)
  header
}

#' Extract title from a Markdown document's yaml metadata
#'
#' @param input Name of input file.
#' @param ... Additional arguments are ignored.
#' @author Peter Humburg
rmd_title <- function(input, ...){
  metadata <- extract_yaml(input, ...)
  metadata$title
}

#' Extract title from an HTML document
#' @param input Name of input file.
#' @param ... Additional arguments are ignored.
#' @importFrom xml2 read_html
#' @importFrom xml2 xml_find_one
#' @importFrom xml2 xml_text
#'
#' @author Peter Humburg
html_title <- function(input, ...){
  contents <- xml2::read_html(input, ...)
  title <- xml2::xml_find_one(contents, '//title')
  xml2::xml_text(title)
}

#' Setup environment for Multi-document report
#' @param params List of document parameters (as provided in the YAML header)
#' @return Called for its side effect.
#' @author Peter Humburg
#' @export
setup <- function(params){
  opts_knit$set(input.file=sys.frame(1)$original_input)
  opts_knit$set(loaded_chunks=list())
  deps <- params2deps(params)
  load_dependencies(deps)
  copy_dependencies(deps)
  opts_knit$set(dependencies=deps)
  fig_format <- character()
  for(format in params$format){
    if(format == 'screen'){
      fig_format <- union(fig_format, 'png')
    } else if(format == 'print'){
      fig_format <- union(fig_format, 'pdf')
    } else if(format == 'interactive'){
      fig_format <- union(fig_format, 'png')
    }
  }
  knitr::opts_chunk$set(dev=fig_format)
  if(length(params$format) > 1 && 'print' %in% params$format && params$fig_download){
    knitr::opts_chunk$set(fig_download='(Download as [PDF](%PATH%))')
  }
}
