## Chunk hooks

#' Special processing for figure chunks
#'
#' These hooks are intended for use with knitr. There is usually no need
#' to call them directly.
#'
#' @param before Logical indicating whether the hook was called befere or
#' after the chunk has been processed.
#' @param options List of chunk options.
#' @param envir Environment in which the chunk is evaluated.
#'
#' @note These hooks are intended for chunks with the \code{fig.cap} option and this
#' is assumed to be present.
#' @return In the case of \code{fig.cap_chunk_hook} markup used to wrap the figure is returned.
#' @author Peter Humburg
#' @importFrom knitr opts_chunk
#' @export
#'
#' @rdname figure-hooks
fig.cap_chunk_hook <- function(before, options, envir) {
  global_fmt <- options('reportmd.figure.format') %||% list('screen')
  fmt <- options$format %||% global_fmt[[1]]
  if(fmt == 'interactive'){
    if(before){
      paste0('<div id="', knitr::opts_chunk$get('fig.lp'), options$label, '" class="figure">')
    } else{
      options(reportmd.figure.current=NULL)
      paste0('<p class="caption text-center">', options$fig.cap,"</p></div>")
    }
  }
}

## Option hooks

#' @return \code{fig.cap_opts_hook} returns a list of chunk options
#' adjusted for the requested figure format.
#' @export
#' @rdname figure-hooks
fig.cap_opts_hook <- function(options){
  global_fmt <- options('reportmd.figure.format')[[1]]
  fmt <- options$format %||% global_fmt
  options(reportmd.figure.current=fmt)

  options$fig.cap = figRef(options$label, options$fig.cap)
  options$cache <- FALSE
  if(options$hide.fig.code){
    options$echo <- FALSE
    options$warning <- FALSE
  }
  opts <- options(paste('reportmd', 'figure', fmt, sep='.'))[[1]]
  opts <- merge_list(opts, options)
  opts
}

#' Dependency processing
#'
#' Hooks for use with knitr to facilitate handling of inter-file dependencies.
#' @param options List of chunk options
#'
#' @return Updated list of options
#' @export
#' @author Peter Humburg
dependson_opts_hook <- function(options){
  depends <- options$dependson
  parse <- grepl("[\\S]+:\\S+", depends)
  if(any(parse)){
    if(is.null(options$dependencies)){
      stop("Requested external dependency (", depends[parse][1],
           ") but dependency information is missing. Did you list dependencies in the header?")
    }
    external <- strsplit(depends[parse], "[:/]")
    external[1] <- options$dependencies[external[1]]
    for(ext_dep in external){
      arg <- list(list(ext_dep[2]))
      names(arg) <- ext_dep[1]
      names(arg[[1]]) <- options$dependencies[[arg[1]]]
      load_dependencies(arg, knit_opts)
      if(length(ext_dep) > 2){
        options$ext.depends <- c(options$ext.depends, get(ext_dep[3]))
      } else{
        options$ext.depends <- c(options$ext.depends, file.mtime(ext_dep[1]))
      }
    }
  }
  options
}


#' Set knitr hooks
#'
#' Installs all required knitr hooks.
#'
#' @return Called for its side effect.
#' @author Peter Humburg
#' @importFrom knitr opts_hooks
#' @importFrom knitr knit_hooks
#' @export
installHooks <- function(){
  knitr::opts_hooks$set(fig.cap=fig.cap_opts_hook)
  knitr::opts_hooks$set(dependson=dependson_opts_hook)
  knitr::knit_hooks$set(fig.cap=fig.cap_chunk_hook)
}
