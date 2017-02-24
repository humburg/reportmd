## Chunk hooks

#' Special processing for figure and table chunks
#'
#' These hooks are intended for use with knitr. There is usually no need
#' to call them directly.
#'
#' @param before Logical indicating whether the hook was called befere or
#' after the chunk has been processed.
#' @param options List of chunk options.
#' @param envir Environment in which the chunk is evaluated.
#'
#' @note These hooks are intended for chunks with the \code{fig.cap} (for figures) and
#' \code{tab.cap} (for tables) option and this is assumed to be present.
#' @return The chunk hooks produce markup that adds anchors to enable direct links to the
#' table or figure and add s the caption where required.
#' @author Peter Humburg
#' @importFrom knitr opts_chunk
#' @export
#'
#' @rdname figure-hooks
fig.cap_chunk_hook <- function(before, options, envir) {
  fmt <- options$fig_format %||% list('screen')
  if('interactive' %in% fmt){
    if(before){
      if('screen' %in% fmt){
        paste0('<div id="', knitr::opts_chunk$get('fig.lp'), options$label, '" class="figure responsive">')
      } else {
        paste0('<div id="', knitr::opts_chunk$get('fig.lp'), options$label, '" class="figure">')
      }
    } else{
      options(reportmd.figure.current=NULL)
      paste0("</div>")
    }
  }
}

#' @export
#' @rdname figure-hooks
tab.cap_chunk_hook <- function(before, options, envir) {
  if(before){
    paste0('<div id="tab:', options$label, '" class="table-wrapper">',
           '<p class="caption">', options$tab.cap, "</p>")
  } else{
    "</div>"
  }
}


## Option hooks

#' @return \code{fig.cap_opts_hook} returns a list of chunk options
#' adjusted for the requested figure format.
#' @export
#' @rdname figure-hooks
fig.cap_opts_hook <- function(options){
  fmt <- options$fig_format %||% list('screen')
  options$reportmd.figure.current=fmt

  options$fig.cap = figRef(options$label, options$fig.cap)
  if(length(options$fig_download) && ('print' %in% fmt && length(fmt) > 1) || fmt == 'interactive'){
    download <- options$fig_download
    download <- stringr::str_replace(options$fig_download, stringr::fixed('%PATH%'),
                                     file.path(options$fig.path,
                                               paste(options$label, '1.pdf', sep='-')))
    options$fig.cap <- paste(options$fig.cap, download)
  }

  options$cache <- FALSE
  if(options$hide.fig.code){
    options$echo <- FALSE
    options$warning <- FALSE
  }

  opts <- knitr::opts_chunk$get(paste('reportmd', 'figure', fmt, sep='.'))
  if(length(fmt) == 1){
    opts <- list(opts)
    names(opts) <- paste('reportmd', 'figure', fmt, sep='.')
  }
  opts <- merge_list(opts, options)
  opts
}

#' @return \code{fig.width_opts_hook} adjusts figure width for print and screen
#' figures.
#' @export
#' @rdname figure-hooks
fig.width_opts_hook <- function(options){
  options$reportmd.figure.screen$width <- options$fig.width
  options$reportmd.figure.print$width <- options$fig.width
  options
}

#' @return \code{fig.height_opts_hook} adjusts figure height for print and screen
#' figures.
#' @export
#' @rdname figure-hooks
fig.height_opts_hook <- function(options){
  options$reportmd.figure.screen$height <- options$fig.height
  options$reportmd.figure.print$height <- options$fig.height
  options
}


#' @return \code{tab.cap_opts_hook} returns a list of chunk options with
#' the \code{tab.cap} option augmented for automatic table numbering.
#' @export
#' @rdname figure-hooks
tab.cap_opts_hook <- function(options){
  options$tab.cap <- tabRef(options$label, options$tab.cap)
  options$echo <- FALSE
  options$cache <- FALSE
  options$results <- 'asis'
  options
}

#' Dependency processing
#'
#' Hooks for use with knitr to facilitate handling of inter-file dependencies.
#' @param options List of chunk options
#'
#' @return Updated list of options
#' @export
#' @importFrom knitr opts_knit
#' @author Peter Humburg
dependson_opts_hook <- function(options){
  depends <- options$dependson
  parse <- grepl("\\S+:\\S+", depends)
  if(any(parse)){
    if(is.null(opts_knit$get('dependencies'))){
      stop("Requested external dependency (", depends[parse][1],
           ") but dependency information is missing. Did you list dependencies in the header?")
    }
    external <- strsplit(depends[parse], "[:/]")
    for(ext_dep in external){
      dep <- opts_knit$get('dependencies')[ext_dep[[1]]]
      dep[[1]]$chunks <- ext_dep[[2]]
      if(opts_knit$get('use_namespace')){
        if(!ext_dep[[1]] %in% names(knitr::knit_global())){
          assign(ext_dep[[1]], new.env(parent=knitr::knit_global()), envir=knitr::knit_global())
        }
        load_dependencies(dep, where=get(ext_dep[[1]], envir = knitr::knit_global()))
      } else{
        load_dependencies(dep)
      }
      if(is.null(options$ext.depends)){
        options$ext.depends <- list()
      }
      if(length(ext_dep) > 2){
        options$ext.depends <- c(options$ext.depends, get(ext_dep[3]))
      } else{
        options$ext.depends <- c(options$ext.depends, file.mtime(dep[[1]]$source))
      }
    }
  }
  options
}

format_opts_hook <- function(options){
  general_opts <- c('fig.width', 'fig.height', 'out.width', 'out.height', 'out.extra', 'dpi')
  if('interactive' %in% options$fig_format){
    options$fig_format <- union(options$fig_format, 'screen')
    if(length(options$fig_download)){
      options$fig_format <- union(options$fig_format, 'print')
    }
  }
  options$dev <- plot_formats[options$fig_format]
  dev_opts <- lapply(options$fig_format, function(x )figureOptions(format=x))
  opts <- lapply(dev_opts, function(x, general) x[general], general_opts)
  opts <- Reduce(function(x, y) mapply(`%||%`, x, y, SIMPLIFY=FALSE), opts)
  opts <- opts[!sapply(opts, is.null)]
  options[names(opts)] <- opts
  dev_opts <- lapply(dev_opts, function(x, general) x[!names(x) %in% general], general_opts)
  names(dev_opts) <- options$dev
  options$dev.args <- dev_opts
  options
}

## Output hooks

#' @importFrom knitr opts_knit
document_hook <- function(x){
  if(!knitr::opts_knit$get('child')){
    if(!is.null(opts_knit$get('dependencies'))){
      deps <- opts_knit$get('dependencies')
      link_section <- c('##Related Documents',
                        sapply(deps, printMD, format='md reference'), '',
                        sapply(deps, printMD, format='reference'))
      link_section <- paste(link_section, collapse='  \n')
      ref_idx <- which(stringr::str_detect(x, '^\\s*##\\s*[Rr]eferences\\s*$'))
      if(length(ref_idx)){
        x <- c(x[1:(ref_idx[1]-1)], link_section, x[ref_idx:length(x)])
      } else {
        x <-c(x, link_section)
      }
    }
    mapply(write_index, knitr::opts_knit$get('reportmd.index'), names(knitr::opts_knit$get('reportmd.index')))
  }
  x
}

inline_hook <- function(x){
  printMD(x)
}

## Hooks inherited from knitrBootstrap
bootstrap_chunk_hook <- function(x, options){
  class <- options[["bootstrap.class"]] <- options[["bootstrap.class"]] %||% "row"
  label <- options[["label"]]
  button <- NULL
  if(options[['echo']]){
    button <- tags$button(class=c("btn", "btn-default", "btn-xs", "sidenote"),
                          'data-toggle'="tooltip", title=label,
                          tags$span(paste(options[["engine"]], 'source')))
  }
  tags$div(class="container-fluid",
           tags$div(class=c(class, 'code-chunk'), id=add_anchor(label),
                    button, x))
}

#' @importFrom knitr hook_plot_md
bootstrap_plot_hook <- function(x, options) {
  thumbnail <- options[["bootstrap.thumbnail"]] <- options[["bootstrap.thumbnail"]] %||% TRUE
  if (!thumbnail) {
    fig <- knitr::hook_plot_md(x, options)
    if(options$fig.show != "hold"){
      fig <- paste0("\n\n", fig, "\n\n")
    }
    classes <- c("row", "text-center")
    if('interactive' %in% options$fig_format){
      classes <- c(classes, 'plotly-fallback')
    }
    return(tags$div(class = classes, fig))
  }
  thumbnail_plot_hook(x, options)
}

thumbnail_plot_hook <- function(x, options){
  thumbnail_size <- options["bootstrap.thumbnail.size"] <- options[["bootstrap.thumbnail.size"]] %||% "col-md-6"
  src <- opts_knit$get('upload.fun')(x)
  caption <- options$fig.cap %||% ""
  img <- tags$img(src=src, alt=caption)
  if(caption != "" && options$fig.show != "hold"){
    caption <- tags$p(class="caption", caption)
  }
  fig <- tags$a(href = "#", class = "thumbnail", img)
  if (options$fig.show == "hold"){
    fig <- tags$div(class=thumbnail_size, fig)
  } else{ #only one figure from this code block so center it
    classes <- c("figure", calc_offset(thumbnail_size), thumbnail_size)
    if('interactive' %in% options$fig_format){
      classes <- c(classes, 'plotly-fallback')
    }
    fig <- tags$div(class = classes, id=add_anchor(options[["label"]], prefix=knitr::opts_chunk$get('fig.lp')), fig, caption)
    fig <- tags$div(class = "row", fig)
  }
  fig
}
