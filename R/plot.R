plot_formats <- c(screen='png', print='pdf', interactive='png')

#' Create plots for inclusion in RMarkdown reports
#'
#' @param fig A ggplot2 plot for processing.
#' @param format A character vector of length one describing the desired
#'    output format.
#' @return Depending on the value of \code{format} either a ggplot2 object
#'  (possibly the same as \code{fig}) or, if \code{format = 'interactive'}
#'  a \code{plotly} plot.
#' @examples \dontrun{
#'  library(ggplot2)
#'  library(plotly)
#'  fig <- ggplot(cars, aes(x=speed, y=dist)) + geom_point()
#'  plotMD(fig, format='s') ## uses ggplot2
#'  plotMD(fig, format='i') ## uses plotly
#'
#' }
#' @seealso \code{\link[plotly]{ggplotly}}
#' @author Peter Humburg
#' @importFrom plotly ggplotly
#' @export
plotMD <- function(fig, format=knitr::opts_current$get('fig_format')){
  format <- match.arg(format, c('screen', 'print', 'interactive'), several.ok=TRUE)
  if(format[1] == 'interactive') {
    width <- knitr::opts_current$get('out.width')
    width <- as.integer(stringr::str_replace(width, 'px', ''))
    height <- knitr::opts_current$get('out.height')
    height <- as.integer(stringr::str_replace(height, 'px', ''))
    plotly::ggplotly(fig, width=width, height=height)
  }else {
    fig
  }
}

#' Set figure related chunk options for interactive figures
#' @param out.width Output width for figure in pixels
#' @param out.height Output height for figure in pixels
#' @param ... Additional knitr chunk options
#' @return A list with the previous set of options is returned invisibly.
#' @author Peter Humburg
#' @export
interactiveFig <- function(out.width='800px', out.height='600px', ...){
  opts <- c(list(out.width=out.width, out.height=out.height),
            list(...), list(format='interactive'))
  do.call(figureOptions, opts)
}

#' Set figure related chunk options for static figures
#' @param fig.width Figure width in inches
#' @param fig.height Figure height in inches
#' @param dpi Figure resolution in dots per inch
#' @param ... Additional knitr chunk options
#' @return A list with the previous set of options is returned invisibly.
#' @author Peter Humburg
#' @export
screenFig <- function(fig.width=8, fig.height=8, dpi=300, ...){
  opts <- c(list(fig.width=fig.width, fig.height=fig.height, dpi=dpi),
            list(...), list(format='screen'))
  do.call(figureOptions, opts)
}

#' Set figure related chunk options for print figures
#'
#' @param fig.width Figure width in inches
#' @param fig.height Figure height in inches
#' @param dpi Resolution to use for figure output
#' @param ... Additional knitr chunk options
#'
#' @return A list with the previous set of options is returned invisibly.
#' @author Peter Humburg
#' @export
printFig <- function(fig.width=8, fig.height=8, dpi=300, ...){
  opts <- c(list(fig.width=fig.width, fig.height=fig.height, dpi=dpi),
            list(...), list(format='print'))
  do.call(figureOptions, opts)
}

#' Manage chunk options for different figure formats
#' @param ... Named arguments corresponding to knitr chunk options.
#' @param format character vector of length one indicting the format
#' for which options should be set.
#' @author Peter Humburg
#' @export
figureOptions <- function(..., format){
  target = paste('reportmd', 'figure', format, sep='.')
  dots <- list(...)
  if(length(dots)){
    dots <- merge_list(dots, knitr::opts_chunk$get(target))
    arg <- list(dots)
    names(arg) <- target
    do.call(knitr::opts_chunk$set, dots)
  } else{
    knitr::opts_chunk$get(target)
  }
}

#' Figure and Table cross-references
#'
#' Define figure and table labels and reference them in the text to create automatic
#' reference the corresponding table or figure. Numbers are assigned automatically.
#'
#' @param label Identifying label.
#' @param caption Caption to display.
#' @param target An object of class \code{Dependency}.
#' @param prefix Fixed part of the printed label. Defaults to 'Figure' for figures and to
#' 'Table' for tables.
#' @param sep Separator to use between printed label and caption.
#' @param prefix.highlight Markdown code the figure label should be wrapped in.
#'    Allows the label to be displayed in bold or italics.
#'
#' @details Typically \code{figRef} only needs to be called
#' explicitly to refer to figures in the text. The call to set the label and generate
#' the appropriately modified caption is issued automatically when a code chunk with
#' the \code{fig.cap} option is encountered. In that case the label used in the reference
#' should be the label of the code chunk that generated the figure. Note that this means
#' code chunks that generate figures have to be named.
#'
#' Reference can occur at any point in the text. It is not strictly necessary to
#' define a label before it is referenced. However, numbering is determined by the order
#' in which labels are first encountered and this can lead to figures or tables appearing
#' to be out of order.
#'
#' It is possible to refer to figures in other documents by supplying a \code{Dependency}
#' object to argument \code{target}. This will generate a reference of the form
#' "Short Title, Figure Label", e.g. "Methods, Figure 1". It is an error to provide
#' a \code{target} as well as a \code{caption}.
#'
#' @return If the \code{caption} argument is present a string combining the (computed)
#' figure label with the caption. Otherwise a (markdown formatted) link to the
#' figure is returned.
#' @export
#' @importFrom knitr opts_chunk
#' @importFrom knitr opts_knit
#' @author Peter Humburg
#' @examples
#' knitr::opts_knit$set('figcap.prefix','Figure')
#' knitr::opts_knit$set('figcap.prefix.highlight', '**')
#'
#' figRef('foo', 'A test caption')
#' figRef('foo')
figRef <- local({
  tag <- numeric()
  created <- logical()
  used <- logical()
  function(label, caption, target, prefix = knitr::opts_knit$get("figcap.prefix"),
           sep = knitr::opts_knit$get("figcap.sep"),
           prefix.highlight = knitr::opts_knit$get("figcap.prefix.highlight")) {
    if(!missing(target)){
      if(!missing(caption)){
        stop("Can't set caption for Figure in external file '", target$label, "'.")
      }
      target <- knitr::opts_knit$get('dependencies')[[target]]
      idx <- get_index(target, 'figure')
      idx <- subset(idx, V1 == label & V2 == target$label)
      if(nrow(idx) == 1){
        result <- paste0("[", target$short_title, ', ', idx[1, 3], "](", target$document, "#", knitr::opts_chunk$get('fig.lp'), label, ")")
      } else{
        warning("Unable to locate Figure with label '", label, "' in file '", target$label, "'.")
      }
    } else{
      i <- which(names(tag) == label)
      if (length(i) == 0) {
        i <- length(tag) + 1
        tag <<- c(tag, i)
        names(tag)[length(tag)] <<- label
        used <<- c(used, FALSE)
        names(used)[length(used)] <<- label
        created <<- c(created, FALSE)
        names(created)[length(created)] <<- label
      }
      if (!missing(caption)) {
        created[label] <<- TRUE
        caption <- eval(caption)
        index(label, knitr::current_input(), paste(prefix, i), caption, type="figure")
        result <- paste0(prefix.highlight, prefix, "&nbsp;", i, sep, prefix.highlight,
                         " ", caption)
      } else {
        used[label] <<- TRUE
        result <- paste(prefix, tag[label], sep="&nbsp;")
        result <- paste0('[', result, '](#', knitr::opts_chunk$get('fig.lp'), label, ')')
      }
    }
    result
  }
})

utils::globalVariables(c('V1', 'V2'))

#' @rdname figRef
#' @export
tabRef <- local({
  tag <- numeric()
  created <- logical()
  used <- logical()
  function(label, caption, target, prefix = knitr::opts_knit$get("tabcap.prefix"),
           sep = knitr::opts_knit$get("tabcap.sep"),
           prefix.highlight = knitr::opts_knit$get("tabcap.prefix.highlight")) {
    if(!missing(target)){
      if(!missing(caption)){
        stop("Can't set caption for Table in external file '", target$label, "'.")
      }
      target <- knitr::opts_knit$get('dependencies')[[target]]
      idx <- get_index(target, 'table')
      idx <- subset(idx, V1 == label & V2 == target$label)
      if(nrow(idx) == 1){
        result <- paste0("[", target$short_title, ', ', idx[1, 3], "](", target$document, "#tab:", label, ")")
      } else{
        warning("Unable to locate Table with label '", label, "' in file '", target$label, "'.")
      }
    } else{
      i <- which(names(tag) == label)
      if (length(i) == 0) {
        i <- length(tag) + 1
        tag <<- c(tag, i)
        names(tag)[length(tag)] <<- label
        used <<- c(used, FALSE)
        names(used)[length(used)] <<- label
        created <<- c(created, FALSE)
        names(created)[length(created)] <<- label
      }
      if (!missing(caption)) {
        created[label] <<- TRUE
        index(label, knitr::current_input(), paste(prefix, i), caption, type="table")
        result <- paste0(prefix.highlight, prefix, "&nbsp;", i, sep, prefix.highlight,
               " ", eval(caption))
      } else {
        used[label] <<- TRUE
        result <- paste(prefix, tag[label], sep="&nbsp;")
        result <- paste0('[', result, '](#tab:', label, ')')
      }
    }
    result
  }
})
