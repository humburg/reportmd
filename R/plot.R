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
plotMD <- function(fig, format=options('reportmd.figure.current')){
  if(format == 'interactive') {
    plotly::ggplotly(fig)
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

printFig <- function(fig.width=8, fig.height=8, ...){
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
    dots <- merge_list(dots, options(target)[[1]])
    arg <- list(dots)
    names(arg) <- target
    do.call(knitr::opts_chunk$set, dots)
    do.call(options, arg)[[1]]
  } else{
    options(target)[[1]]
  }
}

#' Figure and Table cross-references
#'
#' Define figure and table labels and reference them in the text to create automatic
#' reference the corresponding table or figure. Numbers are assigned automatically.
#'
#' @param label Identifying label.
#' @param caption Caption to display.
#' @param prefix Fixed part of the printed label. Defaults to 'Figure' for figures and to
#' 'Table' for tables.
#' @param sep Separator to use between printed label and caption.
#' @param prefix.highlight Markdown code the figure label should be wrapped in.
#'    Allows the label to be displayed in bold or italics.
#'
#' @details Typically \code{figRef} this function only needs to be called
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
#' @return If the \code{caption} argument is present a string combining the (computed)
#' figure label with the caption. Otherwise a (markdown formatted) link to the
#' figure is returned.
#' @export
#' @importFrom knitr opts_chunk
#' @author Peter Humburg
#' @examples
#' options(figcap.prefix='Figure')
#' options(figcap.prefix.highlight='**')
#'
#' figRef('foo', 'A test caption')
#' figRef('foo')
figRef <- local({
  tag <- numeric()
  created <- logical()
  used <- logical()
  function(label, caption, prefix = options("figcap.prefix"),
           sep = options("figcap.sep"), prefix.highlight = options("figcap.prefix.highlight")) {
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
      result <- paste0(prefix.highlight, prefix, " ", i, sep, prefix.highlight,
                       " ", caption)
    } else {
      used[label] <<- TRUE
      result <- paste(prefix, tag[label])
      result <- paste0('[', result, '](#', knitr::opts_chunk$get('fig.lp'), label, ')')
    }
    result
  }
})

#' @rdname figRef
#' @export
tabRef <- local({
  tag <- numeric()
  created <- logical()
  used <- logical()
  function(label, caption, prefix = options("tabcap.prefix"),
           sep = options("tabcap.sep"), prefix.highlight = options("tabcap.prefix.highlight")) {
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
      paste0(prefix.highlight, prefix, " ", i, sep, prefix.highlight,
             " ", caption)
    } else {
      used[label] <<- TRUE
      paste(prefix, tag[label])
    }
  }
})
