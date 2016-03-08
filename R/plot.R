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
#' @param fig.width Figure width in inches
#' @param fig.height Figure height in inches
#' @param out.width Output width for figure in pixels
#' @param out.height Output height for figure in pixels
#' @param ... Additional knitr chunk options
#' @return A list with the previous set of options is returned invisibly.
#' @author Peter Humburg
#' @export
interactiveFig <- function(fig.width=8, fig.height=6, out.width='800px', out.height='600px', ...){
  opts <- c(list(fig.width=fig.width, fig.height=fig.height, out.width=out.width, out.height=out.height),
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
