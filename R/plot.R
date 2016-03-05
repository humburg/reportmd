#' Create plots for inclusion in RMarkdown reports
#'
#' @param fig A ggplot2 plot for processing.
#' @param format A character vector of length one describing the desired
#'    output format.
#' @return Depending on the value of \code{format} either a ggplot2 object
#'  (possibly the same as \code{fig}) or, if \code{format = 'interactive'}
#'  a \code{\link[plotly]{plotly-package}} plot.
#' @author Peter Humburg
#' @export
plotMD <- function(fig, format = c('screen', 'interactive', 'print')){
  if(format == 'interactive') {
    ggplotly(fig)
  }else {
    fig
  }
}
