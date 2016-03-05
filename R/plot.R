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
plotMD <- function(fig, format = c('screen', 'interactive', 'print')){
  format <- match.arg(format)
  if(format == 'interactive') {
    plotly::ggplotly(fig)
  }else {
    fig
  }
}
