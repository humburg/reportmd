
panel_types <- c("source" = "panel-primary",
                 "output" = "panel-success",
                 "message" = "panel-info",
                 "warning" = "panel-warning",
                 "error" = "panel-danger")

#' Writing Complex Scientific Reports in R
#'
#' An R Markdown document format for multi-part reports.
#' The *Multi-part Report* template included in the package provides a detailed
#' example of its use.
#'
#' @param theme Visual theme to use for styling of the html output
#' @param highlight Visual style to use for Syntax highlighting
#' @param pandoc_args Arguments to be passed to pandoc
#' @param fig_format Default format(s) for figures
#' @param fig_download Logical indicating whether a download link should be added to
#'     figure captions.
#' @param fig_width Default figure width in inches.
#' @param fig_height Default figure height in inches.
#' @param figcap_prefix Prefix to use for figure labels. Figure labels will
#' consist of this prefix and an automatically generated number.
#' @param figcap_sep Separator to use between figure label and caption.
#' @param figcap_prefix_highlight Markdown to use for highlighting of figure label.
#' @param thumbnail A logical indicating whether thumbnails should be included
#' in the output document instead of the full figure.
#' @param thumbnail_size The width of the thumbnails, specified as a
#' \href{http://getbootstrap.com/css/#grid}{bootstrap column class}. The default
#' produces thumbnails that are 50\% of the text width. Use \code{col-md-12} for
#' full width thumbnails.
#' @param tabcap_prefix Prefix to use for table labels. Table labels will
#' consist of this prefix and an automatically generated number.
#' @param tabcap_sep Separator to use between table label and caption.
#' @param tabcap_prefix_highlight Markdown to use for highlighting of table label.
#' @param dpi The resolution to use for figures.
#' @param use_namespace Logical indicating whether variables loaded from dependencies
#' should be encapsulated into their own namespace. If \code{TRUE} these variables
#' are loaded into a separate environment for each depedency, rather than into
#' the global environment.
#' @param template Path to custom template
#' @param ... Additional arguments are passed to rmarkdown::html_document
#' @importFrom rmarkdown html_dependency_jquery
#' @export
multi_document <- function(theme = NULL, highlight = NULL, pandoc_args = NULL,
                           fig_format=c('screen', 'print'), fig_download=TRUE,
                           fig_height=8, fig_width=8, figcap_prefix="Figure",
                           figcap_sep = ":", figcap_prefix_highlight = "**",
                           thumbnail=TRUE, thumbnail_size='col-md-6',
                           tabcap_prefix="Table", tabcap_sep = ":",
                           tabcap_prefix_highlight = "**", dpi=300,
                           use_namespace=FALSE, template = system.file(
                             package="reportMD", "rmarkdown/rmd/default.html"),
                           depends=NULL, ...){
  theme <- theme %||% "default"
  highlight <- highlight %||% "default"
  pandoc_args <- pandoc_args %||% c(
    "--variable",
    "mathjax-url:https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
  )
  results <- rmarkdown::html_document(
    highlight = NULL,
    theme = NULL,
    fig_height=fig_height, fig_width=fig_width,
    extra_dependencies=list(
      rmarkdown::html_dependency_jquery(),
      html_dependency_bootstrap3(theme),
      html_dependency_hljs(highlight),
      html_dependency_magnific_popup(),
      html_dependency_navigation(),
      html_dependency_multi()
    ),
    pandoc_args = check_pandoc_args(pandoc_args), ...)
  ff <- character()
  for(format in fig_format){
    ff <- union(ff, plot_formats[format])
  }

  fig_download_text <- ''
  if(length(fig_format) > 1 && 'print' %in% fig_format && isTRUE(fig_download)){
    fig_download_text <- '(Download as [PDF](%PATH%))'
  }

  if(!is.null(depends)){
    depends <- params2deps(depends)
    for(d in depends){
      update_dependency(d)
    }
    copy_dependencies(depends)
  }

  results$knitr <- list(
    opts_knit = list(reportmd.index=list(figure=matrix(ncol=4, nrow=0), table=matrix(ncol=4, nrow=0)),
                     loaded_chunks=list(), dependencies=depends, figcap.prefix=figcap_prefix,
                     figcap.sep = figcap_sep, figcap.prefix.highlight = figcap_prefix_highlight,
                     use_namespace=use_namespace,
                     tabcap.prefix = tabcap_prefix, tabcap.sep = tabcap_sep,
                     tabcap.prefix.highlight = tabcap_prefix_highlight),
    opts_chunk = list(tidy=FALSE, highlight=FALSE, cache=TRUE, dev=ff, fig_format=fig_format,
                      hold=TRUE, hide.fig.code=TRUE, fig_download=fig_download_text,
                      dpi=dpi, bootstrap.thumbnail.size=thumbnail_size,
                      bootstrap.thumbnail=thumbnail,
                      reportmd.figure.interactive=list(out.width='700px', out.height='600px'),
                      reportmd.figure.screen=list(fig.width=fig_width, fig.height=fig_height, dpi=dpi),
                      reportmd.figure.print=list(fig.width=fig_width, fig.height=fig_height, dpi=dpi)),
    knit_hooks = multi_knit_hooks(),
    opts_hooks = multi_opts_hooks()
  )

  # first call to 'rmarkdown::html_document' uses the default template to retain MathJax functionality
  # then the pandoc template setting is modified to use the custom template
  template_arg <- which(results$pandoc$args == "--template") + 1L
  results$pandoc$args[template_arg] <- template

  results
}

# create an html dependency for our bootstrap 3, originally from rmarkdown package
#' @importFrom htmltools htmlDependency
html_dependency_bootstrap3 <- function(theme) {
  htmltools::htmlDependency(name = "bootstrap3",
                            version = "3.2.0",
                            src = system.file("rmarkdown/rmd/bootstrap3/", package="reportMD"),
                            meta = list(viewport = "width=device-width, initial-scale=1.0"),
                            script = "js/bootstrap.min.js",
                            stylesheet = c("css/bootstrap.min.css",
                                           paste0("css/themes/", theme, "/bootstrap.min.css")))
}

html_dependency_hljs <- function(highlight) {
  htmltools::htmlDependency(name = "highlightjs",
                            version = "9.9",
                            src = system.file(package="reportMD", "rmarkdown/rmd/highlightjs/"),
                            script = "highlight.pack.js",
                            stylesheet=paste0("styles/", highlight, ".css"))
}

html_dependency_multi <- function() {
  htmltools::htmlDependency(name = "reportMD_multi",
                            version = "0.0.1",
                            src = system.file(package="reportMD", "rmarkdown/templates/multipart_report/skeleton/"),
                            script = "js/multi.js",
                            stylesheet="css/multi.css")
}

html_dependency_magnific_popup <- function() {
  htmltools::htmlDependency(name = "MagnificPopup",
                            version = "0.9.9",
                            src = system.file(package="reportMD", "rmarkdown/rmd/magnific_popup/"),
                            script = "magnific-popup.js",
                            stylesheet="magnific-popup.css")
}

html_dependency_navigation <- function(){
  htmltools::htmlDependency(name = "Navigation",
                            version = "1.1",
                            src = system.file(package="rmarkdown", "rmd/h/navigation-1.1/"),
                            script = "tabsets.js")
}


generate_panel <- function(engine, name, label, x, show){
  tags$div(class=c("panel", panel_types[name]),
           tags$pre(tags$code(class=c(name, tolower(engine)), 'data-label'=label, x))
  )
}

#' @importFrom knitr opts_current
multi_knit_hooks <- function() {
  html_hook <- function(name) {
    force(name)
    function(x, options) {
      x <- paste(x, collapse = "\n")
      show <- switch(name,
                     source = (options[["bootstrap.show.code"]] <- options[["bootstrap.show.code"]] %||% TRUE),
                     output = (options[["bootstrap.show.output"]] <- options[["bootstrap.show.output"]] %||% TRUE),
                     message = (options[["bootstrap.show.message"]] <- options[["bootstrap.show.message"]] %||% TRUE),
                     warning = (options[["bootstrap.show.warning"]] <- options[["bootstrap.show.warning"]] %||% TRUE),
                     error = (options[["bootstrap.show.error"]] <- options[["bootstrap.show.error"]] %||% TRUE),
                     TRUE)
      generate_panel(options$engine, name, knitr::opts_current$get("label"), x, !show)
    }
  }
  c(
    sapply(c("source", "warning", "message", "error", "output"), html_hook),
    plot = bootstrap_plot_hook,
    chunk = bootstrap_chunk_hook,
    fig.cap=fig.cap_chunk_hook,
    tab.cap=tab.cap_chunk_hook,
    document=document_hook,
    inline=inline_hook,
    NULL
  )
}

multi_opts_hooks <- function() {
  list(fig.cap=fig.cap_opts_hook,
       tab.cap=tab.cap_opts_hook,
       dependson=dependson_opts_hook,
       fig.width=fig.width_opts_hook,
       fig.height=fig.height_opts_hook,
       fig_format=format_opts_hook)
}
