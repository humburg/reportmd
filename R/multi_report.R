
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
#' @param ... Additional arguments are passed to rmarkdown::html_document
#' @importFrom rmarkdown html_dependency_jquery
#' @export
multi_document <- function(theme = NULL, highlight = NULL, pandoc_args = NULL,
                           fig_format=c('screen', 'print'), fig_download=TRUE, ...){
  theme <- theme %||% "default"
  highlight <- highlight %||% "default"
  pandoc_args <- pandoc_args %||% c(
    "--variable",
    "mathjax-url:https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
  )
  results <- rmarkdown::html_document(
    highlight = NULL,
    theme = NULL,
    extra_dependencies=list(
      rmarkdown::html_dependency_jquery(),
      html_dependency_bootstrap3(theme),
      html_dependency_hljs(highlight),
      html_dependency_magnific_popup(),
      html_dependency_navigation(),
      html_dependency_multi()
    ),
    template =
      system.file(
        package="reportMD", "rmarkdown/rmd/default.html"),
    pandoc_args = check_pandoc_args(pandoc_args), ...)

  ff <- character()
  for(format in fig_format){
    ff <- union(ff, plot_formats[format])
  }

  fig_download_text <- ''
  if(length(fig_format) > 1 && 'print' %in% fig_format && isTRUE(fig_download)){
    fig_download_text <- '(Download as [PDF](%PATH%))'
  }

  args <- list(...)
  deps <- NULL
  if('depends' %in% names(args)){
    deps <- params2deps(args$depends)
    for(d in deps){
      update_dependency(d)
    }
    copy_dependencies(deps)
  }

  results$knitr <- list(
    opts_knit = list(reportmd.index=list(figure=matrix(ncol=4, nrow=0), table=matrix(ncol=4, nrow=0)),
                     loaded_chunks=list(), dependencies=deps, figcap.prefix="Figure",
                     figcap.sep = ":", figcap.prefix.highlight = "**",
                     tabcap.prefix = "Table", tabcap.sep = ":", tabcap.prefix.highlight = "**"),
    opts_chunk = list(tidy=FALSE, highlight=FALSE, cache=TRUE, dev=ff, fig_format=fig_format,
                      hold=TRUE, hide.fig.code=TRUE, fig_download=fig_download_text),
    knit_hooks = multi_knit_hooks(),
    opts_hooks = multi_opts_hooks()
  )

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
                            version = "8.2",
                            src = system.file(package="reportMD", "rmarkdown/rmd/highlightjs/"),
                            script = "highlight.pack.js",
                            stylesheet=paste0("highlight/", highlight, ".css"))
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
    NULL
  )
}

multi_opts_hooks <- function() {
  list(fig.cap=fig.cap_opts_hook,
       tab.cap=tab.cap_opts_hook,
       dependson=dependson_opts_hook,
       format=format_opts_hook)
}
