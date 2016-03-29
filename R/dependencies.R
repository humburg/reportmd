
#' Load results from child documents
#'
#' @param deps List of dependencies
#' @param opts \code{knitr} options
#'
#' @return Called for its side effect.
#' @export
#' @importFrom rmarkdown render
#' @importFrom devtools clean_source
#' @author Peter Humburg
load_dependencies <- function(deps, opts){
  started <- Sys.time()
  out_ext <- c(latex='pdf', html='html', markdown='md', jerkyll='html')
  docs <- lapply(deps, function(x){
    if(is.character(x)) x else names(x)
  })
  chunks <- lapply(deps, function(x) {
    if(is.list(x)) x[[1]] else list()
  })
  for(i in 1:length(docs)){
    format <- opts$get('rmarkdown.pandoc.to')
    child_path <- opts$get('child.path')
    if(child_path == '') child_path <- getwd()
    input <- file.path(child_path, docs[i])
    prefix <- sub("\\.[^.]+$", "", input)
    out <- paste(prefix, out_ext[format], sep='.')
    cache <- file.path(paste0(prefix, '_cache'), format)
    if(!file.exists(out) || file.mtime(out) < started){
      wrapper <- paste0("render_", basename(prefix), '.R')
      cat("rmarkdown::render('", input, "', quiet=TRUE)",
          file=wrapper, sep='')
      devtools::clean_source(wrapper, quiet=TRUE)
      unlink(wrapper)
    }
    if(!file.exists(out)){
      stop("Unable to locate output of child document: ", out)
    }

    ## identify files to load
    ## either data from all cached chunks or only the ones listed explicitly
    cache_pattern <- '^[^_]{2}'
    if(length(chunks[[i]])){
      cache_pattern <- paste(paste0('^', chunks[[i]], '_'), collapse='|')
    }
    cached <- dir(cache, pattern=cache_pattern, full.names=TRUE)
    cached <- unique(sub("\\.[^.]+$", "", cached))
    if(length(cached) < length(chunks[i])){
      found <- sapply(strsplit(cached, '_'), '[[', 1)
      found_pattern <- paste(found, collapse='|')
      missing <- cache[i][!grepl(found_pattern, cache[i])]
      stop("Unable to locate output for chunks ", paste(missing, collpse=', '), " from document ", docs[i], ".")
    }
    lapply(cached, lazyLoad)
  }
  invisible(NULL)
}
