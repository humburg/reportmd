
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
  docs <- sapply(deps, function(x){
    if(is.character(x)) x else names(x)
  })
  chunks <- lapply(deps, function(x) {
    if(is.list(x)) x[[1]] else list()
  })
  out <- character(length(docs))
  prefix <- character(length(docs))
  tag_dir <- character(length(docs))

  for(i in 1:length(docs)){
    format <- opts$get('rmarkdown.pandoc.to')
    child_path <- opts$get('child.path')
    if(child_path == '') child_path <- getwd()
    docs[i] <- file.path(child_path, docs[i])
    prefix[i] <- sub("\\.[^.]+$", "", docs[i])
    out[i] <- paste(prefix[i], out_ext[format], sep='.')
    tag_dir[i] <- file.path(dirname(docs[i]), '.processing')
    if(!file.exists(tag_dir[i])){
      on.exit(unlink(tag_dir[i], recursive=TRUE), add=TRUE)
      dir.create(tag_dir[i])
    }
  }
  for(i in 1:length(docs)){
    cache <- file.path(paste0(prefix[i], '_cache'), format)
    tag <- file.path(tag_dir[i], paste0(basename(prefix[i]), '.complete'))
    if(!file.exists(tag)){
      wrapper <- file.path(tag_dir[i], paste0("render_", basename(prefix[i]), '.R'))
      cat("setwd('..')\n", "rmarkdown::render('", docs[i], "', quiet=TRUE)",
          file=wrapper, sep='')
      devtools::clean_source(wrapper, quiet=TRUE)
      file.create(tag)
    }
    if(!file.exists(out[i])){
      stop("Unable to locate output of child document: ", out[i])
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
    lapply(cached, lazyLoad, parent.frame(1))
  }
  invisible(NULL)
}
