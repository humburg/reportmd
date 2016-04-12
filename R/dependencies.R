
#' @inheritParams load_dependencies
#' @export
#' @rdname load_dependencies
update_dependencies <- function(deps, opts){
  out_ext <- c(latex='pdf', html='html', markdown='md', jerkyll='html')
  docs <- sapply(deps, function(x){
    if(is.character(x)) x else names(x)
  })
  out <- character(length(docs))
  prefix <- character(length(docs))
  tag_dir <- character(length(docs))
  format <- opts$get('rmarkdown.pandoc.to')

  for(i in 1:length(docs)){
    child_path <- opts$get('child.path')
    if(child_path == '') child_path <- getwd()
    docs[i] <- normalizePath(file.path(child_path, docs[i]), winslash='/', mustWork=FALSE)
    prefix[i] <- sub("\\.[^.]+$", "", docs[i])
    out[i] <- paste(prefix[i], out_ext[format], sep='.')
    tag_dir[i] <- file.path(dirname(docs[i]), '.processing')
    if(!file.exists(tag_dir[i])){
      on.exit(unlink(tag_dir[i], recursive=TRUE), add=TRUE)
      dir.create(tag_dir[i])
    }
  }
  for(i in 1:length(docs)){
    if(needs_update(prefix[i], stringr::str_extract(docs[i], '[^.]+$'), out_ext[format], opts$get('input.file'))){
      wrapper <- file.path(tag_dir[i], paste0("render_", basename(prefix[i]), '.R'))
      cat("setwd('..')\n", "rmarkdown::render(normalizePath('", docs[i], "'), quiet=TRUE)",
          file=wrapper, sep='')
      devtools::clean_source(wrapper, quiet=TRUE)
    }
    if(!file.exists(out[i])){
      stop("Unable to locate output of child document: ", out[i])
    }
  }
}


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
  if(is.null(deps)) return(invisible(character(0)))
  docs <- sapply(deps, function(x){
    if(is.character(x)) x else names(x)
  })
  chunks <- lapply(deps, function(x) {
    if(is.list(x)) x[[1]] else list()
  })
  prefix <- character(length(docs))
  format <- opts$get('rmarkdown.pandoc.to')
  out_ext <- c(latex='pdf', html='html', markdown='md', jerkyll='html')

  for(i in 1:length(docs)){
    prefix[i] <- sub("\\.[^.]+$", "", docs[i])
    cache <- file.path(paste0(prefix[i], '_cache'), format)
    if(needs_update(prefix[i], stringr::str_extract(docs[i], '[^.]+$'), out_ext[format], opts$get('input.file'))){
      update_dependencies(deps, opts)
    }
    ## identify files to load
    ## either data from all cached chunks or only the ones listed explicitly
    cache_pattern <- '^[^_]{2}'
    if(length(chunks[[i]])){
      cache_pattern <- paste(paste0('^', chunks[[i]], '_'), collapse='|')
    }
    cached <- dir(file.path(opts$get('child.path'), cache), pattern=cache_pattern, full.names=TRUE)
    cached <- unique(sub("\\.[^.]+$", "", cached))
    if(length(cached) < length(chunks[[i]])){
      found <- sapply(strsplit(cached, '_'), '[[', 1)
      found_pattern <- paste(found, collapse='|')
      missing <- cache[i][!grepl(found_pattern, cache[i])]
      stop("Unable to locate output for chunks ", paste(missing, collpse=', '), " from document ", docs[i], ".")
    }
    lapply(cached, lazyLoad, parent.frame(1))
  }
  invisible(NULL)
}

#' Copy dependencies to working directory
#'
#' Output of child documents is copied to the working directory of the main document and installs
#' a \code{dependencies} package option.
#'
#' @param deps List of dependencies.
#' @param opts Knitr options.
#'
#' @return List of objects of class \link{Dependency}.
#' @export
#'
#' @author Peter Humburg
copy_dependencies <- function(deps, opts){
  if(is.null(deps)) return(NULL)
  out_ext <- c(latex='pdf', html='html', markdown='md', jerkyll='html')
  docs <- sapply(deps, function(x){
    if(is.character(x)) x else names(x)
  })

  out <- character(length(docs))
  prefix <- character(length(docs))
  format <- opts$get('rmarkdown.pandoc.to')
  for(i in 1:length(docs)){
    child_path <- opts$get('child.path')
    if(child_path == '') child_path <- getwd()
    docs[i] <- normalizePath(file.path(child_path, docs[i]), winslash='/', mustWork=FALSE)
    prefix[i] <- sub("\\.[^.]+$", "", docs[i])
    out[i] <- file.path(getwd(), basename(prefix[i]))
    if(out[i] != prefix[i]){
      file.copy(paste(prefix[i], out_ext[format], sep='.'), dirname(out[i]), overwrite=TRUE)
      file.copy(paste(prefix[i], 'files', sep='_'), dirname(out[i]), recursive=TRUE, overwrite=TRUE)
    }
    out[i] <- basename(paste(out[i], out_ext[format], sep='.'))
  }
  deps <- mapply(Dependency, names(deps), out, docs, SIMPLIFY=FALSE)
  opts$set(dependencies=deps)
  deps
}

needs_update <- function(prefix, in_format, out_format, main_file){
  update <- FALSE
  in_doc <- paste(prefix, in_format, sep='.')
  out_doc <- paste(prefix, out_format, sep='.')
  main_out <- file.path(dirname(main_file), basename(sub("\\.[^.]+$", paste0(".", out_format), main_file)))
  cache_dir <- file.path(paste(prefix, 'cache', sep='_'), out_format)
  if(!file.exists(out_doc)){
    update <- TRUE
  } else if(file.mtime(out_doc) < file.mtime(in_doc)){
    update <- TRUE
  } else if(!file.exists(cache_dir)){
    update <- TRUE
  } else{
    deps <- extract_yaml(in_doc)$params
    if(!is.null(deps)){
      deps <- deps$depends
    }
    if(!is.null(deps)){
      deps <- sapply(deps$value, function(x){
        if(is.character(x)) x else names(x)
      })
      cache_time <- sapply(deps, function(d, fmt)
        max(file.mtime(dir(file.path(paste(sub("\\.[^.]+$", "", d), 'cache', sep='_'), fmt), full.names=TRUE))), out_format)
      update <- any(cache_time > file.mtime(main_file))
      if(!update){
        update <- any(sapply(deps, function(d, output, main)
          needs_update(sub("\\.[^.]+$", "", d), stringr::str_extract(d, '[^.]+$'), output, main),
          out_format, main_file))
      }
      if(!update){
        deps_out <- paste(sub("\\.[^.]+$", "", deps), out_format, sep='.')
        times <- sapply(deps_out, file.mtime)
        timestamp <- file.mtime(out_doc)
        update <- any(times > timestamp)
      }
    }
  }
  update
}
