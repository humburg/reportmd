#' @importFrom knitr opts_knit
dependency_source <- function(deps){
  child_path <- knitr::opts_knit$get('child.path')
  if(child_path == '') child_path <- '.'
  docs <- sapply(deps, function(x){
    if(is.character(x)) x else names(x)
  })
  for(i in 1:length(docs)){
    docs[i] <- normalizePath(file.path(child_path, docs[i]), winslash='/', mustWork=FALSE)
  }
  docs
}

#' @importFrom knitr opts_knit
dependency_output <- function(source){
  out_ext <- c(latex='pdf', html='html', markdown='md', jerkyll='html')
  format <- 'html'
  prefix <- sub("\\.[^.]+$", "", source)
  paste(prefix, out_ext[format], sep='.')
}

dependency_subdir <- function(source, type){
  prefix <- sub("\\.[^.]+$", "", source)
  paste(prefix, type, sep="_")
}

#' Extract dependency information from YAML header
#'
#' @param deps List of dependencies
#'
#' @return A list of \code{Dependency} objects
#' @author Peter Humburg
#' @export
params2deps <- function(deps){
  if(is.null(deps)){
    return(list())
  }
  docs <- dependency_source(deps)
  mapply(Dependency, label=names(deps), source=docs, SIMPLIFY=FALSE)
}

#' @param dep Dependency to update.
#' @export
#' @importFrom knitr opts_knit
#' @importFrom callr r
#' @rdname load_dependencies
update_dependency <- function(dep){
  tag_dir <- file.path(dirname(dep$source), '.processing')
  if(!file.exists(tag_dir)){
    on.exit(unlink(tag_dir, recursive=TRUE), add=TRUE)
    dir.create(tag_dir)
  }
  if(needs_update(dep, knitr::current_input())){
    wrapper <- file.path(tag_dir, paste0("render_", dep$label, '.R'))
    message("Processing ", dep$source, "...")
    cat("setwd('..')\n", "rmarkdown::render(normalizePath('", dep$source, "'), quiet=TRUE)",
        file=wrapper, sep='')
    callr::r(function(x) source(x), args=list(wrapper))
  }
  if(!file.exists(dep$document)){
    stop("Unable to locate output of child document: ", dep$document)
  }
}


#' Load results from child documents
#'
#' @param deps List of objects of class \code{Dependency}
#' @param where Environment into which dependencies should be loaded.
#'
#' @return Called for its side effect.
#' @export
#' @importFrom rmarkdown render
#' @importFrom knitr knit_global
#' @author Peter Humburg
load_dependencies <- function(deps, where=knitr::knit_global()){
  for(d in deps){
    ## identify files to load
    chunks <- d$chunks
    if(!length(chunks)) next
    loaded <- knitr::opts_knit$get('loaded_chunks')
    chunks <- setdiff(chunks, loaded[[d$label]])
    if(length(chunks)){
      cache_pattern <- paste(paste0('^', chunks, '_'), collapse='|')
      cached <- dir(d$cache, pattern=cache_pattern, full.names=TRUE)
      cached <- unique(sub("\\.[^.]+$", "", cached))
      if(length(cached) < length(chunks)){
        found <- sapply(strsplit(cached, '_'), '[[', 1)
        if(length(found)){
          found_pattern <- paste(found, collapse='|')
          missing <- chunks[!grepl(found_pattern, chunks)]
        } else {
          missing <- chunks
        }
        stop("Unable to locate output for chunks ", paste(missing, collpse=', '), " from document ", d$source, ".")
      }
      lapply(cached, lazyLoad, where)
      loaded[[d$label]] <- c(loaded[[d$label]], chunks)
      knitr::opts_knit$set(loaded_chunks=loaded)
    }
  }
  invisible(NULL)
}

#' Copy dependencies to working directory
#'
#' Output of child documents is copied to the working directory of the main document and installs
#' a \code{dependencies} package option.
#'
#' @param deps List of dependencies.
#'
#' @return Called for its side effect.
#' @export
#'
#' @author Peter Humburg
copy_dependencies <- function(deps){
  for(d in deps){
    if(dirname(d$document) != getwd()){
      file.copy(d$document, getwd(), overwrite=TRUE)
      file.copy(d$files, getwd(), recursive=TRUE, overwrite=TRUE)
    }
  }
  invisible(NULL)
}

#' @importFrom knitr opts_knit
needs_update <- function(dependency, main_file){
  if(dependency$label %in% names(knitr::opts_knit$get('loaded_chunks'))) return(FALSE)
  update <- FALSE
  in_doc <- dependency$source
  out_doc <- dependency$document
  cache_dir <- dependency$cache
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
      deps <- params2deps(list(depends=deps$value))
      update <- any(sapply(deps, needs_update, in_doc))
      if(!update){
        dep_times <- file.mtime(sapply(deps, '[[', 'source'))
        my_time <- file.mtime(dependency_output(main_file))
        update <- any(dep_times > my_time) && !is.na(my_time)
      }
    }
  }
  update
}
