## functions to generate a pipeline description from Rmarkdown files

#' @importFrom yaml yaml.load
#' @importFrom stringr str_detect
front_matter <- function(lines, delim=c('---', '\\.\\.\\.')){
  pattern <- paste0('^(', paste(delim, collapse='|'), ')\\s*$')
  blanks <- stringr::str_detect(lines, '^\\s*$')
  lines <- lines[!blanks]
  boundaries <- grep(pattern, lines)
  if(length(boundaries) >= 2 && boundaries[1] == 1 &&
     boundaries[2] - boundaries[1] > 1){
    yaml::yaml.load(paste(lines[2:(boundaries[2]-1)], collapse='\n'))
  }
}

yaml2deps <- function(header, child_path='.'){
  depends <- header$output[['reportMD::multi_document']]$depends
  sources <- sapply(depends, function(x) file.path(child_path, x))
  mapply(Dependency, label=names(depends), source=sources, SIMPLIFY=FALSE)
}

get_deps <- function(document, ...){
  header <- front_matter(readLines(document))
  ans <- list()
  if(!is.null(header)) {
    ans <- yaml2deps(header, ...)
  }
  ans
}

cwl_stage <- function(document, depends){
  deps <- lapply(depends, function(d) paste0(d$label, '_out'))
  names(deps) <- NULL
  stage <- list(run='render.cwl', 'in'=list(rmd=paste0(document$label, '_in')))
  if(length(deps)){
    stage$`in`$dep <- deps
  }
  stage$out <- list('html')
  stage
}

#' @importFrom stringr str_replace
dependencies <- function(main_doc){
  main_label <- stringr::str_replace(basename(main_doc), "\\.[^.]+$", "")
  docs <- list(Dependency(label=main_label, source=main_doc))
  names(docs) <- main_label
  inputs <- list()
  inputs[[main_label]] <- get_deps(main_doc, child_path=dirname(main_doc))
  for(d in inputs[[main_label]]){
    if(!d$label %in% names(docs)){
      deps <- dependencies(d$source)
      docs <- merge_list(docs, deps$docs)
      inputs[[d$label]] <- deps$inputs[[1]]
    }
  }
  list(docs=docs, inputs=inputs)
}

#' Extract workflow description from Rmd files
#'
#' Parse the documents of a multi-part report to extract the dependencies
#' between them. This information is then encoded in a
#' \href{http://www.commonwl.org/v1.0/}{CWL} workflow that
#' can be used to generate the full report with a compatible workflow
#' management system (a comprehensive list is available
#' \href{https://github.com/common-workflow-language/common-workflow-language#implementations}{online}).
#' Through this mechanism it is possible to take advantage
#' of parallel processing of documents, where the structure of the report allows.
#'
#' @param main_doc Path to main document of the report.
#' @param prefix Prefix for generated CWL files.
#' @param root_dir Use file paths relative to this directory. Set to \code{'/'}
#'   to use absolute paths.
#'
#' @details By default all paths to input files, i.e. the Rmarkdown files
#' comprising the report, are reported relative to the current working directory.
#' A different root directory can be chosen by providing an appropriate (absolute)
#' path as \code{root_dir}. If absolute paths are desired in the output \code{root_dir}
#' can be set to \code{'/'} but note that this makes the resulting job parameters
#' less portable.
#'
#' @export
#' @author Peter Humburg
#' @importFrom tools file_path_sans_ext
#' @importFrom yaml as.yaml
#'
workflow <- function(main_doc,
                     prefix=file.path(root_dir, 'cwl', tools::file_path_sans_ext(basename(main_doc))),
                     root_dir=getwd()){

  ## ensure output directory exists
  if(is.null(root_dir)){
    root_dir <- '/'
  }
  out_dir <- dirname(prefix)
  if(!dir.exists(out_dir)){
    dir.create(out_dir)
  }
  if(!file.exists(file.path(out_dir, 'render.cwl'))){
    file.copy(system.file('cwl/render.cwl', package='reportMD'),
              file.path(out_dir, 'render.cwl'))
  }

  ## parse documents to extract dependencies
  docs <- dependencies(main_doc)
  labels <- names(docs$docs)
  stages <- mapply(cwl_stage, docs$docs, docs$input[labels], SIMPLIFY=FALSE)
  inputs <- lapply(docs$docs, function(x) 'File')
  names(inputs) <- paste(names(inputs), 'in', sep='_')
  outputs <- lapply(docs$docs, function(x)
        list(type='File', outputSource=paste(x$label, 'html', sep='/')))
  names(outputs) <- paste(names(outputs), 'out', sep='_')

  wf <- list(cwlVersion='v1.0', class='Workflow',
             requirements=list(list(class='MultipleInputFeatureRequirement')),
             inputs=inputs, outputs=outputs, steps=stages)
  params <- params(docs$docs, rel_path=dirname(prefix))
  write(yaml::as.yaml(wf), file=paste0(prefix, '.cwl'))
  write(yaml::as.yaml(params), file=paste0(prefix, '-job.yml'))
}

#' @importFrom R.utils getRelativePath
params <- function(documents, rel_path){
  if(missing(rel_path) || is.null(rel_path)){
    rel_path <- '/'
  }
  par <- lapply(documents, function(d){
    path <- d$source
    path <- R.utils::getRelativePath(pathname=path, relativeTo=rel_path)
    list(class='File', path=path)
  })
  names(par) <- paste0(names(par), '_in')
  par
}
