#' Replace null values with default
#'
#' @param x Variable to be tested.
#' @param y Default to use if \code{x} is \code{NULL}.
#' @return If \code{x} is \code{NULL}, \code{y} is returned, otherwise \code{x}.
#' @name null-or
#' @author Peter Humburg
#' @export
`%||%` <- function(x, y){
  if(is.null(x)) y else x
}

#' Merge two named lists
#'
#' @param x Base list
#' @param y Additional entries to be merged into \code{x}
#'
#' @note Entries in \code{x} that are also present in \code{y}
#' will use the values from \code{y}.
#' @return A list with entries from \code{x} and \code{y}.
#' @export
merge_list <- function(x,y){
  if(!is.null(y) && length(y)){
    exclude <- names(y) %in% names(x) & sapply(y, is.null)
    y <- y[!exclude]
    x[names(y)] <- y
  }
  x
}

#' Processing YAML metadata
#'
#' @param input Name of input file.
#' @param ... Additional arguments are ignored.

#' @importFrom stringr str_extract_all
#' @importFrom stringr str_replace
#' @importFrom stringr regex
#' @importFrom yaml yaml.load
#'
#' @return \code{extract_yaml} returns a named list with the
#' contents of the yaml metadata block.
#' @author Peter Humburg
extract_yaml <- function(input, ...){
  contents <- readLines(input, ...)
  header <- stringr::str_match_all(paste(contents, collapse="\n"),
                                   stringr::regex("^---+\n(.+?)^(---+|\\.\\.\\.+)\n",
                                                  multiline=TRUE, dotall=TRUE))[[1]][,2]
  header <- lapply(header, stringr::str_replace_all, '((^---+$)|((\\.\\.\\.+$)|(---+$)))', '')
  header <- lapply(header, yaml::yaml.load)
  header <- Reduce(merge_list, header)
  header <- eval_yaml(header)
  header
}

#' Evaluate R code in YAML fields
#'
#' @param x YAML meta data to process.
#' @param data Data to use when evaluating R expressions.
#' @param ... Further arguments, currently ignored.
#'
#' @return A list of the same shape as \code{x} with all inline R code chunks evaluated.
eval_yaml <- function(x, data, ...){
  if(missing(data)) data <- x
  if(is.list(x)){
    lapply(x, eval_yaml, data, ...)
  } else if(stringr::str_detect(x, "`r ([^`]+)`")){
    code <- stringr::str_match(x, "`r ([^`]+)`")[,2]
    with(data, eval(parse(text=code)))
  } else{
    x
  }
}

#' Extract title from a Markdown document's yaml metadata
#'
#' @return The \code{rmd_*} functions return character vector of
#' length one containing the value of the corresponding field.
#' @rdname extract_yaml
rmd_title <- function(input, ...){
  metadata <- extract_yaml(input, ...)
  metadata$title
}

#' @details \code{rmd_short_title} tries to infer
#' a suitable short title for a document if it isn't
#' provided explicitly in the header by using the part
#' of the title preceding the first punctuation mark
#' (if any).
#' @rdname extract_yaml
rmd_short_title <- function(input, ...){
  metadata <- extract_yaml(input, ...)
  ans <- metadata$short_title
  if(is.null(ans)){
    ans <- metadata$title
    ans <- sub('(^[^.:;!?]+).*', '\\1', ans)
  }
  ans
}

#' Extract title from an HTML document
#' @param input Name of input file.
#' @param ... Additional arguments are ignored.
#' @importFrom xml2 read_html
#' @importFrom xml2 xml_find_one
#' @importFrom xml2 xml_text
#'
#' @author Peter Humburg
html_title <- function(input, ...){
  contents <- xml2::read_html(input, ...)
  title <- xml2::xml_find_one(contents, '//title')
  xml2::xml_text(title)
}

index <- function(label, origin, name, caption, type=c('figure', 'table')){
  type <- match.arg(type)
  if(!is.null(origin)){
    origin <- strsplit(basename(origin), '.', fixed=TRUE)[[1]]
    origin <- paste(origin[-length(origin)], sep='.')
  } else {
    origin <- ''
  }
  idx <- opts_knit$get('reportmd.index')
  idx[[type]] <- rbind(idx[[type]], c(label, origin, name, caption))
  opts_knit$set(reportmd.index=idx)
}

#' @importFrom utils write.table
write_index <- function(index, type){
  if(nrow(index)){
    input <- sub('(.*)\\.[^.]+$','\\1', knitr::current_input())
    index_name <- paste0(input, '_', type, '.idx')
    write.table(index, file=index_name, sep="\t", row.names=FALSE, col.names=FALSE)
  }
}

#' @importFrom utils read.table
read_index <- function(file){
  if(file.exists(file)){
    read.table(file, header=FALSE)
  }
}

get_index <- function(target, type=c('figure', 'table')){
  type <- match.arg(type)
  if(is.character(target$index[[type]])){
    target$index[[type]] <- read_index(target$index[[type]])
  }
  target$index[[type]]
}

# there are 12 columns, odd numbers cannot be centered in the columns
calc_offset <- function(size) {
  res <- strsplit(size, "-")[[1]]
  num_size <- as.numeric(res[3])
  offset <- paste0(paste0(res[1:2], collapse = "-"), "-offset-", (12 - (num_size + (num_size %% 2))) / 2)
  offset
}

tags <- list(
  a = function(...) tag("a", list(...)),
  b = function(...) tag("b", list(...)),
  button = function(...) tag("button", list(...)),
  caption = function(...) tag("p", c(class='caption', list(...))),
  code = function(...) tag("code", list(...), add_newline = FALSE),
  div = function(...) tag("div", list(...)),
  img = function(...) tag("img", list(...)),
  li = function(...) tag("li", list(...)),
  p = function(...) tag("p", list(...)),
  pre = function(...) tag("pre", list(...)),
  span = function(...) tag("span", list(...)),
  ul = function(...) tag("ul", list(...)),
  h5 = function(...) tag('h5', list(...)),
  link = function(...) tag('link', list(...)),
  title = function(...) tag('title', list(...))
)

tag <- function(type, arg_list, add_newline = TRUE){
  named_idx = nzchar(names(arg_list))
  named_idx = if(length(named_idx) == 0) FALSE else named_idx

  newline = if (add_newline) { "\n" } else { "" }
  paste0('<', type, ' ', print_attributes(arg_list[named_idx]), '>',
         paste0(unlist(arg_list[!named_idx]), collapse='\n'), '</', type,'>', newline)
}


generate_attribute <- function(index, data) {
  paste0(names(data)[index], '="', paste0(data[[index]], collapse=' '), '"')
}

print_attributes <- function(attributes) {
  paste0(vapply(seq_along(attributes), FUN.VALUE=character(1),
                FUN=generate_attribute, USE.NAMES=FALSE, attributes), collapse= ' ')

}

check_pandoc_args <- function(args){
  if(!"--wrap" %in% args){
    args <- c(args, "--wrap=none")
  }
  if(!any(grepl("mathjax-url:", args))){
    args <- c(args, "--variable", "mathjax-url:https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
  }
  args
}

add_anchor <- function(label, prefix, named_only=TRUE){
  if(!grepl("^unnamed-chunk", label) || !named_only){
    if(!missing(prefix)){
      label <- paste0(prefix, label)
    }
  } else{
    label <- ""
  }
  label
}

fix_exponent <- function(x){
  x <- stringr::str_replace(x, stringr::fixed('e'), '\\times 10^{')
  x <- paste0('$', x, '}$')
  x <- stringr::str_replace(x, '\\{\\+?(-?)0*(\\d+)', '{\\1\\2')
  x <- stringr::str_replace(x, stringr::fixed('$1\\times '), '$')
  x
}

#' @importFrom stringr str_match
parse_md_link <- function(x){
  non_empty <- x != ''
  ans <- data.frame(text=NA, link=NA,
                    label=character(length(x)), stringsAsFactors=FALSE)
  if(any(non_empty)){
    match <- stringr::str_match(x[non_empty], '\\[(.+)\\]\\((.+)\\)')
    invalid <- is.na(match[,1])
    if(any(invalid)){
      if(sum(invalid) < 0){
        warning("Failed to parse links: ", paste(x[invalid], collapse=', '))
      } else {
        warning("Failed to parse ", sum(invalid), " links. First failure was ", x[invalid][1])
      }
    }
    ans[non_empty, ] <- cbind(text=match[,2], link=match[,3], label=match[,2])
  }
  ans
}

#' @importFrom stringr str_match
parse_html_link <- function(x){
  non_empty <- x != ''
  ans <- data.frame(text=character(length(x)), link=character(length(x)),
                    label=character(length(x)), stringsAsFactors=FALSE)
  if(any(non_empty)){
    match <- stringr::str_match(x, '<a.*href\\s*=\\s*(\\S+).*>\\s*(.+)\\s*</a\\s*>')
    invalid <- is.na(match[,1])
    if(any(invalid)){
      if(sum(invalid) < 0){
        warning("Failed to parse links: ", paste(x[invalid], collapse=', '))
      } else {
        warning("Failed to parse ", sum(invalid), " links. First failure was ", x[invalid][1])
      }
    }
    ans[non_empty, ] <- cbind(text=match[,2], link=match[,3], label=match[,2])
  }
  ans
}

#' @importFrom stringr str_replace
make_label <- function(x){
  x <- stringr::str_replace(x, '&[^;]*;', '')
  make.names(x)
}

#' Convert links in a table column to reference links
#'
#' @param data A \code{data.frame} or an object that can be coerced to one.
#' @param links Numerical vector of column indices indicating columns that
#' contain links in need of conversion.
#' @param format Format used to specify links in the input.
#'
#' @details It is assumed that each entry consists either of a single link
#' or doesn't contain any link. If a link is identified the resulting reference
#' link will replace the entire entry. Entries without links will be left unchanged.
#'
#' Regardles of the input format of the links, the output will always
#' have markdown reference links.
#'
#' @return A \code{data.frame} with links in the indicated columns replaced
#' with reference links.
#' @export
#' @author Peter Humburg
make_ref_links <- function(data, links=1, format=c('markdown', 'html')){
  format <- match.arg(format)
  if(!is.data.frame(data)){
    data <- as.data.frame(data)
  }
  parsed_links <- lapply(links, function(i){
    switch(format,
           markdown=parse_md_link(data[[i]]),
           html=parse_html_link(data[[i]]))
  })
  refs <- knitr::opts_knit$get('.ref_links')

  ## ensure labels are unique
  for(i in 1:length(parsed_links)){
    for(j in 1:nrow(parsed_links[[i]])){
      label <- parsed_links[[i]][j, 'label']
      link <- parsed_links[[i]][j, 'link']
      text <- parsed_links[[i]][j, 'text']
      if(!is.na(link) && nchar(label)){
        if(label %in% names(refs)){
          if(parsed_links[[i]][j, 'link'] != refs[[label]]){
            parsed_links[[i]][j, 'label'] <- label <- make.unique(c(names(refs), make_label(label)))[length(refs)+1]
          }
        }
        refs[[label]] <- link
      }
    }
    update <- !is.na(parsed_links[[i]][['link']])
    update_simple <- update & parsed_links[[i]][update, 'text'] == parsed_links[[i]][update, 'label']
    update_full <- update & !update_simple
    if(any(update_simple))
      data[update_simple, links[i]] <- paste0('[', parsed_links[[i]][update_simple, 'text'], ']')
    if(any(update_full))
      data[update_full, links[i]] <- paste0('[', parsed_links[[i]][update_full, 'text'], '][', parsed_links[[i]][update_full, 'label'], ']')
  }
  data
}

ref_links <- function(){
  refs <- knitr::opts_knit$get('.ref_links')
  ans <- character(length(refs))
  if(!length(ans)) return(ans)
  for(i in 1:length(refs)){
    label <- names(refs)[i]
    ans[i] <- paste0('[', label, ']: ', refs[[label]])
  }
  paste('\n', paste(ans, collapse='\n'), '\n')
}

#' @importFrom stringr str_detect
valid_size <- function(size, size_class='md'){
  if(!stringr::str_detect(size,'^col')){
    if(as.integer(size) < 1 || as.integer(size) > 12){
      stop(deparse(substitute(size)), ' should be between 1 and 12')
    }
    if(!is.null(size_class) && !is.na(size_class) && size_class != ''){
      size <- paste('col', size_class, size, sep='-')
    } else{
      size <- paste('col', size, sep='-')
    }
  }
  size
}
