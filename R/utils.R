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

