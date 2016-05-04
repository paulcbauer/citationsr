#' Identifies meta information within txt documents.
#'
#' @param filename Input file; assumed to be txt format..
#' @param encoding Character encoding scheme of input txt file. Default is "utf-8".
#' @param lines.import Number of lines that should be imported from the input txt file. Default is 2000.
#' @param rcrossref Use rcrossref function to identify article meta data via CrossRef API? Default is TRUE. Currently, no useful other ways of identifying metadata are implemented.
#' @param bibtex Should reference be exported as bibtex? Works only with rcrossref = TRUE. Default is FALSE.
#' @param vars Which variables should be returned? Specify as character vectors. By default, all available variables are returned.
#'
#'

get_metadata_doc <- function(files, .fun = get_metadata_doc_nv, .progress = "text", .parallel = FALSE, ...) {
  require(plyr)
  out <- adply(files, .margins = 1, .fun = .fun, .progress = .progress, .parallel = .parallel, ...)
  return (out)
}  



