#' Count citations of study (or studies) in the web of knowledge.
#'
#' @param wos.url See steps below to get the respective url.
#' @return Returns the number of times a study has been cited in the web of knowledge.
#' #' @section Steps to follow:
#' 1. Search for study in WOS.
#'
#' 2. Click on times cited for the particular study you are interested in.
#'
#' 3. Click on the first result in the list.
#'
#' 4. Copy the link out of the adress line in the browser and insert that into the function.

count_wos_citations <- function(wos.url) {
  URL <- wos.url
  cat("\nYou are using the following URL: ", paste(URL, sep = ""))

  # Get the first page
  doc <- XML::htmlParse(URL) # , encoding="UTF-8" - ATTENTION UTF does not work on WINDOWS 7

  # Extract how many pages there are
  records.max <- stringr::str_replace(XML::xpathSApply(doc, "//*[@id='paginationForm']", fun = XML::xmlValue), "1", "")[1]
  records.max <- stringr::str_extract(records.max, "[0-9,]+")
  records.max <- stringr::str_replace(records.max, ",", "")


    # Goes on page of one record and extracts the number of records "X of XXX"
  cat("\n\n There are ", records.max, " records/pages to scrape.", sep = "")


}
