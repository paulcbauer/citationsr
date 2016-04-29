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
#' @examples
#' \dontrun{
#'  setwd("C:/Users/pbauer/Downloads")
#'  folder <- "docs"
#'  files <- list.files(folder, pattern = "txt$", full.names = T)
#'  foo <- plyr::adply(files[1:length(files)], .margins = 1, .fun = get_metadata_doc, .progress = "text")
#'  # Replace NULL values in metadata tables
#'  for(z in 1:nrow(foo)){for(y in 1:length(foo)){if(is.null(foo[z,y][[1]])){foo[z,y][[1]] <- NA}}}
#'  # save metadatatables in data folder
#'  save(foo, file = paste("./", folder, "_metadata.RData", sep=""))
#' }


get_metadata_doc <- function(filename, encoding = "UTF-8", lines.import = 2000, rcrossref = TRUE, bibtex = FALSE, vars = NULL){

  # load packages
  require(magrittr)
  require(stringr)
  require(dplyr)

  # open file
  con <- file(filename, encoding = encoding)

  # import text lines; lines.import specifies number of lines
  x <- readLines(con, warn = F, n = lines.import)
  close(con) # close connection

  # remove form feed, line feed, carriage returns and empty lines from doc
  x <- str_replace_all(x, "[\r\n\f]" , "")
  x <- str_replace_all(x, "^[Dd]ownloaded from.+" , "")
  x <- str_replace_all(x, "ﬁ", "fi")
  x <- x[sapply(x, nchar) > 0]

  # generate empty containers
  doc_name <- basename(filename)
  doc_size <- file.size(filename)
  full_line <- NA
  doi <- NA
  meta_source <- NA
  doi_guess <- NA
  meta_dat <- data.frame(author = NA, title = NA, container.title = NA, volume = NA, issue = NA, created = NA, issued = NA, page = NA, publisher = NA, subject = NA, type = NA, URL = NA, DOI = NA, ISSN = NA, link = NA, reference.count = NA, score = NA, source = NA)

  # identify DOI
  doi <- stringr::str_extract(x, '\\b(10[.][0-9]{4,}(?:[.][0-9]+)*/(?:(?!["&\'])\\S)+)\\b') # taken from http://stackoverflow.com/questions/27910/finding-a-doi-in-a-document-or-page
  doi <- doi[!is.na(doi)]

  # check if DOI contains non-ASCII characters
  check_nonascii <- tools::showNonASCII(doi)
  doi_nonascii <- ifelse(length(check_nonascii) > 0, TRUE, FALSE)

  # if no DOI available, try fetch it from CrossRef via information from first 20 lines
  # detect frequent lines (headers?)
  if(length(doi)==0){
    # significant_lines <- sort(table(x), decreasing = TRUE)[1:3] # header approach
    # significant_lines_combined <- paste(names(frequent_lines), collapse = " ")
    significant_lines_combined <- paste(x[1:20], collapse = " ") # first 20 lines approach
    doi <- rcrossref::cr_works(query=significant_lines_combined, limit = 1)$data$DOI
    doi_guess <- TRUE
  }

  # if DOI available, query info from CrossRef
  if(length(doi)!=0 & doi_nonascii == FALSE){
    # extract lines that contain identifiers of DOI
    doi <- stringr::str_replace(doi, "^DOI |^DOI: |DOI:|^doi |^doi: |doi:|http://dx\\.doi\\.org/", "")
    doi <- stringr::str_replace(doi, "_supp$|\\.supp$", "")
    # take the first one
    doi <- doi[1]
    if (rcrossref == TRUE) {
    # query crossref with DOI
    if(bibtex == TRUE) {
      crossref_bibtex <- rcrossref::cr_cn(dois = doi, format = "bibtex", style = "apa")
      write(crossref_bibtex, paste0(filename, ".bib"))
    }
    crossref_df <- try(rcrossref::cr_works(dois = doi) %>% .$data %>% dplyr::select(matches("^author$|^title$|^container.title$|^volume$|^issue$|^created$|^issued$|^page$|^publisher$|^subject$|^type$|^URL$|^DOI$|^ISSN$|^reference.count$|^score$|^source$")))
      if (class(crossref_df) != "try-error") {   # error handling if call caused an error
        meta_dat <- crossref_df
    }
    meta_source <- "CrossRef"
    }
  }

  # add empty variables if necessary
  if (!("author" %in% colnames(meta_dat))) { meta_dat$author <- NA }
  if (!("title" %in% colnames(meta_dat))) { meta_dat$title <- NA }
  if (!("container.title" %in% colnames(meta_dat))) { meta_dat$container.title <- NA }
  if (!("volume" %in% colnames(meta_dat))) { meta_dat$volume <- NA }
  if (!("issue" %in% colnames(meta_dat))) { meta_dat$issue <- NA }
  if (!("created" %in% colnames(meta_dat))) { meta_dat$created <- NA }
  if (!("issued" %in% colnames(meta_dat))) { meta_dat$issued <- NA }
  if (!("page" %in% colnames(meta_dat))) { meta_dat$page <- NA }
  if (!("publisher" %in% colnames(meta_dat))) { meta_dat$publisher <- NA }
  if (!("subject" %in% colnames(meta_dat))) { meta_dat$subject <- NA }
  if (!("type" %in% colnames(meta_dat))) { meta_dat$type <- NA }
  if (!("URL" %in% colnames(meta_dat))) { meta_dat$URL <- NA }
  if (!("DOI" %in% colnames(meta_dat))) { meta_dat$DOI <- NA }
  if (!("ISSN" %in% colnames(meta_dat))) { meta_dat$ISSN <- NA }
  if (!("reference.count" %in% colnames(meta_dat))) { meta_dat$reference.count <- NA }
  if (!("score" %in% colnames(meta_dat))) { meta_dat$score <- NA }
  if (!("source" %in% colnames(meta_dat))) { meta_dat$source <- NA }

  # attach additional information
  meta_dat$meta_source <- meta_source
  meta_dat$doc_name <- doc_name
  meta_dat$doc_size <- doc_size
  meta_dat$doi_guess <- doi_guess

  # clean variables
  meta_dat$journal <- meta_dat$container.title
  meta_dat$date1 <- meta_dat$created
  meta_dat$date2 <- meta_dat$issued
  meta_dat$doc_year <- meta_dat$doc_name %>% str_extract("[[:digit:]]{4}")
  meta_dat$doc_author <- meta_dat$doc_name %>% str_extract("^[[:alpha:]- .]+([[:digit:]]{4}){1}") %>% str_replace("[[:digit:]]{4}", "")

  # clean journal variable
  journals_num <- meta_dat$journal %>% str_split(",") %>% unlist() %>% length()
  if (journals_num > 1) {
    journal_names <- meta_dat$journal %>% str_split(",") %>% unlist()
    journal_which <- journal_names  %>% str_length() %>% which.max()
    meta_dat$journal <- journal_names[journal_which]
  }

  # add journal information from scimagojr
  meta_dat$ISSN2 <- meta_dat$ISSN %>% str_replace(".+,", "") %>% str_replace_all("-", "") %>% as.numeric() %>% as.character()
  meta_dat$ISSN <- meta_dat$ISSN %>% str_replace_all("-", "") %>% str_replace_all(".+,", "") %>% as.numeric() %>% as.character()
  journal_matched <- c(match(meta_dat$ISSN[1], journals_df$journal_issn), match(meta_dat$ISSN2[1], journals_df$journal_issn), match(meta_dat$journal[1], journals_df$journal_title)) %>% na.omit %>% extract(1)
  meta_dat <- merge(meta_dat[1,], journals_df[journal_matched,], all = TRUE)

  # finalize data frame

  if (is.null(vars)) {
    meta_dat <- dplyr::select(meta_dat, doc_year, doc_author, doc_name, doc_size, author, date1, date2, title, journal, volume, issue, page, publisher, subject, type, URL, DOI, ISSN, ISSN2, reference.count, source, doi_guess, journal_sjr, journal_hindex, journal_cites_doc, journal_ref_doc, journal_country)
  } else {
    meta_dat <- meta_dat[,vars]
  }

  # return data frame
    return(meta_dat)

}
