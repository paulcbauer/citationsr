#' Scrape citations from the web of knowledge.
#'
#' @param wos.url Follow the steps below to get the url.
#' on the first citation of the study.
#' @param n.pages Number of pages you would like to scrape.
#' @param folder Name the folder in which citations are saved.
#' @return Returns a file 'citations.csv' with all the scraped citations of that particular study.
#' #' @section Steps to follow:
#' 1. Search for study in WOS.
#'
#' 2. Click on the "times cited" link on the right.
#'
#' 3. Click on the first result in the list.
#'
#' 4. Copy the link out of the adress line in the browser and insert that into the function.

#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# Url: "http://apps.webofknowledge.com/full_record.do?product=UA&search_mode=CitingArticles&qid=3&SID=Z23xuqBbwEKhrQTYEwM&page=1&doc=1"


get_wos_citations <- function(wos.url, n.pages, folder) {
    URL <- wos.url
    cat("\nYou are using the following URL: ", paste(URL, sep = ""))

    # Get the first page
    doc <- htmlParse(URL) # , encoding="UTF-8" - ATTENTION UTF does not work on WINDOWS 7

    # Get max number of citations
    records.max <- stringr::str_replace(XML::xpathSApply(doc, "//*[@id='paginationForm']", fun = XML::xmlValue), "1", "")[1]
    records.max <- stringr::str_extract(records.max, "[0-9,]+")
    records.max <- stringr::str_replace(records.max, ",", "")


    # Generate URLs of the single records/pages
    all.URLs <- paste(substr(URL, 1, nchar(URL)-1), 1:records.max, sep="")


    # Decide how many pages "n.pages" to scrape
    URLs <- all.URLs[1:n.pages]
    cat("\n\n You decided to scrape ", n.pages, " out of ", records.max, " records/pages.", sep = "")


    # Loop through pages
    ptm <- proc.time()
    citing.articles.wos <- NULL
    for (i in 1:n.pages){

      x <- URLs[i] # Make a loop here!
      doc <- XML::htmlParse(x) # , encoding="UTF-8"

      # Title
      tit <- XML::xpathSApply(doc, "//div[@class='title']", fun = XML::xmlValue)
      tit <- stringr::str_replace_all(tit, "\n\"", "")
      tit <- stringr::str_replace_all(tit, "\n", "")
      tit <- stringr::str_replace_all(tit, "\"", "")

      # Authors
      authors <- XML::xpathSApply(doc, "//p[@class='FR_field']", fun = XML::xmlValue)[1]
      authors <- as.character(stringr::str_extract_all(authors, "\\(.*\\)"))
      authors <- stringr::str_replace_all(authors, "\\)|\\(", "")
      authors <- stringr::str_replace_all(authors, "\", \"", "; ")
      authors <- stringr::str_replace_all(authors, "c\"", "")
      authors <- stringr::str_replace_all(authors, "\"", "")


      # Journal
      journals <- XML::xpathSApply(doc, "//p[@class='sourceTitle']", fun = XML::xmlValue)[1]
      journals <- stringr::str_replace_all(journals, "\n", "")
      journals <- tolower(journals)


      # Journalinfo
      journal.info <- XML::xpathSApply(doc, "//div[@class='block-record-info-source-values']", fun = XML::xmlValue)
      journal.info <- stringr::str_replace_all(journal.info, "\n", " ")
      volume <- stringr::str_replace(stringr::str_extract(journal.info, "Volume: [0-9]+"), "Volume: ", "")
      issue <- stringr::str_replace(stringr::str_extract(journal.info, "Issue: [0-9]+"), "Issue: ", "")
      pages <- stringr::str_replace(stringr::str_extract(journal.info, "Pages: [0-9]+-[0-9]+"), "Pages: ", "")


      # DOI
      doi <- stringr::str_subset(XML::xpathSApply(doc, "//p[@class='FR_field']", fun = XML::xmlValue), "DOI:")
      doi <- stringr::str_replace_all(doi, "\n", " ")
      doi <- stringr::str_replace(stringr::str_extract(doi, "DOI: .+"), "DOI: ", "")


      # Year
      year <- stringr::str_subset(XML::xpathSApply(doc, "//p[@class='FR_field']", fun = XML::xmlValue), "Published:")
      year <- stringr::str_replace_all(year, "\n", " ")
      year <- stringr::str_extract(year, "[0-9][0-9][0-9][0-9]")


      # Bind together to a charactervector
      dat2 <- c(tit, authors, journals, volume, issue, pages, doi, year)

      citing.articles.wos <- rbind(citing.articles.wos, dat2)
    }
    time <- proc.time() - ptm
    cat("\n\n Scraping took around ", as.numeric(time[3]), " second(s) this is around ", round(as.numeric(time[3])/60,2), " minutes.\n", sep = "")


    citing.articles.wos <- data.frame(citing.articles.wos)
    names(citing.articles.wos) <- c("title", "author", "journal", "volume", "issue", "pages", "doi", "year")


    # generate identifier
    citing.articles.wos$identifier <- paste("id", stringr::str_replace_all(as.character(citing.articles.wos$doi), "\\.|/|-", ""), sep="")

    # Write data into a file

    # Create directory for study
    dir.create(folder)
    # dir.create(paste(stringr::str_extract(studies$authors, "[^,]*"), studies$year, sep = " "))

    # Generate path to save
    # savepath <- paste(paste(stringr::str_extract(studies$authors, "[^,]*"), studies$year, sep = " "), "/citations.csv", sep="")
    savepath <- paste(folder, "/citations.csv", sep="")
    # Save into path
    write.table(citing.articles.wos, savepath)
    working.directory <- getwd()
    cat("\nThe citations have been saved in the file 'citations.csv' in your working directory: \n\n", working.directory, sep="")

}


