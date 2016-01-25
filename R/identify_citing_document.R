#' Identifies citation information in documents.
#'
#' @param folder The folder in which the citing documents are located.
#' @return Returns a both a .csv file as well as a .html file that contain a
#' list with the titles that were extracted from all the citing documents.


# STEPS
# 1. Read in all texts in folder
# 2. Identify doi in texts
# 3. If there is doi.. 3a.. if there is no doi 3b 
    # 3a. Use rcrossref to query all the info
    # 3b. Try to identify some other info in text (e.g. full line with journal title volume number) that
      # can be queried using cross ref

identify_citing_document <- function(folder, number=NULL){

# List file names
  file.names <- dir(paste("./", folder, sep = ""), pattern = ".txt")
  file.paths <- paste(paste("./", folder, "/", sep = ""), file.names, sep="")
  n.docs <- length(file.paths)

# Specify number of documents to assess
  if(!is.null(number)){n.docs <- number}


# Load documents and search for full citation in them
  doc.information <- NULL
  for (i in 1:n.docs){
    
    # Open file
    con <- file(file.paths[i], encoding = "UTF-8")
    x <- readLines(con, warn = F, n = 200)
    close(con)
  
  # Reste containers
    full.line <- NA
    doi <- NA
    
  # Find doi, save in object "doi" and query info from crossref using doi
    doi <- stringr::str_extract(x, "(DOI:?|doi:?)\\s{0,2}[0-9\\.\\-a-zA-Z]*/[0-9\\.\\-a-zA-Z]*(/[0-9\\.\\-a-zA-Z]*)?|http://dx\\.doi\\.org/[0-9\\.\\-a-zA-Z]*/[0-9\\.\\-a-zA-Z/]*|^10\\.[0-9]{2,6}/[0-9\\.\\-a-zA-Z/]*")
    doi <- doi[!is.na(doi)]
    if(length(doi)!=0){
    doi <- stringr::str_replace(doi, "^DOI |^DOI: |DOI:|^doi |^doi: |doi:|http://dx\\.doi\\.org/", "")
    doi <- doi[1]
    print(doi)
    # get information with doi
    crossref.info <- cr_cn(dois = doi, format = "text", style = "apa")
    print(crossref.info)
    }


  
  # IF NO DOI.. FIND FULL LINE AND SEARCH For that in rcrosssref
      if(length(doi)==0){
        
      # detect full line with information  
      detected <- stringr::str_detect(x, paste("(?=.*Vol\\.)(?=.*No\\.).*|(?=.*Volume)(?=.*Issue).*", sep=""))
      full.line <- x[detected]
      print(full.line)
      
      # Use full line to search for article
      if(length(full.line)!=0){
      doi.2 <- rcrossref::cr_works(query=full.line, limit = 1)$data$DOI
      crossref.info <- cr_cn(dois = doi.2, format = "text", style = "apa")
      }
      if(length(full.line)==0){print("no full line with info")}
      }
    

  # SAVE INFORMATION IN LIST
  doc.information[[i]] <- c(doi, crossref.info)

  # Counter
  if(stringr::str_detect(as.character(i), "[0-9]*0")){cat(i, ".. ", sep="")}

  }
  print(doc.information)
}







# IDEAS...

  # JOURNAL TITLE
  # journal.titles <- c("American Political Science Review|International Organization")
  # journal.titles <- paste(journals$Title, collapse = "|")
  # matched <- stringr::str_match(x, journal.titles)
  # cd.journal <- matched[!is.na(matched)][1]
  # if(is.na(cd.journal)){cd.journal <- "no title"}
  # print(cd.journal)

# YEAR
  # Get year from the authors
    # year <- stringr::str_extract("ª The Author(s) 2011", "(?=The Author\\(s\\)).*")
    # year  <- stringr::str_extract(year, "[0-9]{4}")


# Search with part of title
    # x <- cr_works(query="ARE NICHE PARTIES DIFFERENT?", limit = 1)
    # x$data$title

# VOLUME, NUMBER, PAGES
  # detected <- stringr::str_detect(x, journal.titles)
  # cit.info <- x[detected]
  # cit.info <- str_extract(cit.info, paste(citing.document.journal.title, ".{0,5}Volume\\s{0,2}[0-9]{0,4}.{0,5}Issue\\s{0,2}[0-9]{0,4}.{0,5}(", paste(month.name, collapse="|"),")\\s{0,2}[0-9]{4}.{0,5}[0-9]{1,4}.{0,5}[0-9]{1,4}", sep=""))
  # cit.info <- cit.info[!is.na(cit.info)]

  # volume <- str_extract(cit.info, "Volume\\s{0,2}[0-9]{1,4}")
  # issue <- str_extract(cit.info, "Issue\\s{0,2}[0-9]{1,4}")
  # pages <- str_extract(cit.info, "pp.{0,2}[0-9]{1,4}.{1,3}[0-9]{1,4}")
  # year <- str_extract(cit.info, paste("(", paste(month.name, collapse="|"),")\\s{0,3}[0-9]{4}", sep=""))


  # Another way with lookahead
   # detected <- stringr::str_match(x, "(?=Issue)(?=Volume)") # FIND the right connection
   # cit.info <- x[detected]
   # citing.document.journal.title <- matched[!is.na(matched)][1]
  # American Journal of Political Science, Vol. 55, No. 2, April 2011, Pp. 370–382

  # W.G. Kennedy, N. Agarwal, and S.J. Yang (Eds.): SBP 2014, LNCS 8393, pp. 108–115, 2014.



# if(length(full.line)==0){full.line <- "no full line"}
# print(full.line)
# (?=.*(", paste(month.name, collapse="|"), "))
#

# Use journal names for matching
    # journals <- read.csv("./identify_citation_document/new table.csv", sep=";", header=T)
    # journals$Title <- as.character(journals$Title)







