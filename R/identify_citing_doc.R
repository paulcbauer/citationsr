#' Identifies meta information within documents.
#'
#' @return Stores list with meta information identified within citin documents (.txt files).
#' @param folder Name of folder within working directory in which the citing documents (.txt files) are located, e.g. "Beck 1995".
#' @param number Number of .txt files in folder the function should be applied to. Default is "all .txt files in folder".




identify_citing_document <- function(folder, number=NULL){

# List file names in folder (ONLY .TXT FILES)
  file.names <- dir(paste("./", folder, sep = ""), pattern = ".txt")

# Generate file paths
  file.paths <- paste(paste("./", folder, "/", sep = ""), file.names, sep="")

# Count number of files in folder
  n.docs <- length(file.paths)

# Specify number of documents to assess by setting n.docs
  if(!is.null(number)){n.docs <- number}

# Loop over .txt files one by one (until document nr. "number" = n.docs)
  doc.information <- NULL
  for (i in 1:n.docs){ # 1:number specified in function

    # Open file
    con <- file(file.paths[i], encoding = "UTF-8")

    # Read first 200 lines (that's a lot of info is normally)
    x <- readLines(con, warn = F, n = 200) # 200 can be changed
    close(con) # close connection

    # Generate empty containers
    full.line <- NA
    doi <- NA

    # Find doi, save in object "doi" and query info from crossref using cr_cn()
    doi <- stringr::str_extract(x, "(DOI:?|doi:?)\\s{0,2}[0-9\\.\\-a-zA-Z]*/[0-9\\.\\-a-zA-Z]*(/[0-9\\.\\-a-zA-Z]*)?|http://dx\\.doi\\.org/[0-9\\.\\-a-zA-Z]*/[0-9\\.\\-a-zA-Z/]*|^10\\.[0-9]{2,6}/[0-9\\.\\-a-zA-Z/]*")
    doi <- doi[!is.na(doi)]
    if(length(doi)!=0){
    # Extract lines that contain identifiers of doi
    doi <- stringr::str_replace(doi, "^DOI |^DOI: |DOI:|^doi |^doi: |doi:|http://dx\\.doi\\.org/", "")
    # Take the first one
    doi <- doi[1]
    print(doi)
    # Query crossref with doi
    crossref.info <- rcrossref::cr_cn(dois = doi, format = "text", style = "apa")
    print(crossref.info)
    }



# IF NO DOI...
      # Find "full line" with a lot of information
         # full line refers to a particular line that contains a lot of info
      if(length(doi)==0){ # if no doi!

      # Detect "full line" with information
      detected <- stringr::str_detect(x, paste("(?=.*Vol\\.)(?=.*No\\.).*|(?=.*Volume)(?=.*Issue).*", sep=""))
      full.line <- x[detected]
      print(full.line)

      # Use extracted "full line" to query crossref
      if(length(full.line)!=0){
      doi.2 <- rcrossref::cr_works(query=full.line, limit = 1)$data$DOI
      crossref.info <- cr_cn(dois = doi.2, format = "text", style = "apa")
      }

      if(length(full.line)==0){print("no 'full line' with info")}
      }


  # Save information in list
  doc.information[[i]] <- c(doi, crossref.info)

  # Counter shown in the console
  if(stringr::str_detect(as.character(i), "[0-9]*0")){cat(i, ".. ", sep="")}

  }
  print(doc.information)
}







# OTHER IDEAS

# IDENTIFY JOURNAL TITLE
  # journal.titles <- c("American Political Science Review|International Organization")
  # journal.titles <- paste(journals$Title, collapse = "|")
  # matched <- stringr::str_match(x, journal.titles)
  # cd.journal <- matched[!is.na(matched)][1]
  # if(is.na(cd.journal)){cd.journal <- "no title"}
  # print(cd.journal)

# IDENTIFY YEAR
  # Get year from the authors
    # year <- stringr::str_extract("ª The Author(s) 2011", "(?=The Author\\(s\\)).*")
    # year  <- stringr::str_extract(year, "[0-9]{4}")


# Search with part of title
    # x <- cr_works(query="ARE NICHE PARTIES DIFFERENT?", limit = 1)
    # x$data$title

# IDENTIFY VOLUME, NUMBER, PAGES
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
