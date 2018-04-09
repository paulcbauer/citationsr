#' Extract citation cases.
#'
#' @return Returns and saves data (.csv and .html) containing citation cases in current working directory.
#' @param from Name of folder within working directory in which the citing documents (.txt files) are located, e.g. "Beck 1995".
#' @param to Folder in which citation case data is stored.
#' @param authorname Enter the names of all authors separated by comma.
#' @param studyyear Enter the year when the study was published.
#' @param scope Enter number of sentences before/after citation case sentence to extract.
#' @param clean If TRUE (default) it cleanes some stuff before the citation case
#'
#'
#' @examples
#' \dontrun{
#'  setwd("C:/Users/paul/Google Drive/Research/2016_Quality_of_citations/data")
#'  folder <- "docs"
#'  study.title <- "The colonial origins of comparative development: An empirical investigation"
#'  authorname <- "Acemoglu, Johnson, Robinson"
#'  studyyear <- 2001
#'  extract_citation_cases(folder, authorname, studyyear, clean = TRUE) #
#' }

extract_citation_cases <- function(from, to, authorname, studyyear, scope=NULL, number=NULL, clean=NULL, encoding = "ASCII"){

    require(stringr)
    require(readr)


# Generate search terms depending on number of authors and year ####
    authorname <- unlist(stringr::str_split(authorname, ","))
    authorname <- gsub(" ", "", authorname)
    length.authorname <- length(authorname)

    # Single author  ####
    if(length.authorname==1){
      searchterms <- paste(authorname[1], "(|\\'s|\\’s|\\’|\\')(|,)\\s{0,2}(|\\[|\\()" ,studyyear, "", sep="")
    }

    # Two authors  ####
    if(length.authorname==2){
      searchterms <- paste(authorname[1], " (\\&|and) ", authorname[2], "(|\\'s|\\’s|\\’|\\')(|,)\\s{0,2}(|\\[|\\()" ,studyyear, "(\\s{0,2}(:|,)(?# Komma oder Doppelpunkt)\\s{0,2}(PAGE|)(?# Page kommt vor oder nicht)(\\s{0,2}|)(?# nochmal space oder nicht)\\d*(?# zahl mit länge 0 oder mehr)(\\]|\\)|)(?# schliesst mit versch klammer oder nicht)|)(?# seitenzahlen ja,nein, falls nein einfach klammer matchen)(\\]|\\)|)", sep="")
      # paste(authorname[1], " and ", authorname[2], sep=""), # Without year!
    }

    # Three authors  ####
    if(length.authorname==3){
      searchterms <- c(paste(authorname[1], ", ", authorname[2], ", & ", authorname[3], " ", studyyear, sep=""),
               paste(authorname[1], ", ", authorname[2], ", & ", authorname[3], ", ", studyyear, sep=""),
               paste(authorname[1], ", ", authorname[2], ", and ", authorname[3], ", ", studyyear, sep=""),
               paste(authorname[1], ", ", authorname[2], ", and ", authorname[3], " ", studyyear, sep=""),
               paste(authorname[1], ", ", authorname[2], ", and ", authorname[3], " ", "\\(" ,studyyear, "\\)", sep=""),

               paste(authorname[1], " AND OTHERS, ",studyyear, sep=""),
               paste(authorname[1], " AND OTHERS ",studyyear, sep=""),
               paste(authorname[1], " AND OTHERS (" ,studyyear, ")", sep=""),
               paste(authorname[1], " AND OTHERS, (" ,studyyear, ")", sep="")
      )

    }

    # More than three authors  ####
    if(length.authorname>3){
      searchterms <- c(
        paste(authorname[1], " AND OTHERS", "(|,)\\s{0,2}(|\\[|\\()" ,studyyear, "(\\s{0,2}(:|,)(?# Komma oder Doppelpunkt)\\s{0,2}(PAGE|)(?# Page kommt vor oder nicht)(\\s{0,2}|)(?# nochmal space oder nicht)\\d*(?# zahl mit länge 0 oder mehr)(\\]|\\)|)(?# schliesst mit versch klammer oder nicht)|)(?# seitenzahlen ja,nein, falls nein einfach klammer matchen)(\\]|\\)|)", sep="")

      )

    }







# List file names in folder (ONLY .TXT FILES) ####
  myfiles <- dir(from, pattern = "processed.txt", full.names = TRUE)
  myfiles.names <- dir(from, pattern = "processed.txt", full.names = FALSE)
# Count number of files in folder ####
  n.docs <- length(myfiles)

# Specify number of documents to assess by setting n.docs ####
  if(!is.null(number)){n.docs <- number}




# Generate regex for search terms and dependency on scope ####
    searchterms <- paste("\\.[^.]*", searchterms, "[^.]*\\.", sep = "")

    # Change the regex if scope is broader, i.e. if sentence before and after should be
      # extracted
    if(!is.null(scope)){

      searchterms <- paste(paste(rep("\\.[^.]*", scope), collapse=""),
                           searchterms,
                           paste(rep("[^.]*\\.", scope), collapse=""),
                           sep = "")
    }



# Loop over .txt files one by one (until document nr. "number" = n.docs) ####
  all.docs.cit.cases <- NULL
  for (i in 1:n.docs){

    # Read in files
    con <- file(myfiles[i], encoding = encoding)
    x <- readLines(con)
    close(con)

    # Extract sentences/lines that contain searchterms
    cit.cases.doc.i <- stringr::str_extract_all(x, paste(searchterms, collapse="|"))

    # Write them to list
    all.docs.cit.cases[i] <- cit.cases.doc.i

    # counter
    if(stringr::str_detect(as.character(i), "^.*0$")){cat(i, ".. ", sep="")}

  }




# Get first estimate of number of citation cases ####
  total.citation.cases <- sum(sapply(all.docs.cit.cases, length))

# Generate dataframe with citation cases ####
  citation.data <- data.frame(document = 1:total.citation.cases, citation.case = 1:total.citation.cases)

# Take names from citing document file names ####
  citation.data[,1] <- rep(myfiles.names, sapply(all.docs.cit.cases[1:length(myfiles.names)], length))
  # does it work with filenames here?
  citation.data[,2] <- unlist(all.docs.cit.cases)

# Clean citation cases ####
  if(!is.null(clean)){
  citation.data$citation.case <- stringr::str_replace_all(citation.data$citation.case, "^\\.FOOTNOTE[0-9]{1,3}\\s", "^\\.\\s")
  citation.data$citation.case <- stringr::str_replace_all(citation.data$citation.case, "^\\.\\s|^\\.\\?\\s|^\\.!\\s|^\\.\\s?[0-9]{1,3}\\s?", "")
  }

# Add year variable ####
  citation.data$year <- as.numeric(str_extract(citation.data$document, '\\s([1-2]{1}[0-9]{3})'))

# Add document number variable ####
  citation.data$doc.nr <- as.numeric(sub(".*?doc(.*?)\\s.*", "\\1", citation.data$document))


# Change document names in citation cases data frame ####
  # citation.data$document <- sub("\\s-$", "", stringr::str_extract(citation.data$document, "^.*-.*\\s-"))

# Add ncar of citation case
  citation.data$nchar.citation.case <- nchar(citation.data$citation.case)

# Save citation cases in table (csv and html) ####
  write_csv(citation.data, path =  paste(to, "/", paste(authorname, collapse=""), "_", studyyear, "_citation_cases.csv", sep = ""))
  print(xtable::xtable(citation.data),type='html',comment=FALSE, file=paste(to, "/", paste(authorname, collapse=""), "_", studyyear, "_citation_cases.html", sep = ""))

  # Message to user
  cat("\n\n For ", authorname, " we have identified ", total.citation.cases, " citation cases within ", n.docs, " documents. They are printed and saved as files in the working directory: '*_citation_cases.csv' and '*_citation_cases.html'.\n\n", sep="")

  }
