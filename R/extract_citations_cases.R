#' Extract citation cases from "cleaned_woref.txt" files.
#'
#' @return Returns and saves data on citation cases (citation_data.csv) in the working directory.
#' @param folder Name of folder within your working directory that contains the PDFs.
#' Looping over folder containing citing documents for different studies is possible.
#' @param authorname Enter the names of all authors separated by comma.
#' @param studyyear Enter the year when the study was published.



extract_citation_cases <- function(folder, authorname, studyyear, scope=NULL, number=NULL){


#############################################
### FUNCTION TO GENERATE THE SEARCH TERMS ###
#############################################
    authorname <- unlist(stringr::str_split(authorname, ","))
    authorname <- gsub(" ", "", authorname)
    length.authorname <- length(authorname)
    if(length.authorname==1){
      searchterms <- paste(authorname[1], "(|\\'s|\\’s|\\’|\\')(|,)\\s{0,2}(|\\[|\\()" ,studyyear, "", sep="")
    }

    # stringr::str_extract("sdflkjds Beck & Katz, 1995 sljsdf", searchterms)

    if(length.authorname==2){
      searchterms <- paste(authorname[1], " (\\&|and) ", authorname[2], "(|\\'s|\\’s|\\’|\\')(|,)\\s{0,2}(|\\[|\\()" ,studyyear, "(\\s{0,2}(:|,)(?# Komma oder Doppelpunkt)\\s{0,2}(PAGE|)(?# Page kommt vor oder nicht)(\\s{0,2}|)(?# nochmal space oder nicht)\\d*(?# zahl mit länge 0 oder mehr)(\\]|\\)|)(?# schliesst mit versch klammer oder nicht)|)(?# seitenzahlen ja,nein, falls nein einfach klammer matchen)(\\]|\\)|)", sep="")
      # paste(authorname[1], " and ", authorname[2], sep=""), # Without year!
    }

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
    if(length.authorname>=3){
      searchterms <- c(
        paste(authorname[1], " AND OTHERS", "(|,)\\s{0,2}(|\\[|\\()" ,studyyear, "(\\s{0,2}(:|,)(?# Komma oder Doppelpunkt)\\s{0,2}(PAGE|)(?# Page kommt vor oder nicht)(\\s{0,2}|)(?# nochmal space oder nicht)\\d*(?# zahl mit länge 0 oder mehr)(\\]|\\)|)(?# schliesst mit versch klammer oder nicht)|)(?# seitenzahlen ja,nein, falls nein einfach klammer matchen)(\\]|\\)|)", sep="")

      )

    }

#############################################





# Get file names
  file.names <- dir(paste("./", folder, sep = ""), pattern = ".txt")
  file.paths <- paste(paste("./", folder, "/", sep = ""), file.names, sep="")
  n.docs <- length(file.paths)

  # Specify number of documents
  if(!is.null(number)){n.docs <- number}




# Regexp for search terms and dependency on scope
    searchterms <- paste("\\.[^.]*", searchterms, "[^.]*\\.", sep = "")

    if(!is.null(scope)){

      searchterms <- paste(paste(rep("\\.[^.]*", scope), collapse=""),
                           searchterms,
                           paste(rep("[^.]*\\.", scope), collapse=""),
                           sep = "")
    }

      #
      # "\\. = Starts with a dot
      # [^.] = not dot characters
      # * = 0 or more
      # \\." = Ends with a dot




  # Load documents and search for full citation in them
  all.docs.cit.cases <- NULL
  for (i in 1:n.docs){
    con <- file(file.paths[i], encoding = "UTF-8")
    x <- readLines(con)
    close(con)


    cit.cases.doc.i <- stringr::str_extract_all(x, paste(searchterms, collapse="|"))

    # identify ". Beck and Katz 1995."
    # cit.cases.doc.i[cit.cases.doc.i==". Beck and Katz 1995."]



    all.docs.cit.cases[i] <- cit.cases.doc.i

    # Counter
    if(stringr::str_detect(as.character(i), "[0-9]*0")){cat(i, ".. ", sep="")}

  }




  # Get fist estimate of number of citation cases
  total.citation.cases <- sum(sapply(all.docs.cit.cases, length))

  cat("\n For ", authorname, " we have identified ", total.citation.cases, " citation cases within ", n.docs, " documents.", sep="")

  # Generate dataframe with citation cases
  citation.data <- data.frame(document = 1:total.citation.cases, citation.case = 1:total.citation.cases)
  citation.data[,1] <- rep(file.names, sapply(all.docs.cit.cases, length))
  citation.data[,2] <- unlist(all.docs.cit.cases)
  # Change document names
  citation.data$document <- sub("\\s+$", "", stringr::str_extract(citation.data$document, "^[^-]+"))

  # Save citation cases in table (csv and html)
  write.table(citation.data, file =  paste("./", folder, "_citation_cases.csv", sep = ""), sep=",")
  print(xtable::xtable(citation.data),type='html',comment=FALSE, file=paste("./", folder, "_citation_cases.html", sep = ""))

  cat("\n \nThey are printed and saved as files in the working directory: '*_citation_cases.csv' and '*_citation_cases.html'.\n\n")

  }
