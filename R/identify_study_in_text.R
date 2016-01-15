#' Identifies whether single documents (text files) really cite (or just contain) study.
#'
#' @param study.title Specify title of study.
#' @param folder The folder in which the citing documents are located.
#' @return Returns a both a .csv file as well as a .html file that contain a
#' list with the titles that were extracted from all the citing documents.


identify_study <- function(study.title, folder, number=NULL){


  # List file names
  file.names <- dir(paste("./", folder, sep = ""), pattern = ".txt")
  file.paths <- paste(paste("./", folder, "/", sep = ""), file.names, sep="")

  n.docs <- length(file.paths)

  # Specify number of documents
  if(!is.null(number)){n.docs <- number}


  # Load documents and search for full citation in them
  list.extracted.titles <- NULL
  for (i in 1:n.docs){
    con <- file(file.paths[i], encoding = "UTF-8")
    x <- readLines(con, warn = F)
    close(con)
    x <- paste(x, collapse = " ")

    # aregexec()
    DISTANCE <- 20 # CHOOSE DISTANCE: Must be high enough!
    start <- aregexec(study.title, x, max.distance = DISTANCE)[[1]][1]
    length <- as.numeric(unlist(attributes(aregexec(study.title, x, max.distance = DISTANCE)[[1]]))[1])
    # REPAIR: WHAT TO WITH SEVERAL MATCHES
    end <- start + length - 1
    start <- start - 70
    end <- end + 60
    extracted.title <- substr(x, start, end)
    list.extracted.titles[[i]] <- extracted.title

    # Counter
    if(stringr::str_detect(as.character(i), "[0-9]*0")){cat(i, ".. ", sep="")}
  }


   # Replace so that whitespace is omitted
  table <- cbind(stringr::str_extract(file.names, "[^-]*"), list.extracted.titles)
  write.table(table, paste("./identify_citation_", folder, ".csv", sep = ""))



  print(xtable::xtable(table),type='html',comment=FALSE, file=paste("./identify_citation_", folder, ".html", sep = ""))

  cat("\n Check whether citation were rightly identifed using file 'identify_citation.csv' 'identify_citation.html' in the working directory. \n\n")
}

