#' Identifies whether single documents (text files) really cite (or just contain) study.
#'
#' @param study.title Title of the study, e.g. "Measuring trust"
#' @param folder Name of folder within working directory in which the citing documents (.txt files) are located, e.g. "Beck 1995".
#' @param number Number of .txt files in folder the function should be applied to. Default is "all .txt files in folder".
#' @return Returns a both a .csv and html files that contain a
#' list with the titles that were extracted across the citing documents.


identify_study_doc <- function(study.title, folder, number=NULL){


# List file names in folder (ONLY .TXT FILES)
  file.names <- dir(paste("./", folder, sep = ""), pattern = ".txt")

# Generate file paths
  file.paths <- paste(paste("./", folder, "/", sep = ""), file.names, sep="")

# Count number of files in folder
  n.docs <- length(file.paths)

# Specify number of documents to assess by setting n.docs
  if(!is.null(number)){n.docs <- number}


# Loop over .txt files one by one (until document nr. "number" = n.docs)
  list.extracted.titles <- NULL
  for (i in 1:n.docs){
    con <- file(file.paths[i], encoding = "UTF-8")
    x <- readLines(con, warn = F)
    close(con)
    x <- paste(x, collapse = " ")

    # aregexec() - distance matching
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

  # Print table
    print(xtable::xtable(table),type='html',comment=FALSE, file=paste("./", folder, "_identified_citation.html", sep = ""))

  cat("\n Check whether citation were rightly identifed using file 'identify_citation.csv' 'identify_citation.html' in the working directory. \n\n")
}

