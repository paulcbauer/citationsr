#' Identifies whether single documents (text files) really cite (or just contain) study.
#'
#' @param study.title Specify title of study.
#' @return Returns a list of the extracted references/titles that correspond to the input title.

identify_study <- function(study.title){
  folders <- list.files()
  for(z in 1:length(folders)){

  # List file names
  file.names <- dir(paste("./", folders[z], "/documents/", sep = ""), pattern = "cleaned.txt")

  # Generate individual paths to each file
  file.paths <- paste(paste("./", folders[z], "/documents/", sep = ""), file.names, sep="")

  # Load documents and search for full citation in them
  list.extracted.titles <- NULL
  for (i in 1:length(file.paths)){ # 4 DOES NOT WORK!
    x <- readLines(file.paths[i])
    x <- paste(x, collapse = " ")
    # aregexec()
    DISTANCE <- 20 # CHOOSE DISTANCE: Must be high enough!
    start <- aregexec(study.title, x, max.distance = DISTANCE)[[1]][1]
    length <- as.numeric(attributes(aregexec(study.title, x, max.distance = DISTANCE)[[1]]))
    end <- start + length - 1
    start <- start - 70
    end <- end + 60
    extracted.title <- substr(x, start, end)
    list.extracted.titles[[i]] <- extracted.title
  }


   # Replace so that whitespace is omitted
  cbind(stringr::str_extract(file.names, "[^-]*"), list.extracted.titles)
  write.table(cbind(stringr::str_extract(file.names, "[^-]*"), list.extracted.titles), paste("./", folders[z], "/identify_citation.csv", sep = ""))

  cat("\n Check whether citation were rightly identifed using file 'identify_citation.csv' in the respective study folder. \n\n")
}
}
