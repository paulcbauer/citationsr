#' Extract citation cases from "cleaned_woref.txt" files.
#'
#' @return Returns and saves data on citation cases (citation_data.csv).


extract_citation_cases <- function(){
  folders <- list.files()
  for(z in 1:length(folders)){


  # Get file names
  file.names <- dir(paste("./", folders[z], "/documents/", sep = ""), pattern = "cleaned_woref.txt")

  # Generate individual paths to each file
  file.paths <- paste(paste("./", folders[z], "/documents/", sep = ""), file.names, sep="")

  # Extract search term from folder name
    authorname <- stringr::str_extract(folders[z], "[:alpha:]*")

  # Generate extraction regexp
    searchterm <- paste("\\.[^.]*", authorname, "[^.]*\\.", sep = "")

  # Load documents and search for full citation in them
  all.docs.cit.cases <- NULL
  for (i in 1:length(file.paths)){ # 4 DOES NOT WORK!
    x <- readLines(file.paths[i])
    x <- paste(x, collapse = " ")
    cit.cases.doc.i <- stringr::str_extract_all(x, searchterm)
    all.docs.cit.cases[i] <- cit.cases.doc.i
  }

  # Get fist estimate of number of citation cases
  total.citation.cases <- sum(sapply(all.docs.cit.cases, length))

  cat("\n For ", folders[z], " we have identified ", total.citation.cases, " citation cases within ", length(file.paths), " documents.", sep="")

  # Generate dataframe with citation cases
  citation.data <- data.frame(document = 1:total.citation.cases, citation.case = 1:total.citation.cases)
  citation.data[,1] <- rep(file.names, sapply(all.docs.cit.cases, length))
  citation.data[,2] <- unlist(all.docs.cit.cases)
  # Change document names
  citation.data$document <- sub("\\s+$", "", stringr::str_extract(citation.data$document, "^[^-]+"))
  # print(citation.data)
  write.table(citation.data, file =  paste("./", folders[z], "/citation_cases.csv", sep = ""), sep=",")
  cat("\n \nThey are printed and saved in the file named 'citation_cases.csv' in the respective folder.\n\n")
  }
}






