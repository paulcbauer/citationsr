#' Deletes the references section in "_cleaned.txt" files and generates "cleaned_woref.txt" files.
#'
#' @return Returns and saves text files that do not contain a reference section anymore.

delete_reference_section <- function(){


  folders <- list.files()
  for(z in 1:length(folders)){


  # Delete files with "cleaned" in name if there are any from before
  file.names <- dir(paste("./", folders[z], "/documents/", sep = ""), pattern = "_woref.txt")
  if(identical(file.names, character(0))==FALSE){
    file.paths <- paste(paste("./", folders[z], "/documents/", sep = ""), file.names, sep="")
    file.remove(file.paths)
  }

  # List file names
  file.names <- dir(paste("./", folders[z], "/documents/", sep = ""), pattern = "cleaned.txt")

  # Generate individual paths to each file
  file.paths <- paste(paste("./", folders[z], "/documents/", sep = ""), file.names, sep="")

  # Load documents and search for full citation in them
  list.extracted.titles <- NULL
  for (i in 1:length(file.paths)){ # 4 DOES NOT WORK!
    x <- readLines(file.paths[i])

    # Locate "References" section
    references.location <- grep("References|REFERENCES", x, ignore.case = FALSE)



    if (length(references.location)==1){
      print("'References' only appears once. Everything fine with discarding the reference sections.")
    } else {
      cat("\nScotty we have a problem with the references in the following study:\n\n", file.paths[i], sep="")

      cat("\n\n", "We identified '", grep("References|REFERENCES", x, ignore.case = FALSE, value=T), "' was identified.\n", sep = "")

    }
    x <- x[1:references.location-1]

    fileConn<-file(paste("./", folders[z], "/documents/", substr(file.names[i], 1, nchar(file.names[i])-4), "_woref.txt", sep=""))
    writeLines(x, fileConn)
    close(fileConn)

  }
  }
}
