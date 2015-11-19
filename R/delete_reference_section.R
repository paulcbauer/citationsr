#' Deletes the references section in "_cleaned.txt" files and generates "cleaned_woref.txt" files.
#'
#' @return Returns and saves text files that do not contain a reference section anymore.

delete_reference_section <- function(){


  folders <- list.files()
  for(z in 1:length(folders)){


  # Delete files with "woref" in name if there are any from before
    file.names <- dir(paste("./", folders[z], "/documents/", sep = ""), pattern = "_woref.txt")
    if(identical(file.names, character(0))==FALSE){
      file.paths <- paste(paste("./", folders[z], "/documents/", sep = ""), file.names, sep="")
      file.remove(file.paths)
    }



  # List file names
    file.names <- dir(paste("./", folders[z], "/documents/", sep = ""), pattern = "cleaned.txt")

  # Generate individual paths to each file
    file.paths <- paste(paste("./", folders[z], "/documents/", sep = ""), file.names, sep="")


  # Loop to generate txt files without reference section
  for (i in 1:length(file.paths)){

    x <- readLines(file.paths[i])

    # Locate "References" section
      references.location <- grep("References|REFERENCES", x, ignore.case = FALSE)

      tryCatch(
        {
          print("'References' only appears once. Everything fine with discarding the reference sections.")

          # Save text file without references
          fileConn<-file(paste("./", folders[z], "/documents/", substr(file.names[i], 1, nchar(file.names[i])-4), "_woref.txt", sep=""))
          writeLines(x, fileConn)
          close(fileConn)

        },
        error=function(cond) {
          message(paste("\n PROBLEM! \n", file.paths[i]))
        },
        warning=function(cond) {
          message(paste("\n WARNING! \n", file.paths[i]))
        },
        finally={}
        )


  }}}
