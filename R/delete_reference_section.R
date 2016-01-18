#' Deletes the references section in "_cleaned.txt" files and generates "cleaned_woref.txt" files.
#'
#' @return Returns and saves text files that do not contain a reference section anymore.

delete_reference_section <- function(folder, number=NULL){

  # List file names
    file.names <- dir(paste("./", folder, sep = ""), pattern = ".txt")
    file.paths <- paste(paste("./", folder, "/", sep = ""), file.names, sep="")
    n.docs <- length(file.paths)

    # Specify number of documents
    if(!is.null(number)){n.docs <- number}

  # Loop to generate txt files without reference section
  for (i in 1:n.docs){

    con <- file(file.paths[i], encoding = "UTF-8")
    x <- readLines(con, warn = F)
    close(con)


    # Locate "References" section
      references.location <- grep("References|REFERENCES|Literatur|LITERATUR", x, ignore.case = FALSE)

      if(length(references.location)==1){
        cat("In document '", file.names[i], "' 'References' only appears once. (discarded).\n\n", sep="")
        fileConn<-file(file.paths[i])
        writeLines(x[1:references.location], fileConn, useBytes = TRUE)
        close(fileConn)
        }

      if(length(references.location)>1){
        cat("In document '", file.names[i], "' 'References' appears more than once. PROBLEM! (using last location).\n\n", sep="")
        fileConn<-file(file.paths[i])
        writeLines(x[1:tail(references.location, n=1)], fileConn, useBytes = TRUE)
        close(fileConn)
        }

      if(length(references.location)<1){
        cat("In document '", file.names[i], "' 'References' does not appear at all. PROBLEM! (NOT discarded).\n\n", sep="")
        fileConn<-file(file.paths[i])
        writeLines(x, fileConn, useBytes = TRUE)
        close(fileConn)
        }




          # Save text file without references


        # },
          # error=function(cond) {
          # message(paste("\n PROBLEM! \n", file.paths[i]))
          # },
          #  warning=function(cond) {
          # message(paste("\n WARNING! \n", file.paths[i]))
          # },
          # finally={}
          # )

      # Counter
      if(stringr::str_detect(as.character(i), "[0-9]*0")){cat(i, ".. ", sep="")}

  }}
