#' Returns and saves text files without the references section.
#'
#' @param folder Name of folder within working directory in which the citing documents (.txt files) are located, e.g. "Beck 1995".
#' @param number Number of .txt files in folder the function should be applied to. Default is "all .txt files in folder".
#' @return Returns and saves text files without the references section.

delete_ref_section <- function(folder, number=NULL){

# List file names in folder (ONLY .TXT FILES)
    file.names <- dir(paste("./", folder, sep = ""), pattern = ".txt")

# Generate file paths
    file.paths <- paste(paste("./", folder, "/", sep = ""), file.names, sep="")

# Count number of files in folder
    n.docs <- length(file.paths)

# Specify number of documents to assess by setting n.docs
    if(!is.null(number)){n.docs <- number}

# Loop over .txt files one by one (until document nr. "number" = n.docs)
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



      # Counter
      if(stringr::str_detect(as.character(i), "[0-9]*0")){cat(i, ".. ", sep="")}

  }}
