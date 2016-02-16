#' Converts and stores PDFs in folder as .txt files.
#'
#' @return Converts and stores PDFs in folder as .txt files.
#' @param folder Name of folder within working directory in which the citing documents (PDFs) are located, e.g. "Beck 1995".
#' @param number Number of PDFs you want to apply the function to. Default is "all PDFs in folder".



extract_text <- function(folder, number=NULL) {

# Add xpdf to the environmental variable PATH
  # use 'Sys.getenv("PATH")' to check path
  Sys.setenv(PATH = paste(Sys.getenv("PATH"), ";C:\\Program Files\\xpdf\\bin32\\", sep="")) # LAPTOP
  Sys.setenv(PATH = paste(Sys.getenv("PATH"), ";C:\\Program Files\\xpdf\\bin64\\", sep=""))

# Identify and delete any .txt files present in folder
  file.names <- dir(paste("./", folder, sep = ""), pattern = ".txt")
  if(identical(file.names, character(0))==FALSE){
    file.paths <- paste(paste("./", folder,"/", sep = ""), file.names, sep="")
    file.remove(file.paths)
  }



# Identify names of PDF files in folder + their path
  file.names <- dir(paste("./", folder, sep = ""), pattern = ".pdf")
  file.paths <- paste(paste("./", folder,"/", sep = ""), file.names, sep="")

# Count number of files in folder
  n.docs <- length(file.paths)

# Specify number of documents to assess by setting n.docs
  if(!is.null(number)){n.docs <- number}




    # Measure time
      ptm <- proc.time()

# Loop over .pdf files one by one (until document nr. "number" = n.docs)
  for (i in 1:n.docs){  #
    tryCatch(
      { # Good PDFs

        fulltext::ft_extract(file.paths[i], which = "xpdf")
        cat("\n Ok! ", file.paths[i], "\n", sep="")

      },
      error=function(cond)
      { # Error PDFs (e.g. no text extraction)

        # Give error message with path
        message(paste("\n PROBLEM! \n", file.paths[i]))

        # Create folder for bad PDFs (if not present)
        if(dir.exists(paste("./", folder, "/text extraction error/", sep = ""))==F){
          dir.create(paste("./", folder, "/text extraction error/", sep = ""))}

        # Copy bad PDFs to 'text extraction error' folder
        file.copy(file.paths[i], paste("./", folder, "/text extraction error/", file.names[i], sep = ""), overwrite = TRUE, recursive = FALSE,
                  copy.mode = TRUE, copy.date = FALSE)

        # Delete bad PDFs from documents folder
        unlink(file.paths[i], force = T)

        # Message
        message("\n\nPDF copied to 'text extraction error' folder.\n")

      },
      warning=function(cond)
      { # Warning PDFs

        message(paste("\n WARNING! \n", file.paths[i]))


        # Create folder for bad PDFs (if not present)
        if(dir.exists(paste("./", folder, "/text extraction error/", sep = ""))==F){
          dir.create(paste("./", folder, "/text extraction error/", sep = ""))}

        # Copy bad PDFs to error folder
        file.copy(file.paths[i], paste("./", folder, "/text extraction error/", file.names[i], sep = ""), overwrite = TRUE, recursive = FALSE,
                  copy.mode = TRUE, copy.date = FALSE)

        # Delete bad PDFs from documents folder
        unlink(file.paths[i], force = T)
        message("\n\n PDF copied to 'text extraction error' folder.\n")

      },
      finally={}

      )}

      # Measure time
      time <- proc.time() - ptm

  # Messages for user
    cat("\n\n For the folder '", folder, "' text extraction too around ", as.numeric(time[3]), " second(s) this is around ", round(as.numeric(time[3])/60,2), " minutes.\n", sep = "")

    cat("\n", n.docs, " PDFs where just sequeezed to release their text in folder '", folder, "'!\n\n", sep = "")

    cat(length(dir(paste("./", folder, "/text extraction error", sep=""))),
        " PDF file(s) were problematic and copied to the 'text extraction error' folder." , sep = "")
  }







