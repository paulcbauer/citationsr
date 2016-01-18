#' Convert PDFs in /documents/ into text files which are stored in same folder.
#'
#' @return Converts PDFs into text files that a stored in the folder.
#' @param folder Name of folder within your working directory that contains the PDFs.
#' @param number Number of documents you want to apply function to. Default is "all docs in folder".
#' Looping over folder containing citing documents for different studies is possible.


extract_text <- function(folder, number=NULL) {

# Add xpdf to the environmental variable PATH
  # Sys.getenv("PATH") # check path
  Sys.setenv(PATH = paste(Sys.getenv("PATH"), ";C:\\Program Files\\xpdf\\bin32\\", sep="")) # LAPTOP
  Sys.setenv(PATH = paste(Sys.getenv("PATH"), ";C:\\Program Files\\xpdf\\bin64\\", sep=""))

# Delete text files in folder if there are any
  file.names <- dir(paste("./", folder, sep = ""), pattern = ".txt")
  if(identical(file.names, character(0))==FALSE){
    file.paths <- paste(paste("./", folder,"/", sep = ""), file.names, sep="")
    file.remove(file.paths)
  }



# Extract PDF file names + path
  file.names <- dir(paste("./", folder, sep = ""), pattern = ".pdf")
  file.paths <- paste(paste("./", folder,"/", sep = ""), file.names, sep="")
  n.docs <- length(file.paths)

  # Specify number of documents
  if(!is.null(number)){n.docs <- number}





  ptm <- proc.time()
  for (i in 1:n.docs){  #
    tryCatch(
      {

        fulltext::ft_extract(file.paths[i], which = "xpdf")
        cat("\n Ok! ", file.paths[i], "\n", sep="")

      },
      error=function(cond) {
        message(paste("\n PROBLEM! \n", file.paths[i]))
        # message("\n Here's the original error message: \n")
        # message(cond)

        # Create folder for bad PDFs (if not present)
        if(dir.exists(paste("./", folder, "/text extraction error/", sep = ""))==F){
          dir.create(paste("./", folder, "/text extraction error/", sep = ""))}

        # Copy bad PDFs to 'text extraction error' folder
        file.copy(file.paths[i], paste("./", folder, "/text extraction error/", file.names[i], sep = ""), overwrite = TRUE, recursive = FALSE,
                  copy.mode = TRUE, copy.date = FALSE)

        # Delete bad PDFs from documents folder
        unlink(file.paths[i], force = T)
        message("\n\nPDF copied to 'text extraction error' folder.\n")

      },
      warning=function(cond) {
        message(paste("\n WARNING! \n", file.paths[i]))
        # message("Here's the original warning message:")
        # message(cond)

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
      finally={
        # NOTE: Final message regardess of error or success
        # message(paste("\n\n Finished!", file.paths[i]))
      })}

  time <- proc.time() - ptm

  # Messages
  cat("\n\n For the folder '", folder, "' text extraction too around ", as.numeric(time[3]), " second(s) this is around ", round(as.numeric(time[3])/60,2), " minutes.\n", sep = "")

  cat("\n", n.docs, " PDFs where just sequeezed to release their text in folder '", folder, "'!\n\n", sep = "")

  cat(length(dir(paste("./", folder, "/text extraction error", sep=""))),
      " PDF file(s) were problematic and copied to the 'text extraction error' folder." , sep = "")
  }







