#' Convert PDFs in /documents/ into text files which are stored in same folder.
#'
#' @return Converts PDFs into text files that a stored in the "documents" folder.


extract_text <- function() {

# Add xpdf to the environmental variable PATH
  Sys.getenv("PATH")
  Sys.setenv(PATH = paste(Sys.getenv("PATH"), ";C:\\Program Files\\xpdf\\bin32\\", sep="")) # LAPTOP
  Sys.setenv(PATH = paste(Sys.getenv("PATH"), ";C:\\Program Files\\xpdf\\bin64\\", sep=""))
  Sys.getenv("PATH")

# Specify folders, i.e. studies
  folders <- list.files()
  for(z in 1:length(folders)){


# Delete text files in folder if there are
  file.names <- dir(paste("./", folders[z], "/documents/", sep = ""), pattern = ".txt")
  if(identical(file.names, character(0))==FALSE){
    file.paths <- paste(paste("./", folders[z], "/documents/", sep = ""), file.names, sep="")
    file.remove(file.paths)
  }



# Extract PDF file names + path
  file.names <- dir(paste("./", folders[z], "/documents/", sep = ""), pattern = ".pdf")
  file.paths <- paste(paste("./", folders[z], "/documents/", sep = ""), file.names, sep="")


  ptm <- proc.time()
  for (i in 1:length(file.paths)){  #
    tryCatch(
      {

        fulltext::ft_extract(file.paths[i], which = "xpdf")
        message(paste("\n\n Ok!", file.paths[i]))

      },
      error=function(cond) {
        message(paste("\n PROBLEM! \n", file.paths[i]))
        # message("\n Here's the original error message: \n")
        # message(cond)

        # Create folder for bad PDFs (if not present)
        if(dir.exists(paste("./", folders[z], "/documents/error/", sep = ""))==F){
          dir.create(paste("./", folders[z], "/documents/error/", sep = ""))}

        # Copy bad PDFs to error folder
        file.copy(file.paths[i], paste("./", folders[z], "/documents/error/", file.names[i], sep = ""), overwrite = TRUE, recursive = FALSE,
                  copy.mode = TRUE, copy.date = FALSE)

        # Delete bad PDFs from documents folder
        unlink(file.paths[i], force = T)
        message("\n\nPDF copied to error folder.\n")

      },
      warning=function(cond) {
        message(paste("\n WARNING! \n", file.paths[i]))
        # message("Here's the original warning message:")
        # message(cond)

        # Create folder for bad PDFs (if not present)
        if(dir.exists(paste("./", folders[z], "/documents/error/", sep = ""))==F){
          dir.create(paste("./", folders[z], "/documents/error/", sep = ""))}

        # Copy bad PDFs to error folder
        file.copy(file.paths[i], paste("./", folders[z], "/documents/error/", file.names[i], sep = ""), overwrite = TRUE, recursive = FALSE,
                  copy.mode = TRUE, copy.date = FALSE)
        # Delete bad PDFs from documents folder
        unlink(file.paths[i], force = T)
        message("\n\n PDF copied to error folder.\n")

      },
      finally={
        # NOTE: Final message regardess of error or success
        # message(paste("\n\n Finished!", file.paths[i]))
      })}

  time <- proc.time() - ptm
  cat("\n\n For ", folders[z], " text extraction too around ", as.numeric(time[3]), " second(s) this is around ", round(as.numeric(time[3])/60,2), " minutes.\n", sep = "")

  cat("\n\n For '", folders[z], "' ", length(dir(paste("./", folders[z], "/documents/error", sep=""))),
      " PDF files were problematic." , sep = "")

  cat("\n\n", length(file.paths), " PDFs where just sequeezed to release their text in folder '", folders[z], "'!\n\n", sep = "")

  }
  }







