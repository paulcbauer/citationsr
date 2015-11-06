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


# Extract texts from all PDF files in directory and save as txt file in folder
  text.list <- NULL
  for (i in 1:length(file.paths)){ # 4 DOES NOT WORK!
  text.list[[i]] <- fulltext::ft_extract(file.paths[i], which = "xpdf")
  # text.list[[i]] <- fulltext::ft_extract(file.paths[i], which = "xpdf", "-layout")
  # text.list[[i]] <- fulltext::ft_extract(file.paths[i], which = "xpdf", "-table", "-layout")
  }

  cat("\n\n", length(file.paths), " PDFs where just sequeezed to release their text in folder '", folders[z], "'!\n\n", sep = "")

  }
  }







