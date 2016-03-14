#' Clean text files in /documents/ folder for subsequent analysis.
#'
#' @return Converts PDFs to text and stores in documents folder.


clean_filenames <- function(){

  require(stringr)

  # Specify folders, i.e. studies
  folders <- list.files()
  for(z in 1:length(folders)){




# Identify files
  file.names <- dir(paste("./", folders[z], "/documents/", sep = ""))


  for(i in 1:length(file.names)){
    if(stringr::str_detect(file.names[i], "…")==TRUE){
      file.rename(paste("/", folders[z], "/documents/", file.names[i], sep = "")
                  , stringr::str_replace(paste("/", folders[z], "/documents/", file.names[i], sep = ""), "…", " "))
    }
    if(stringr::str_detect(file.names[i], "\\.\\.\\.")==TRUE){
      file.rename(paste("/", folders[z], "/documents/", file.names[i], sep = "")
                  , stringr::str_replace(paste("/", folders[z], "/documents/", file.names[i], sep = ""), "\\.\\.\\.", " "))
    }
  }


}
}




