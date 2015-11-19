#' Clean text files in /documents/ folder for subsequent analysis.
#'
#' @return Converts PDFs to text and stores in documents folder.



clean_filenames <- function(){

# Specify folders, i.e. studies
  folders <- list.files()
  for(z in 1:length(folders)){

# Identify files
  file.names <- dir(paste("./", folders[z], "/documents/", sep = ""))

# REPLACE uncommon stuff in filenames '
  for(i in 1:20){ # length(file.names)
    if(str_detect(file.names[i], "…")==TRUE){
      file.rename(paste("/", folders[z], "/documents/", file.names[i], sep = "")
                  , str_replace(paste("/", folders[z], "/documents/", file.names[i], sep = ""), "…", ""))
    }



    if(str_detect(file.names[i], "  ")==TRUE){
      file.rename(paste("/", folders[z], "/documents/", file.names[i], sep = "")
                  , str_replace(paste("/", folders[z], "/documents/", file.names[i], sep = ""), "  ", " "))
    }

    if(str_detect(file.names[i], "  ")==TRUE){
      file.rename(paste("/", folders[z], "/documents/", file.names[i], sep = "")
                  , str_replace(paste("/", folders[z], "/documents/", file.names[i], sep = ""), "  ", " "))
    }


    if(str_detect(file.names[i], "\\.\\.\\.")==TRUE){
      file.rename(paste("/", folders[z], "/documents/", file.names[i], sep = "")
                  , str_replace(paste("/", folders[z], "/documents/", file.names[i], sep = ""), "\\.\\.\\.", ""))
    }

    if(str_detect(file.names[i], "\\.\\.\\.")==TRUE){
      file.rename(paste("/", folders[z], "/documents/", file.names[i], sep = "")
                  , str_replace(paste("/", folders[z], "/documents/", file.names[i], sep = ""), "\\.\\.\\.", ""))
    }

    if(str_detect(file.names[i], "—")==TRUE){
      file.rename(paste("/", folders[z], "/documents/", file.names[i], sep = "")
                  , str_replace(paste("/", folders[z], "/documents/", file.names[i], sep = ""), "—", ""))
    }

    if(str_detect(file.names[i], "'")==TRUE){
      file.rename(paste("/", folders[z], "/documents/", file.names[i], sep = "")
                  , str_replace(paste("/", folders[z], "/documents/", file.names[i], sep = ""), "’", ""))
    }

  }}}




