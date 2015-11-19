#' Generate .ris for all folders/studies in working directory and save in respective folders called ris.
#'
#' @return Returns a .ris file that can be imported into paperpile.



gen_ris <- function(){
  folders <- list.files()
  for(z in 1:length(folders)){

    citations <- read.table(paste(folders[z], "/citations.csv", sep=""), header=TRUE, stringsAsFactors = F)

       # Create ris folder in respective study folder
    if(dir.exists(paste(folders[z], "/ris/", sep = ""))==FALSE){
    dir.create(paste(folders[z], "/ris/", sep = ""))}

    for (i in 1:nrow(citations)){
      fileConn <- file(paste(folders[z], "/ris/", citations$identifier[i], ".ris", sep=""))
      writeLines(c("TY  - JOUR",
                   paste("AU  - ", citations$author[i], sep=""),
                   paste("PY  - ", citations$year[i], sep=""),
                   paste("JO  - ", citations$journal[i], sep=""),
                   paste("DO  - ", citations$doi[i], sep=""),
                   paste("TI  - ", citations$title[i], sep="")), fileConn)
      close(fileConn)
    }

  }


}




