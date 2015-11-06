#' Clean text files in /documents/ folder for subsequent analysis.
#'
#' @return Converts PDFs to text and stores in documents folder.


clean_text <- function(){

  # Specify folders, i.e. studies
  folders <- list.files()
  for(z in 1:length(folders)){

  # Delete files with "cleaned" in name if there are any from before
    file.names <- dir(paste("./", folders[z], "/documents/", sep = ""), pattern = "cleaned")
    if(identical(file.names, character(0))==FALSE){
      file.paths <- paste(paste("./", folders[z], "/documents/", sep = ""), file.names, sep="")
      file.remove(file.paths)
    }


# Identify files
  file.names <- dir(paste("./", folders[z], "/documents/", sep = ""), pattern = ".txt")

# Generate individual paths to each file
  file.paths <- paste(paste("./", folders[z], "/documents/", sep = ""), file.names, sep="")

# Load texts, clean them and save them

  for (i in 1:length(file.paths)){
    x <- readLines(file.paths[i])

    # Replace abbreviations in text with full words
      # et al. -> AND OTHERS
      # No. -> NUMBER
      # e.g. -> FOR EXAMPLE
      # obs. -> OBSERVATIONS
      # var. -> VARIANCE
      # fig. -> FIGURE
      # 0.82 -> 0,82
      x <- stringr::str_replace_all(x, "et al.", "AND OTHERS")
      x <- stringr::str_replace_all(x, "e.g.", "FOR EXAMPLE")
      x <- stringr::str_replace_all(x, "No.", "NUMBER")
      x <- stringr::str_replace_all(x, "fig.", "FIGURE")
      x <- stringr::str_replace_all(x, "obs.", "OBSERVATIONS")
      # Replace citation of interest with original name (NOT NECESSARY)
      # x <- stringr::str_replace_all(x, "Glaeser AND OTHERS", "Glaeser et al.")

      # Get rid of em-dashes - QUESTION WHICH COMPUTER YOU USE
        # x <- iconv(x, "", "ASCII", "byte")
        # x <- stringr::str_replace_all(x, "<c2><ad><e2><80><93>", "-")
        # x <- iconv(x, "", "UTF-8")



    fileConn<-file(paste("./", folders[z], "/documents/", substr(file.names[i], 1, nchar(file.names[i])-4), "_cleaned.txt", sep=""))
    writeLines(x, fileConn)
    close(fileConn)

    }

    cat("\n\n", length(file.paths), " texts have been cleaned in folder '", folders[z] ,"' and are now reeeeaaadddyyy to be analyzed!\n\n", sep = "")
}
}


