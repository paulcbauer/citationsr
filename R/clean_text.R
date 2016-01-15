#' Clean text files in /documents/ folder for subsequent analysis.
#'
#' @return Converts PDFs to text and stores in documents folder.
#' @param folder The folder in which the PDFs are located.


clean_text <- function(folder, number=NULL){




# Identify txt files
  file.names <- dir(paste("./", folder, sep = ""), pattern = ".txt")
  file.paths <- paste(paste("./", folder, "/", sep = ""), file.names, sep="")
  n.docs <- length(file.paths)

  # Specify number of documents
  if(!is.null(number)){n.docs <- number}


# Load texts, clean them and save them
  for (i in 1:n.docs){
    con <- file(file.paths[i], encoding = "UTF-8") #
    x <- readLines(con)
    close(con)
    x <- paste(x, collapse = " ")


# REPLACE DOTS IN DIFFERENT FORMS

    # ABBREVIATIONS
      x <- stringr::str_replace_all(x, "et al\\.", "AND OTHERS")
      x <- stringr::str_replace_all(x, "e\\.g\\.", "FOR EXAMPLE")
      x <- stringr::str_replace_all(x, "No\\.", "NUMBER")
      x <- stringr::str_replace_all(x, "NO\\.", "NUMBER")
      x <- stringr::str_replace_all(x, "fig\\.", "FIGURE")
      x <- stringr::str_replace_all(x, "obs\\.", "OBSERVATIONS")
      x <- stringr::str_replace_all(x, "var\\. ", "VARIANCE")
      x <- stringr::str_replace_all(x, "i\\.e\\.", "THAT IS")
      x <- stringr::str_replace_all(x, "vs\\.", "VERSUS")
      x <- stringr::str_replace_all(x, " p\\.", "PAGE")
      x <- stringr::str_replace_all(x, "pp\\.", "PAGE")
      x <- stringr::str_replace_all(x, " pp\\.", "PAGE")
      x <- stringr::str_replace_all(x, "\\(pp\\.", "PAGE")
      x <- stringr::str_replace_all(x, "apps\\.", "APPENDIX")
      x <- stringr::str_replace_all(x, "app\\.", "APPENDIX")
      x <- stringr::str_replace_all(x, "\\. \\. \\.", "\\[;;;\\]")
      x <- stringr::str_replace_all(x, "n\\.d\\.", "NO DATE")
      x <- stringr::str_replace_all(x, "f\\.n\\.", "FOOTNOTE")
      x <- stringr::str_replace_all(x, "VOL\\.", "VOLUME")
      x <- stringr::str_replace_all(x, "vol\\.", "VOLUME")
      x <- stringr::str_replace_all(x, "esp\\.", "ESPECIALLY")
      x <- stringr::str_replace_all(x, "Jr\\.", "JUNIOR")
      x <- stringr::str_replace_all(x, "U\\.S\\.", "UNITED STATES")
      x <- stringr::str_replace_all(x, "U\\.S\\.", "UNITED STATES")


      # REPLACE DOTS IN DECIMAL NUMBER WITH ","
      detected <- unlist(stringr::str_extract_all(x, "[0-9]{1,10}\\.[0-9]{1,10}"))
      if(length(detected)>0){
        for(z in 1:length(detected)){
          x <- stringr::str_replace(x, detected[z], stringr::str_replace_all(detected[z], "\\.", ","))

        }
      }






    # NAMES IN TEXT
      x <- stringr::str_replace_all(x, "G. Bingham Powell and Guy Whitten", "Bingham Powell and Whitten")


    # Replace citation of interest with original name (NOT NECESSARY)
      # x <- stringr::str_replace_all(x, "Glaeser AND OTHERS", "Glaeser et al.")
      # 0.82 -> 0,82
      # Get rid of em-dashes - QUESTION WHICH COMPUTER YOU USE
        # x <- iconv(x, "", "ASCII", "byte")
        # x <- stringr::str_replace_all(x, "<c2><ad><e2><80><93>", "-")
        # x <- iconv(x, "", "UTF-8")



    fileConn<-file(file.paths[i])
    writeLines(x, fileConn, useBytes = TRUE)
    close(fileConn)

    # Counter
    if(stringr::str_detect(as.character(i), "[0-9]*0")){cat(i, ".. ", sep="")}

    }

    cat("\n\n", n.docs, " texts/documents have been cleaned in folder '", folder ,"' and are now reeeeaaadddyyy to be analyzed!\n\n", sep = "")

}


