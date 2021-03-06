#' Clean text files to facilitate citation case extraction.
#'
#' @param folder Name of folder within working directory in which the citing documents (.txt files) are located, e.g. "Beck 1995".
#' @param number Number of .txt files in folder the function should be applied to. Default is "all .txt files in folder".
#' @return Returns and saves text files that have been cleaned (e.g. abbreviations replaced)
#'
#'
#' @examples
#' \dontrun{
#'  setwd("C:/Users/paul/Google Drive/Research/2016_Quality_of_citations/data")
#'  folder <- "Acemoglu 2001"
#'  clean_text(folder)
#' }

clean_text <- function(folder, number=NULL, encoding = "ASCII"){


  require(stringr)


# List file names in folder (ONLY "*processed.txt" FILES)
  file.names <- dir(paste(folder, sep = ""), pattern = "processed.txt")

# Generate file paths
  file.paths <- paste(paste(folder, "/", sep = ""), file.names, sep="")

# Count number of files in folder
  n.docs <- length(file.paths)

# Specify number of documents to assess by setting n.docs
  if(!is.null(number)){n.docs <- number}
  if(!is.null(number)&&number>length(file.paths)){n.docs <- length(file.paths)} # if not enough files


# Loop over .txt files one by one (until document nr. "number" = n.docs)
  for (i in 1:n.docs){
    con <- file(file.paths[i], encoding = encoding) #
    x <- readLines(con)
    close(con)
    x <- paste(x, collapse = " ")


# Replace dots that appear in various formats

    # Replace dots in abbreviations
      x <- stringr::str_replace_all(x, "et al\\.", "AND OTHERS")
      x <- stringr::str_replace_all(x, "e\\.g\\.", "FOR EXAMPLE")
      x <- stringr::str_replace_all(x, "etc\\.", "ET CETERA")
      x <- stringr::str_replace_all(x, "cf\\.", "COMPARE")
      x <- stringr::str_replace_all(x, "No\\.", "NUMBER")
      x <- stringr::str_replace_all(x, "NO\\.", "NUMBER")
      x <- stringr::str_replace_all(x, " N\\. ", " NUMBER ")
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
      x <- stringr::str_replace_all(x, "E\\.q\\.", "EQUATION")
      x <- stringr::str_replace_all(x, "Eq\\.", "EQUATION")
      x <- stringr::str_replace_all(x, " chs\\.", " CHAPTERS")
      x <- stringr::str_replace_all(x, "CHs\\.", "CHAPTERS")
      x <- stringr::str_replace_all(x, "Ch\\.", "CHAPTER")
      x <- stringr::str_replace_all(x, "CH\\.", "CHAPTER")
      x <- stringr::str_replace_all(x, " I\\.", " ROMAN-ONE")
      x <- stringr::str_replace_all(x, " II\\.", " ROMAN-TWO")
      x <- stringr::str_replace_all(x, " III\\.", " ROMAN-THREE")
      x <- stringr::str_replace_all(x, " IV\\.", " ROMAN-FOUR")
      x <- stringr::str_replace_all(x, " V\\.", " ROMAN-FIVE")
      x <- stringr::str_replace_all(x, " VI\\.", " ROMAN-SIX")
      x <- stringr::str_replace_all(x, " VII\\.", " ROMAN-SEVEN")

      # x <- "lksdjfl! sdflkj. sdfsdf? lskdjfldj. sldjdsl"

      # Other endings of sentences
      x <- stringr::str_replace_all(x, "\\?", "\\.\\?")
      x <- stringr::str_replace_all(x, "!", "\\.!")

      # Quotation marks
      x <- stringr::str_replace_all(x, '\\\\"', "''")

      # Countries
      x <- stringr::str_replace_all(x, "Dem\\. Rep\\. Congo", "Democratic Republic of Congo")




      # Replace footnotes
      detected <- unlist(stringr::str_extract_all(x, paste("[a-z)0-9\\]]\\.", paste("(", paste(seq(1,40), collapse="|"), ")", sep=""),"\\s[A-Z]", sep="")))
      detected <- stringr::str_replace_all(detected, "\\)", "\\\\)")
      if(length(detected)>0){
        for(z in 1:length(detected)){
          x <- stringr::str_replace_all(x, detected[z], stringr::str_replace_all(detected[z], "\\.", "\\.FOOTNOTE"))
        }
      }



      # REPLACE DOTS IN DECIMAL NUMBER WITH ","
      detected <- unlist(stringr::str_extract_all(x, "[0-9]{1,10}\\.[0-9]{1,10}"))
      if(length(detected)>0){
        for(z in 1:length(detected)){
          x <- stringr::str_replace(x, detected[z], stringr::str_replace_all(detected[z], "\\.", ","))

        }
      }
      # User better regex to identify numbers (see anki)






    # Replaces dots in names in text
      # x <- stringr::str_replace_all(x, "G. Bingham Powell and Guy Whitten", "Bingham Powell and Whitten")


      # Get rid of em-dashes - QUESTION WHICH COMPUTER YOU USE
        # x <- iconv(x, "", "ASCII", "byte")
        # x <- stringr::str_replace_all(x, "<c2><ad><e2><80><93>", "-")
        # x <- iconv(x, "", "UTF-8")


    # Save cleaned text in text files
    fileConn<-file(file.paths[i])
    writeLines(x, fileConn, useBytes = TRUE)
    close(fileConn)

    # Counter
    if(stringr::str_detect(as.character(i), "[0-9]*0")){cat(i, ".. ", sep="")}

    }

# Message to user
    cat("\n\n", i, " *.txt documents were processed in folder '", folder ,"' !\n\n", sep = "")

}


