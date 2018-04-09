#' Identifies meta information within txt documents.
#'
#' @param folder Name of folder that contains the documents.
#' @param pdf Default TRUE. Set FALSE if only .txt files should be renamed.
#' @param txt Default TRUE. Set FALSE if only .pdf files should be renamed.
#'
#'

#' @description Renames files in folder with rising index.
#'
#' @examples
#' \dontrun{
#'  setwd("C:/data")
#'  rename_docs(folder = "docs")
#' }


reset_doc_names <- function(folder = "docs", pdf = TRUE, txt = TRUE){

  # Rename filenames for first extraction (PDFs)
  file.paths.pdf <- dir("docs", pattern = ".pdf", full.names = TRUE)
  file.paths.txt <- dir("docs", pattern = ".txt", full.names = TRUE)
  # renaming
  # Generate names
  names <- formatC(seq(1, length(file.paths.pdf)), width=nchar(length(file.paths.pdf)), flag=0)
  # see also sprintf("%04d", 1:99)


  for(i in 1:length(file.paths.pdf)){

    if(isTRUE(pdf)){
    file.rename(from = file.paths.pdf[i],
                to = paste(folder, "/doc", names[i], ".pdf", sep=""))
    }

    if(isTRUE(txt)){
    file.rename(from = file.paths.txt[i],
                to = paste(folder, "/doc", names[i], ".txt", sep=""))
    }
  }

}


