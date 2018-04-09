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


docs_names_encoding <- function(folder = "docs"){

  file.paths <- dir(folder, pattern = ".pdf|.txt", full.names = TRUE)
  file.paths2 <- stringr::str_replace(file.paths, "“", "")
  file.paths2 <- stringr::str_replace(file.paths2, "'", '')
  file.paths2 <- iconv(file.paths2, "UTF-8", "ASCII", sub="")
  # Modify file names
  for(i in 1:length(file.paths)){
    file.rename(from = file.paths[i], to = file.paths2[i])
  }

}


