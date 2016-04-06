#' Renames docs/txt files using the metadata.
#'
#' @param folder Name of folder within working directory in which the citing documents (.txt files) are located, e.g. "Beck 1995".
#' @param number Number of .txt files in folder the function should be applied to. Default is "all .txt files in folder".
#' @return Returns and saves text files without running heads.
#'
#'
#' @examples
#' \dontrun{
#'  setwd("C:/Users/paul/Google Drive/Research/2016_Quality_of_citations/data")
#'  folder <- "Acemoglu 2001"
#'  rename_docs(folder)
#' }

rename_docs <- function(folder){

  # load packages
    require(stringr)



  # Get medadatafile for folder/study
    load(paste("./", folder, "_metadata.RData", sep="")) # load RData file
    metadata <- foo



  # New Author variable
    authors <- list(NULL)
    for(i in 1:nrow(metadata)){
      if(!is.na(metadata$author[[i]])){authors[[i]] <- metadata$author[[i]]$family[1]}
      if(is.na(metadata$author[[i]])){authors[[i]] <- NA}
    }
       # paste(metadata$author[[i]]$family, collapse=",")
    for(i in 1:length(authors)){
      if(is.null(authors[[i]])){authors[[i]] <- NA}
    }
    metadata$author2 <- unlist(authors)


  # New titel variable
    metadata$title2 <- stringr::str_replace(substr(metadata$title, 1,50), "^\\s", "")

  # New year variable
    metadata$year <- substr(metadata$date1, 1,4)


  # New title variable
    metadata$new.doc_name.txt <- paste(metadata$author2, metadata$year, metadata$title2, sep = " - ") %>%
                                 stringr::str_replace_all("[?:]", " ") %>%
                                 paste(".txt", sep = "")

    metadata$new.doc_name.pdf <- paste(metadata$author2, metadata$year, metadata$title2, sep = " - ") %>%
                                 stringr::str_replace_all("[?:]", " ") %>%
                                 paste(".pdf", sep = "")



  # Rename .txt files
    file.rename(from = file.path(paste("./", folder, sep = ""), metadata$doc_name), to = file.path(paste("./", folder, sep = ""), metadata$new.doc_name.txt))

  # Rename .pdf files
    metadata$doc_name_pdf <- metadata$doc_name %>% stringr::str_replace_all(".txt", ".pdf")
    file.rename(from = file.path(paste("./", folder, sep = ""), metadata$doc_name_pdf), to = file.path(paste("./", folder, sep = ""), metadata$new.doc_name.pdf))

}
