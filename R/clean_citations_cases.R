#' Takes the rather "roughly" identifed citation cases and cleans them.
#'
#' @param file 'citation_data.csv' that contains data on the citation cases.
#' @return Returns and saves cleaned version of citation cases data in 'citation_data_cleaned.csv'.



clean_citation_cases <- function(file){
  folders <- list.files()
  for(z in 1:length(folders)){


  # Import citation.cases file
  citation.data <- read.table(paste("./", folders[z], "/citation_cases.csv", sep = ""), sep=",", stringsAsFactors = F)

  # Pull out year from folder name
  year <- as.numeric(stringr::str_extract(folders[z], "[0-9][0-9][0-9][0-9]"))

  # STEP 1:  Keep citation cases that match with the year of the respective citation, e.g. 2013
    citation.data <- citation.data[stringr::str_detect(citation.data$citation.case, as.character(year))==TRUE,]



  # STEP 2:
  # Clean leading dots .
    citation.data$citation.case <- stringr::str_replace(citation.data$citation.case, "^. ", "")

  # Clean trailing dots .
    citation.data$citation.case <- stringr::str_replace(citation.data$citation.case, ".$", "")

  # Save we new file name
    write.table(citation.data, paste("./", folders[z], "/citation_cases_cleaned.csv", sep = ""), sep = ",")


  cat("\n\n Cleaned citation data file saved under 'citation_data_cleaned.csv' in the corresponding study folder.\n\n")

  }
}
