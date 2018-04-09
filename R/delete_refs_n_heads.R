#' Returns and saves text files without the references section.
#'
#' @param folder Name of folder within working directory in which the citing documents (.txt files) are located, e.g. "Beck 1995".
#' @param number Number of .txt files in folder the function should be applied to. Default is "all .txt files in folder".
#' @return Returns and saves text files without the references section.
#'
#'
#' @examples
#' \dontrun{
#'  setwd("C:/Users/paul/Google Drive/Research/2016_Quality_of_citations/data")
#'  folder <- "Acemoglu 2001"
#'  delete_refs_n_heads(folder, number = 100)
#' }


delete_refs_n_heads <- function(metadata = NULL, folder, number=NULL, encoding = "ASCII"){

    require(stringr)
    require(xtable)
    require(dplyr)



# DEL "processed.txt" files ####
  old.files <- dir(folder, pattern = "processed.txt", full.names = TRUE)
  if(length(old.files)!=0){
  file.remove(old.files)
  }


# Generate file names ####
  file.names <- dir(folder, pattern = ".txt")
  file.paths <- dir(folder, pattern = ".txt", full.names = TRUE)

# Count number of files in folder
    n.docs <- length(file.paths)

# Specify number of documents to assess by setting n.docs
    if(!is.null(number)){n.docs <- number}
    if(!is.null(number)&&number>length(file.paths)){n.docs <- length(file.paths)} # if not enough files

# Create empty data frame to collect potential deleted running heads
    deleted.runningheads <- data.frame(study= NA, running.head = NA, loop.i = NA)




# Loop over .txt files one by one (until document nr. "number" = n.docs)
  for (i in 1:n.docs){

    print(i)

    con <- file(file.paths[i], encoding = encoding)
    x <- readLines(con, warn = F)
    close(con)

  # Delete empty lines in text files
    x <- x[nzchar(x)]


##################
### REPLACE \f ###
##################
    # print(x[stringr::str_detect(x, "^\f")])
    x <- stringr::str_replace_all(x, "^\f", "")


### DELETE REFERENCE SECTION ####
    # Locate "References" section
      references.location <- grep("^References$|^REFERENCES$|^Literatur$|^LITERATUR$|^References and Notes$", x, ignore.case = FALSE)

      if(length(references.location)==1){
        # cat("In document '", file.names[i], "' 'References' only appears once. (discarded).\n\n", sep="")
        x <- x[1:references.location]
        }

      if(length(references.location)>1){
        cat("References: In '", substr(file.names[i], 1, 30), "...' 'searchterms' appear more than once (Using LAST LOCATION).\n\n", sep="")
        x <- x[1:tail(references.location, n=1)]
        }

      if(length(references.location)<1){
        cat("References: In '", substr(file.names[i], 1, 30), "...' 'searchterms' do not appear at all (NOT discarded).\n\n", sep="")
        }









# DELETE RUNNING HEADS ####


    # Identify running titles
    # Short length is one characteristic
    # Repetition is one characteristic

    # Store deleted running heads here for check later on
    collect.deleted <- NULL


    # DOWNLOADED FROM, BY ETC.
      # print("downloaded from")
      locations <- NULL
      pattern <- "Downloaded by|Downloaded from|All use subject to JSTOR Terms and Conditions"
      locations <- grep(pattern, x, ignore.case = TRUE)
      if(length(locations)!=0){
        locations.names <- grep(pattern, x, ignore.case = TRUE, value = T)
        if(length(locations.names)>=5){x <- x[-locations]}
        collect.deleted <- c(collect.deleted, paste("(IDENTFIER DOWNLOADED FROM) ", locations.names, sep=""))
      }


    # COPYRIGHT MESSAGES
      # print("copyright")
      locations <- NULL
      pattern <- paste("All use subject to JSTOR Terms and Conditions",
                       "\\s[0-9]{4}\\sThe\\sAuthors",
                       "\\w\\sPolitical Science Association",
                       "Blackwell Publishing Ltd",
                       "©",
                       "Journal compilation"
                       , sep="|")
      locations <- grep(pattern, x, ignore.case = FALSE)
      if(length(locations)!=0){
        locations.names <- grep(pattern, x, ignore.case = FALSE, value = T)
        if(length(locations.names)>=5){x <- x[-locations]}
        collect.deleted <- c(collect.deleted, paste("(IDENTFIER COPYRIGHT) ", locations.names, sep=""))
      }

# METADATA ####

      if(!is.null(metadata)){

    # PUBLISHER
      # print("publisher")
      locations <- NULL
      if(nchar(metadata$publisher[i])>=5&!is.na(nchar(metadata$publisher[i]))){ # nchar  is problematic..
        pattern <- metadata$publisher[i]
        locations <- grep(pattern, x, ignore.case = TRUE)
        locations.names <- grep(pattern, x, ignore.case = TRUE, value = T)
        locations <- locations[duplicated(locations.names) | duplicated(locations.names, fromLast=TRUE)]
        locations.names <- locations.names[duplicated(locations.names) | duplicated(locations.names, fromLast=TRUE)]
        if(length(locations.names)>4){
          x <- x[-locations]
          collect.deleted <- c(collect.deleted, paste("(IDENTFIER PUBLISHER) ", locations.names, sep=""))
        }
      }


    # JOURNAL
      # print("journal")
      locations <- NULL
      if(nchar(metadata$journal[i])>=5&!is.na(nchar(metadata$journal[i]))){
        pattern <- metadata$journal[i]
        locations <- grep(pattern, x, ignore.case = TRUE)
        locations.names <- grep(pattern, x, ignore.case = TRUE, value = T)
        locations <- locations[duplicated(locations.names) | duplicated(locations.names, fromLast=TRUE)]
        locations.names <- locations.names[duplicated(locations.names) | duplicated(locations.names, fromLast=TRUE)]
        if(length(locations.names)>4){
        x <- x[-locations]
        collect.deleted <- c(collect.deleted, paste("(IDENTFIER JOURNAL) ", locations.names, sep=""))
        }
        }


    # TITLE
      # print("title")
      locations <- NULL
      if(nchar(metadata$title[i])>=5&!is.na(nchar(metadata$title[i]))){
        pattern <- stringr::str_extract(metadata$title[i], "^\\s*[:word:]*\\s*[:word:]*[-]*[:word:]*\\s*[:word:]*[-]*[:word:]*\\s*[:word:]*[-]*[:word:]*\\s*[:word:]*[-]*[:word:]*\\s*[:word:]*[-]*[:word:]*")
        locations <- grep(pattern, x, ignore.case = TRUE)
        locations.names <- grep(pattern, x, ignore.case = TRUE, value = T)
        locations <- locations[duplicated(locations.names) | duplicated(locations.names, fromLast=TRUE)]
        locations.names <- locations.names[duplicated(locations.names) | duplicated(locations.names, fromLast=TRUE)]
        if(length(locations.names)>4){
          x <- x[-locations]
          collect.deleted <- c(collect.deleted, paste("(IDENTFIER TITLE) ", locations.names, sep=""))
        }
      }




    # PAGES
      # print("pages")
      locations <- NULL
      if(!is.na(nchar(metadata$page[i]))&nchar(metadata$page[i])>=3){
      pattern <- metadata$page[i]
      locations <- grep(pattern, x, ignore.case = TRUE)
      locations.names <- grep(pattern, x, ignore.case = TRUE, value = T)
      locations <- locations[duplicated(locations.names) | duplicated(locations.names, fromLast=TRUE)]
      locations.names <- locations.names[duplicated(locations.names) | duplicated(locations.names, fromLast=TRUE)]
      if(length(locations.names)>4){
        x <- x[-locations]
        collect.deleted <- c(collect.deleted, paste("(IDENTFIER PAGES) ", locations.names, sep=""))
      }
      }

        # pages <- c("615-633", "iii12-iii80", NA, "155")
        # for(i in 1:579){print(stringr::str_detect(metadata$page[i], ".*-.*"))}

      # IDENTIFY SINGLE PAGES - BEWARE ALSO MATCHES TABLE VALUES..
      # print("single pages")
      if(!is.na(metadata$page[i])){
        locations <- NULL
        pages <- metadata$page[i]
        pages <- str_replace_na(pages, replacement = "NA")
        if(stringr::str_detect(pages, "[0-9]+-[0-9]+")){
        pages <- gsub("[A-z]","",pages)
        min <- as.numeric(gsub("-.*","",pages))
        max <- as.numeric(gsub(".*-","",pages))
        if(min>max){ # REPAIR THIS IN THE metadata
        max2 <- max
        max <- min
        min <- max2
        }
        # print(min); print(max); print(pages); print(i)
        pages <- seq(min, max, 1)
        pattern <- paste("^", pages, "$", sep="")
        pattern <- paste(pattern, collapse="|")
        locations <- stringr::str_detect(x, pattern)
        if(length(locations)>6){
          delnames <- x[locations]
          x <- x[!locations]
          collect.deleted <- c(collect.deleted, paste("(IDENTFIER PAGES SINGLE) ", delnames, sep=""))
        }
        }
        }




    # VOLUME, ISSUE, NUMBER
    # print("volume")
    if(!is.na(metadata$volume[i])){
      if(!is.na(metadata$volume[i])){volume.match <- paste("Volume\\s", metadata$volume[i], sep="")}
      if(!is.na(metadata$issue[i])){volume.match <- paste(volume.match, paste("Issue\\s", metadata$issue[i], sep=""), sep="|")}
      pattern <- volume.match
      locations <- grep(pattern, x, ignore.case = FALSE)
      if(length(locations)!=0){
        locations.names <- grep(pattern, x, ignore.case = FALSE, value = T)
        if(length(locations.names)>=5){x <- x[-locations]
        collect.deleted <- c(collect.deleted, paste("(IDENTFIER VOLUME-ISSUE-NUMBER) ", locations.names, sep=""))
      }
    }
}











    # DATES
    # print("dates")
    pattern <- "(January|February|March|April|May|June|July|August|September|October|November|December)\\s[0-9][0-9][0-9][0-9]\\s/\\s[0-9][0-9][0-9]"
    # substr(metadata$date1[i], 1,4)
    locations <- grep(pattern, x, ignore.case = FALSE)
    if(length(locations)!=0){
      locations.names <- grep(pattern, x, ignore.case = FALSE, value = T)
      if(length(locations.names)>=5){x <- x[-locations]
      #print(locations.names)
      } # delete, print lines
      collect.deleted <- c(collect.deleted, paste("(IDENTFIER DATES) ", locations.names, sep=""))
    }




    # AUTHORSNAMES - takes only first one
      # print("authors")
      locations <- NULL
      if(sum(c('given','family') %in% names(metadata$author[[i]]))==2){ # test for presence of 'given' + 'family'
      print(metadata$author[i][[1]]$family); print(i) # print names and index
      if(nchar(metadata$author[i][[1]]$family)>=3){ # text length of family name
        pattern <- paste(metadata$author[i][[1]]$family, collapse="|")
        locations <- grep(pattern, x, ignore.case = TRUE)
        locations.names <- grep(pattern, x, ignore.case = TRUE, value = T)
        locations <- locations[nchar(locations.names)<=45]
        locations.names <- locations.names[nchar(locations.names)<=45]
        if(length(locations.names)>4){
          x <- x[-locations]
          collect.deleted <- c(collect.deleted, paste("(IDENTFIER AUTHORS) ", locations.names, sep=""))
        }
      }
      }else{metadata$author[i] <- NA}


      }



    # Make table of collected running heads
    if(length(collect.deleted)!=0){ # only if running heeads were identified
      name <- stringr::str_extract(file.names[i], ".*[0-9]{4}") # DEPENDENCY ON FILE NAME!
      running.heads <- cbind(name, collect.deleted, i) # problem if collect.deleted is empty
      running.heads <- data.frame(running.heads)
      names(running.heads) <- c("study","running.head", "loop.i")
      emptyrow <- c(NA, NA, NA)
      deleted.runningheads <- rbind(deleted.runningheads, running.heads, emptyrow)
      # Sort according to identifier
      # deleted.runningheads <- dplyr::arrange(deleted.runningheads, running.head, study)
    }




##############################
### DELETE SECTION HEADERS ###
##############################
      # print("section headers")
      locations <- NULL
      pattern <- paste("^CONCLUSION$",
                       "^Conclusion$",
                       "^INTRODUCTION$",
                       "^Introduction$",
                       "^Summary and Conclusions$",
                       sep="|")
      locations <- grep(pattern, x, ignore.case = TRUE)
      locations.names <- grep(pattern, x, ignore.case = TRUE, value = T)
      locations <- locations[nchar(locations.names)<=45]
      locations.names <- locations.names[nchar(locations.names)<=45]
      if(length(locations.names)>4){
        x <- x[-locations]
        collect.deleted <- c(collect.deleted, paste("(IDENTFIER SECTION HEADERS) ", locations.names, sep=""))
      }


      # print(i)

    # Save text
    fileConn<-file(sub(".txt", "_processed.txt", file.paths[i]))
    writeLines(x, fileConn, useBytes = TRUE)
    close(fileConn)




    # counter
    #if(stringr::str_detect(as.character(i), "^.*0$")){cat(i, ".. ", sep="")}

  }

    # Save deleted running heads in table for checking
    print(xtable::xtable(deleted.runningheads),type='html',comment=FALSE, file="./deleted_running_headers.html")

    # Message to user
    cat("\n\n", i, " texts/documents were processed in folder '", folder ,"' !\n\n", sep = "")

    }
