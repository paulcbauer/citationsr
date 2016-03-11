#' Returns and saves text files without running heads.
#'
#' @param folder Name of folder within working directory in which the citing documents (.txt files) are located, e.g. "Beck 1995".
#' @param number Number of .txt files in folder the function should be applied to. Default is "all .txt files in folder".
#' @return Returns and saves text files without running heads.


delete_running_heads <- function(folder, number=NULL){

# Get medadatafile for folder/study
  load(paste("./", folder, "_metadata.RData", sep="")) # load RData file
  metadata <- foo

# List file names in folder (ONLY .TXT FILES)
  file.names <- dir(paste("./", folder, sep = ""), pattern = ".txt")

# Generate file paths
  file.paths <- paste(paste("./", folder, "/", sep = ""), file.names, sep="")

# Count number of files in folder
  n.docs <- length(file.paths)

# Specify number of documents to assess by setting n.docs
  if(!is.null(number)){n.docs <- number}

# Loop over .txt files one by one (until document nr. "number" = n.docs)
  deleted.runningheads <- data.frame(study= NA, running.head = NA)
  for (i in 1:n.docs){# List file names

    con <- file(file.paths[i], encoding = "UTF-8")
    x <- readLines(con)
    close(con)


  # Delete empty lines in text files
    x <- x[nzchar(x)]


  # Identify running titles
    # Short length is one characteristic
    # Repetition is one characteristic

  # Store deleted running heads here for check later on
    collect.deleted <- NULL


  # DOWNLOADED FROM, BY ETC.
    pattern <- "Downloaded by|Downloaded from|All use subject to JSTOR Terms and Conditions"
    locations <- grep(pattern, x, ignore.case = TRUE)
    if(length(locations)!=0){
    locations.names <- grep(pattern, x, ignore.case = TRUE, value = T)
    if(length(locations.names)>=5){x <- x[-locations]}
    collect.deleted <- c(collect.deleted, locations.names)
    }


  # COPYRIGHT MESSAGES
    pattern <- paste("All use subject to JSTOR Terms and Conditions",
                     # © 2015 The Authors. Political Studies © 2015 Political Studies Association
                    "\\s[0-9]{4}\\sThe\\sAuthors",
                    # Midwest Political Science Association
                    "\\w\\sPolitical Science Association",
                    # Blackwell Publishing Ltd
                    "Blackwell Publishing Ltd"
                    , sep="|")
    locations <- grep(pattern, x, ignore.case = FALSE)
    if(length(locations)!=0){
    locations.names <- grep(pattern, x, ignore.case = FALSE, value = T)
    if(length(locations.names)>=5){x <- x[-locations]}
    collect.deleted <- c(collect.deleted, locations.names)
    }



  # PUBLISHER
    pattern <- metadata$publisher[i]
    locations <- grep(pattern, x, ignore.case = TRUE)
    if(length(locations)!=0){
    locations.names <- grep(pattern, x, ignore.case = TRUE, value = T)
    if(length(locations.names)>=5){x <- x[-locations]}
    collect.deleted <- c(collect.deleted, locations.names)
    }


  # JOURNAL
    pattern <- metadata$journal[i]
    locations <- grep(pattern, x, ignore.case = TRUE)
    if(length(locations)!=0){
    locations.names <- grep(pattern, x, ignore.case = TRUE, value = T)
    if(length(locations.names)>=5){x <- x[-locations]}
    collect.deleted <- c(collect.deleted, locations.names)
    }

  # TITLE
    title.match <- metadata$title[i]
    title.match <- stringr::str_extract(title.match, "^\\s*[:word:]*\\s[:word:]*\\s[:word:]*\\s[:word:]*")
    pattern <- title.match
    locations <- grep(pattern, x, ignore.case = FALSE)
    if(length(locations)!=0){
    locations.names <- grep(pattern, x, ignore.case = FALSE, value = T)
    if(length(locations.names)>=5){x <- x[-locations]}
    collect.deleted <- c(collect.deleted, locations.names)
    }

  # AUTHORS
    # authors.match <- metadata$author[i]
    # authors.match <- authors.match[[1]]$family[1]
    # pattern <-
    # locations <- grep(pattern, x, ignore.case = FALSE)
    # locations.names <- grep(pattern, x, ignore.case = FALSE, value = T)
    # if(length(locations.names)>=5){x <- x[-locations]}
    # collect.deleted <- c(collect.deleted, locations.names)


  # PAGES
    pages.match <- metadata$page[i]
    pattern <- pages.match
    locations <- grep(pattern, x, ignore.case = FALSE)
    if(length(locations)!=0){
    locations.names <- grep(pattern, x, ignore.case = FALSE, value = T)
    if(length(locations.names)>=5){x <- x[-locations]}
    collect.deleted <- c(collect.deleted, locations.names)
    }




  # VOLUME, ISSUE, NUMBER
    if(!is.na(metadata$volume[i])){volume.match <- paste("Volume\\s", metadata$volume[i], sep="")}
    if(!is.na(metadata$issue[i])){volume.match <- paste(volume.match, paste("Issue\\s", metadata$issue[i], sep=""), sep="|")}
    pattern <- volume.match
    locations <- grep(pattern, x, ignore.case = FALSE)
    if(length(locations)!=0){
    locations.names <- grep(pattern, x, ignore.case = FALSE, value = T)
    if(length(locations.names)>=5){x <- x[-locations]
    #print(locations.names)
    } # delete, print lines
    collect.deleted <- c(collect.deleted, locations.names)
    }


  # MANUAL PATTERNS IDENTIFIED IN CITATION CASE DATA
      # pattern <- paste(
      # "POLITICAL PARTISANSHIP AND WELFARE STATE REFORM",
      #  "CHRISTOPHERJ ANDERSONAND DANIELS WARD",
      #  "CHHIBBER & MURALI: DUVERGERIAN DYNAMICS",
      #  "KYRIACOU",
      #   "at European Univ Inst - Library",
      #   "IMPACT OF PENSION PRIVATIZATION ON FOREIGN DIRECT INVESTMENT",
      #   "CRENSHAW",
      #   "DISARMING FEARS OF DIVERSITY",
      #   "THAMES",
      #   "MODEL SPECIFICATION, DATA AND OPERATIONALIZATIONS"
      #   , sep="|")
      # locations <- grep(pattern, x, ignore.case = FALSE)
      # locations.names <- grep(pattern, x, ignore.case = FALSE, value = T)
      # if(length(locations.names)>=5){x <- x[-locations]
      #print(locations.names)
      # } # delete, print lines
      #   collect.deleted <- c(collect.deleted, locations.names)








    # DATES
      pattern <- "(January|February|March|April|May|June|July|August|September|October|November|December)\\s[0-9][0-9][0-9][0-9]\\s/\\s[0-9][0-9][0-9]"
      # substr(metadata$date1[i], 1,4)
      locations <- grep(pattern, x, ignore.case = FALSE)
      if(length(locations)!=0){
      locations.names <- grep(pattern, x, ignore.case = FALSE, value = T)
      if(length(locations.names)>=5){x <- x[-locations]
      #print(locations.names)
      } # delete, print lines
      collect.deleted <- c(collect.deleted, locations.names)
      }


    # Make table of collected running heads
      if(length(collect.deleted)!=0){ # only if running heeads were identified
        name <- paste(metadata$author[i][[1]]$family[1], metadata$doc_year[1], sep=" ")
        running.heads <- cbind(name, collect.deleted) # problem if collect.deleted is empty
        running.heads <- data.frame(running.heads)
        names(running.heads) <- c("study","running.head")
        deleted.runningheads <- rbind(deleted.runningheads, running.heads)
      }



      # Save text
        fileConn<-file(file.paths[i])
        writeLines(x, fileConn, useBytes = TRUE)
        close(fileConn)


      # Counter
      if(stringr::str_detect(as.character(i), "[0-9]*0")){cat(i, ".. ", sep="")}

  }

  # Save deleted running heads in table for checking
  write.table(collect.deleted, file=paste("./", folder, "_deleted_running_headers.html", sep = ""))
  print(xtable::xtable(deleted.runningheads),type='html',comment=FALSE, file=paste("./", folder, "_deleted_running_headers.html", sep = ""))
  }
