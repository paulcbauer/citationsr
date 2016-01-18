#' Deletes the references section in "_cleaned.txt" files and generates "cleaned_woref.txt" files.
#'
#' @return Uses file name to try to identify if there are any running heads that
#' can be omitted from the document.

delete_running_heads <- function(folder, number=NULL){

  file.names <- dir(paste("./", folder, sep = ""), pattern = ".txt")
  file.paths <- paste(paste("./", folder, "/", sep = ""), file.names, sep="")
  n.docs <- length(file.paths)

  # Specify number of documents
  if(!is.null(number)){n.docs <- number}

  # Loop to omit running heads from text file
  deleted.runningheads <- data.frame(study= NA, running.head = NA)
  for (i in 1:n.docs){# List file names

    con <- file(file.paths[i], encoding = "UTF-8")
    x <- readLines(con)
    close(con)


# IDENTIFY RUNNING TITLES AND THEIR POSITIONS: EXACT PATTERN
    # Short length is one characteristic
    # Repetition is one characteristic

    collect.deleted <- NULL


  # DOWNLOADED FROM, BY ETC.
    pattern <- "Downloaded by|Downloaded from|All use subject to JSTOR Terms and Conditions"
    locations <- grep(pattern, x, ignore.case = FALSE)
    locations.names <- grep(pattern, x, ignore.case = FALSE, value = T)
    if(length(locations.names)>=5){x <- x[-locations]
                                    #print(locations.names)
                                    } # delete, print lines
    collect.deleted <- c(collect.deleted, locations.names)

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
    locations.names <- grep(pattern, x, ignore.case = FALSE, value = T)
    if(length(locations.names)>=5){x <- x[-locations]
    #print(locations.names)
    } # delete, print lines
    collect.deleted <- c(collect.deleted, locations.names)







  # JOURNAL
    # MUST BE WITH JOURNAL NAMES
    pattern <- paste(
      # Energy Policy 69 (2014) 374–38
      "\\w\\s\\w\\s{0,2}[0-9]*\\s{0,2}(\\(|)[0-9]{4}(\\)|)\\s{0,2}[0-9]{0,4}(-|)[0-9]{0,4}",
      # POLITICAL STUDIES: 2015, 63(S1)
      "\\w\\s\\w[:punct:]{0,1}.*[0-9]{1,4},.*[0-9][0-9]\\(S[0-9]*\\)",
      # Public Choice (2007) 133: 321–341
      "\\w.\\w..\\d{1,4}.\\d{1,3}[:punct:].[0-9]{0,4}(-|)[0-9]{0,4}",
      # Comparative Political Studies
      "Comparative Political Studies",
      # American Journal of Political Science, Vol. 55, No. 2, April 2011, Pp. 370–382
      "\\w.\\w.\\w.\\w, Vol\\. [0-9][0-9], No\\..[0-9],.*Pp\\. [0-9]{0,4}(-|)[0-9]{0,4}",
      # West European Politics
      "West European Politics"
      , sep="|")
      locations <- grep(pattern, x, ignore.case = FALSE)
      locations.names <- grep(pattern, x, ignore.case = FALSE, value = T)
      if(length(locations.names)>=5){x <- x[-locations]
      #print(locations.names)
      } # delete, print lines
      collect.deleted <- c(collect.deleted, locations.names)


    # MANUAL
      pattern <- paste(
        # POLITICAL PARTISANSHIP AND WELFARE STATE REFORM
        "POLITICAL PARTISANSHIP AND WELFARE STATE REFORM",
        #CHRISTOPHERJ ANDERSONAND DANIELS WARD
        "CHRISTOPHERJ ANDERSONAND DANIELS WARD",
        # CHHIBBER & MURALI: DUVERGERIAN DYNAMICS
        "CHHIBBER & MURALI: DUVERGERIAN DYNAMICS",
        "KYRIACOU",
        "at European Univ Inst - Library",
        "IMPACT OF PENSION PRIVATIZATION ON FOREIGN DIRECT INVESTMENT",
        "CRENSHAW",
        "DISARMING FEARS OF DIVERSITY",
        "THAMES",
        "MODEL SPECIFICATION, DATA AND OPERATIONALIZATIONS"
        , sep="|")
      locations <- grep(pattern, x, ignore.case = FALSE)
      locations.names <- grep(pattern, x, ignore.case = FALSE, value = T)
      if(length(locations.names)>=5){x <- x[-locations]
      #print(locations.names)
      } # delete, print lines
      collect.deleted <- c(collect.deleted, locations.names)



    # TITLE: Regexp that matches title letters (big or small)
      # REWORK THAT!
      pattern <- stringr::str_extract(file.names[i], "-\\s.*\\ - ")
      pattern <- stringr::str_replace_all(pattern, "-", "")
      pattern <- stringr::str_replace(pattern, "^\\s{1,4}", "")
      pattern <- stringr::str_replace(pattern, "\\s{1,4}$", "")
      if(!is.na(pattern)){
      locations <- grep(pattern, x, ignore.case = FALSE)
      locations.names <- grep(pattern, x, ignore.case = FALSE, value = T)
      if(length(locations.names)>=5){x <- x[-locations]
      #print(locations.names)
      } # delete, print lines
      collect.deleted <- c(collect.deleted, locations.names)
      }

    # Volume or Number
      pattern <- "Volume\\s[0-9]{1,4}|Number\\s[0-9]{1,4}" # 4 enough?
      # |Number|VOLUME|NUMBER
      locations <- grep(pattern, x, ignore.case = FALSE)
      locations.names <- grep(pattern, x, ignore.case = FALSE, value = T)
      if(length(locations.names)>=5){x <- x[-locations]
      #print(locations.names)
      } # delete, print lines
      collect.deleted <- c(collect.deleted, locations.names)


    # Dates
      pattern <- "(January|February|March|April|May|June|July|August|September|October|November|December)\\s[0-9][0-9][0-9][0-9]\\s/\\s[0-9][0-9][0-9]"
      locations <- grep(pattern, x, ignore.case = FALSE)
      locations.names <- grep(pattern, x, ignore.case = FALSE, value = T)
      if(length(locations.names)>=5){x <- x[-locations]
      #print(locations.names)
      } # delete, print lines
      collect.deleted <- c(collect.deleted, locations.names)


    # AUTHOR: so wie oben
      # HAS TO BE ONLY AUTHOR NAMES IN LINE!
      # pattern <- stringr::str_extract(file.names[i], "^[:alpha:]+")
      pattern <- "Martin Bisgaard"
      locations <- grep(pattern, x, ignore.case = TRUE)
      locations.names <- grep(pattern, x, ignore.case = FALSE, value = T)
      if(length(locations.names)>=5){x <- x[-locations]
      #print(locations.names)
      } # delete, print lines
      collect.deleted <- c(collect.deleted, locations.names)

      if(length(collect.deleted)!=0){ # only if running heeads were identified
        name <- stringr::str_extract(file.names[i], "^[:alpha:]+\\s[0-9]{4}|^[:alpha:]+\\set\\sal.\\s[0-9]{4}|^[:alpha:]+\\sand\\s[:alpha:]+\\s[0-9]{4}")
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


  write.table(collect.deleted, file=paste("./", folder, "_deleted_running_headers.html", sep = ""))
  print(xtable::xtable(deleted.runningheads),type='html',comment=FALSE, file=paste("./", folder, "_deleted_running_headers.html", sep = ""))


  }
