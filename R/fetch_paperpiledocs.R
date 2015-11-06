#' Fetches documents from paperpile gdrive folder and copies to folder.
#'
#' @param paperpile.directory The directory of the paperpile folder.
#' @return Returns a .ris file that can be imported into paperpile.


fetch_paperpiledocs <- function(paperpile.directory) {
  folders <- list.files()
  for(z in 1:length(folders)){

  ptm <- proc.time()
  x <- dir(paperpile.directory, recursive = T)

  citations <- read.table(paste(folders[z], "/citations.csv", sep=""), header=TRUE, stringsAsFactors = F)
  # Loop to put PDF files in one folder
  # Finds PDF files in paperpile folder
  # Copies them to a new folder

  for (i in 1:nrow(citations)){
    AU <- paste("(", stringr::str_replace(stringr::str_extract(citations$author[i], "[:alpha:]*,"), ",", ""), ")", sep="")
    PY <- paste("(", citations$year[i], ")", sep="")

    # identify articles
    file.path.half <- x[stringr::str_detect(x, AU)&stringr::str_detect(x, PY)]

    # paperpile folder where file is
    from <- paste(paperpile.directory, file.path.half, sep="")

    working.directory <- getwd()

    # create documents folder
    dir.create(paste(folders[z], "/documents/", sep = ""))

    to <- paste(working.directory, paste("/", folders[z], sep=""), "/documents/",
                stringr::str_replace(file.path.half, "All Papers/[A-Z]/", ""), sep="")
    file.copy(from, to, overwrite = T, recursive = FALSE,
              copy.mode = TRUE)
  }
  time <- proc.time() - ptm
  cat("\n Wow.. copying took only ", as.numeric(time[3]), " second(s) for ", folders[z], ". Amazing!", sep = "")
  }
}

