#' Extracts text from PDFs and stores in folder of choice.
#'
#' @return Takes PDFs in folder "from" and extracts .txt files that are copied into folder "to".
#' @param from Folder with .pdf files.
#' @param to Folder with .txt files.
#' @param number Number of PDFs you want to apply the function to. Default is "all PDFs in folder".
#' @param method Which method to use. Default is pdftotext.exe.
#'
#' @examples
#' \dontrun{
#'  folder <- "Acemoglu 2001"
#'  extract_text(folder)
#' }


extract_text <- function(from, to, number = NULL, method = "pdftools", path.pdftotext = NULL) {

# Packages ####


# Measure time ####
  ptm <- proc.time()


# Empty "to" folder if present
  if(dir.exists(paste(to, sep = ""))==TRUE){
    unlink(paste(to, sep = ""), recursive = T)} # delete files in folder

# Create "to" folder if not present
  if(dir.exists(paste(to, sep = ""))==FALSE){ # create folder if it does not exist
    dir.create(paste(to, sep = ""))}



# List of all .pdf files ####
  if(!is.null(number)){file.names <- list.files(path = from, pattern = "pdf", full.names = FALSE)[1:number]}else{
    file.names <- list.files(path = from, pattern = "pdf", full.names = FALSE)
  }

# Method: Pdftotext ####
if(method=="pdftotext"){
  extracttext <- function(i)
  {i1 <- paste(from, "/", i, sep="")
   i2 <- gsub(".pdf", ".txt", paste(to, "/", i, sep=""))
    system(paste(paste('"', path.pdftotext, '"', sep=""),
                 paste0('"', i1, '"'), paste0('"', i2, '"')), wait = TRUE); print(i)
    }
  lapply(file.names, extracttext)
}




# Method: Pdftools ####
  if(method=="pdftools"){
    require(pdftools)
      extracttext.pdftools <- function(i) {
      text <- pdf_text(paste(from, "/", i, sep=""))
      #text <- paste(text, collapse = "\n NEWPAGE \n\n") # to collapse pages
      write(text, paste(to, "/", substr(i, 1, nchar(i)-4), ".txt", sep=""))
      print(i)
      }
    lapply(file.names, extracttext.pdftools)
  }


  # Measure time
  time <- proc.time() - ptm

  # Messages for user
  #cat("\n\n For the folder '", folder, "' text extraction too around ", as.numeric(time[3]), " second(s) this is around ", round(as.numeric(time[3])/60,2), " minutes.\n", sep = "")
  #cat("\n", n.docs, " PDFs where just sequeezed to release their text in folder '", folder, "'!\n\n", sep = "")
  #cat(length(dir(paste(folder, "/text extraction error", sep=""))),
  #   " PDF file(s) were problematic and copied to the 'text extraction error' folder.\n\n" , sep = "")
}












