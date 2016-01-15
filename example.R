

# Prerequesites: xpdf into folder C:\Program Files\xpdf

  setwd("C:\\Users\\paul\\Desktop\\citationdata")
  library(citations)


##########################################################
##########################################################
# SINGLE STUDY
  extract_text(folder = "Beck 1995", number = 20)
  identify_study(study.title = "What to Do (and Not to Do) with Time-Series Cross-Section Data",
                 folder = "Beck 1995"#
                 ,  number = 20)
  delete_reference_section(folder = "Beck 1995", number = 20)
  delete_running_heads(folder = "Beck 1995", number = 20)
  clean_text(folder = "Beck 1995", number = 20)
  extract_citation_cases(folder = "Beck 1995",
                         authorname = "Beck, Katz",
                         studyyear = "1995",
                         number = 20) # scope = 2


  extract_text(folder = "Beck 1995")
  identify_study(study.title = "What to Do (and Not to Do) with Time-Series Cross-Section Data",
                 folder = "Beck 1995"#
                 )
  delete_reference_section(folder = "Beck 1995")
  delete_running_heads(folder = "Beck 1995")
  clean_text(folder = "Beck 1995")
  extract_citation_cases(folder = "Beck 1995",
                         authorname = "Beck, Katz",
                         studyyear = "1995"
                         ) # scope = 2


  extract_text(folder = "Ermisch 2009")
  identify_study(study.title = "What to Do (and Not to Do) with Time-Series Cross-Section Data",
                 folder = "Ermisch 2009"#
  )
  delete_reference_section(folder = "Ermisch 2009")
  delete_running_heads(folder = "Ermisch 2009")
  clean_text(folder = "Ermisch 2009")
  extract_citation_cases(folder = "Ermisch 2009",
                         authorname = "Ermisch, Gambetta, Laurie, Siedler, Uhrig",
                         studyyear = "2009"
  ) # scope = 2


##########################################################
##########################################################
# SEVERAL STUDIES: LOOP
  # Dataset on studies the citations of which are investigate (source documents)
  publicationdata <- read.table("publicationdata.txt", sep=";",
                                header = T, stringsAsFactors = F)

  for (i in 1:nrow(publicationdata)){

    folder <- publicationdata[i,1]
    study.title <- publicationdata[i,2]
    authorname <- publicationdata[i,3]
    studyyear <- publicationdata[i,4]

    # extract_text(folder = folder)
    # identify_study(study.title = study.title, folder = folder)
    # delete_reference_section(folder = folder)
    # clean_text(folder = folder)
    extract_citation_cases(folder = folder, authorname = authorname, studyyear = studyyear)

  }



##########################################################
##########################################################
# Clean citation cases
  # REPLACE BY A POSTPROCESS FUNCTION
  clean_citation_cases(file = "citation_cases.csv",
                       studyyear = "2013")





