

social.science.journals.psc <- data.frame(XML::readHTMLTable("http://www.psc.isr.umich.edu/dis/infoserv/journal/list.html"))
write.table(social.science.journals.psc, "social.science.journals.psc.csv", sep=",")

journals <- read.table(".\\journals\\scimagojr - all journals_w.csv",sep=";")
journals <- read.table(".\\journals\\scimagojr - all journals_w.txt",sep="\t")
