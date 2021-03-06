#' Conducts topic analysis of citations of a given article
#'
#' @param file 'citation_data.csv' that contains data on the citation cases.
#' @param article Name of cited article; e.g. Fearon (2003)
#' @param output folder where topics object, figures, and tables generated by function will be stored
#' @param K number of topics to be computed. If more than one number, will run models with all and then
#' choose that with the highest semantic coherence.
#' @param runs total number of STM runs in the cast net stage
#' @param max.em.its maximum number of EM iterations in STM
#' @param net.max.em.its maximum number of EM iterations when casting the net with STM.
#' @param seed seed for random number generator in STM.
#'
#'
#' @examples
#' \dontrun{
#'  setwd("C:/Users/Paul/GDrive/Research/2016_06_Quality_of_citations")
#'  file <- "data/AcemogluJohnsonRobinson_2001_citation_cases.csv"
#'  article <- "Acemoglu, Johnson & Robinson (2001)"
#'  output <- "output/acemoglu_2001"
#'  topic_analysis(file, article, output)
#' }

topic_analysis <- function(file, article, output, K=3:8, runs=2, max.em.its=10, net.max.em.its=2, seed=777){

  require(ggplot2)
  require(scales)
  require(quanteda)
  require(stm)

  # precleaning file
  text <- scan(file, what="character", sep="\n")
  text <- gsub('\\\\"', "''", text)
  text <- paste0(text, collapse="\n")
  tmp <- tempfile()
  writeLines(text, con=tmp)

  # reading file and cleaning data
  tf <- read.csv(tmp, stringsAsFactors=F, row.names=NULL)
  # extracting year
  tf$year <- as.numeric(gsub('.*([0-9]{4}).*', tf$document, repl='\\1'))
  message("Warning: ", sum(is.na(tf$year)), " citation cases with missing year will be excluded from analysis.")
  tf <- tf[!is.na(tf$year),] # deleting citations with empty years
  message("Warning: ", sum(duplicated(tf$citation.case)), " duplicated citation cases will be excluded from analysis.")
  tf <- tf[!duplicated(tf$citation.case),] # deleting duplicates
  message("Warning: ", sum(nchar(tf$citation.case)>1000), " citation cases longer than 1000 characters will be excluded from analysis.")
  tf <- tf[nchar(tf$citation.case)<=1000,] # deleting duplicates
  message("A total of ", nrow(tf), " citation cases will be included in the analysis.")

   # text cleaning
  authors <- tokens(char_tolower(c(tf$document, article)), remove_punct=T, remove_numbers=T)
  authors <- unique(unlist(authors))
  # tokenizing
  tokens <- tokens(char_tolower(tf$citation.case), remove_punct=T, remove_numbers=T)
  # removing stopwords, author names, and other frequent words
  tokens <- tokens_remove(tokens,
                          patter = c(stopwords("english"), "other", "others", "see", "also", "u", authors))
  # stemming?
  #tokens <- lapply(tokens, wordstem)
  # creating n-grams
  ngrams <- tokens_ngrams(tokens, n = 1)
  # putting it all back together...
  ngrams <- unlist(lapply(ngrams, paste, collapse=" "))
  # constructing the DFM
  cit <- corpus(ngrams)
  docnames(cit) <- paste0(1:nrow(tf), '_', tf$document)
  # summary(cit)
  citmat <- dfm(cit)
  # converting to STM format
  citstm <- convert(citmat, to="stm")

  # removing documents with length 0
  todelete <- which(unlist(lapply(citstm$documents, length))==0)
  if (length(todelete)>0){
    message(length(todelete), " documents with no words were also excluded from the analysis.")
    tf <- tf[-todelete,]
    citstm$documents <- citstm$documents[-todelete]
  }

  # preprocessing
  out <- prepDocuments(citstm$documents, citstm$vocab,
		tf[,c("year", "document", "citation.case")])

  # running topic model
  if (length(K)>1){
		manymodels <- manyTopics(out$documents, out$vocab, K=K, verbose=F,
			LDAbeta=F, sigma.prior=0.5, seed=seed, runs=runs, max.em.its=max.em.its, net.max.em.its=net.max.em.its)
		# finding right number of topics
		chosen <- which.max(unlist(lapply(manymodels$semcoh, mean)))
		model <- manymodels$out[[chosen]]
  }
  if (length(K)==1){
		models <- selectModel(out$documents, out$vocab, K=K, verbose=F,
			LDAbeta=F, sigma.prior=0.5, seed=seed, runs=runs, max.em.its=max.em.its, net.max.em.its=net.max.em.its)
		# finding best model run
		chosen <- which.max(unlist(lapply(models$semcoh, mean)))
		model <- models$runout[[chosen]]
  }

  # table with words most associated with each topic
  	words <- labelTopics(model, n=20)
	topictab <- data.frame(
  		"Topic" = 1:K[chosen],
  		"Words" = apply(words$lift, 1, paste, collapse=", "))
	tab <- xtable::xtable(topictab)
	ftab <- paste0(output, '/topic-words.tex')
	writeLines(print(tab), con=file(ftab))
	message("File generated: ", ftab)

	# table with representative citations
	df <- findThoughts(model, paste0(out$meta$document, ': ', out$meta$citation.case), n=1)
	topictab <- data.frame(
  		"Topic" = 1:K[chosen],
  		"Representative document" = unlist(df$docs))
	names(topictab)[2] <- "Representative document"
	tab <- xtable::xtable(topictab)
	ftab <- paste0(output, '/topic-documents.tex')
	writeLines(print(tab), con=file(ftab))
	message("File generated: ", ftab)

	# topics over time
	topics <- data.frame(
		prop = c(model$theta),
		document = rep(1:nrow(out$meta), times=K[chosen]),
		topic = rep(1:K[chosen], each=nrow(out$meta)),
		year = rep(out$meta$year, times=K[chosen]))
	agg <- aggregate(topics$prop,
		by=list(topic=factor(topics$topic), year=topics$year),
		FUN=mean)
	names(agg)[names(agg)=="x"] <- "prop"

	p <- ggplot(agg, aes(x=year, y=prop, group=topic, color=topic))
	pq <- p + geom_line() + theme_minimal() +
		theme(axis.title.x=element_blank()) +
		scale_y_continuous("Average topic proportion", labels=percent)
  	fplot <- paste0(output, '/topic-proportions.pdf')
  	ggsave(pq, file=fplot, height=4, width=6)
  	message("File generated: ", fplot)



}



