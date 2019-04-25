library(tm)
library(wordcloud)
library(memoise)

# The list of valid books
seasons <<- list(unique(data$season))

# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(season) {

  if (!(season %in% seasons))
    stop("Unknown season")
  
  words <- data$text[data$season == season]
  
  myCorpus = Corpus(VectorSource(words))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})
