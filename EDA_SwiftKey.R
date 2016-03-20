library(tm)
library(wordcloud)
library(RWeka)
library(slam)

## Function to read an input file
readInputFile <- function(filename) {
  f <- filename
  conn <- file(f,open="r")
  l <- readLines(conn)
  for (i in 1:length(l)) {
    print(l[i])
  }
  close(conn)
}

## Function to examine the first n
## lines of the specified file
headInputFile <- function(filename, n) {
  f <- filename
  conn <- file(f,open="r")
  l <- readLines(conn)
  for (i in 1:n) {
    print(l[i])
  }
  close(conn)
}

## Function to build a small corpus
## from the input file name.
buildSmallCorpus <- function(filename, n) {
  f <- filename
  conn <- file(f,open="r")
  l <- readLines(conn)
  textVector <- character(0)
  for (i in 1:n) {
    textVector <- c(textVector, l[i])
  }
  close(conn)
  ## Make the swiftKey Small Corpus global
  swiftKeySmallCorpus <- Corpus(VectorSource(textVector))
  
  return (swiftKeySmallCorpus)
}

## Main function.Embed in rPubs
mainEDA <- function() {
  blogsCorpus <- buildSmallCorpus("./final/en_US/en_US.blogs.txt",4000)
  newsCorpus <- buildSmallCorpus("./final/en_US/en_US.news.txt",4000)
  twitterCorpus <- buildSmallCorpus("./final/en_US/en_US.twitter.txt",4000)
  
  ## Create the main corpus
  corpus <- c(blogsCorpus, newsCorpus, twitterCorpus)
  
  ## Apply transformations to the corpus
  corpus <- tm_map(corpus, removeNumbers)
  ## Make terms lower case
  corpus <- tm_map(corpus, content_transformer(tolower))
  ## Remove extra white space ( result would be
  ## just a single white space )
  corpus <- tm_map(corpus, stripWhitespace)
  ## Remove punctuations
  corpus <- tm_map(corpus, removePunctuation)
  
  ## Create a Term Document Matrix
  termDocMatrix <- TermDocumentMatrix(corpus)
  
  ## Find words that occur atleast 1000 times
  ## in the corpus
  findFreqTerms(termDocMatrix, 1000)
  
  ## Remove the sparse terms
  termDocMatrix <- removeSparseTerms(termDocMatrix, 0.99)
  
  ## Construct a 1-gram wordcloud
  m = as.matrix(termDocMatrix)
  v = sort(rowSums(m),decreasing=TRUE)
  d = data.frame(word = names(v),freq=v)
  
  pal = brewer.pal(9,"Reds")
  wordcloud(words = d$word,
            freq = d$freq,
            scale = c(8,.8),
            random.order = F,
            colors = pal)
  
  tdmTemp <- inspect(termDocMatrix)
  tdmTemp[tdmTemp>=1] <- 1
  termTermMatrix <<- tdmTemp %*% t(tdmTemp)
  
  ## Construct a 2-gram wordcloud
  options(mc.cores=1)
  BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
  
  termDocMatrix2g <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer))
  
  termDocMatrix2g <- removeSparseTerms(termDocMatrix2g, 0.99)
  
  m = as.matrix(termDocMatrix2g)
  v = sort(rowSums(m),decreasing=TRUE)
  d = data.frame(word = names(v),freq=v)
  
  pal = brewer.pal(9,"Reds")
  wordcloud(words = d$word,
            freq = d$freq,
            scale = c(8,.8),
            random.order = F,
            colors = pal)
  
  ## Construct a 3-gram wordcloud
  options(mc.cores=1)
  TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
  
  termDocMatrix3g <- TermDocumentMatrix(corpus, control = list(tokenize = TrigramTokenizer))
  
  termDocMatrix3g <- removeSparseTerms(termDocMatrix3g, 0.99)
  
  #m = as.matrix(termDocMatrix3g)
  m <- rollup(termDocMatrix3g, 2, na.rm=TRUE, FUN = sum)
  v = sort(rowSums(as.matrix(m)),decreasing=TRUE)
  d = data.frame(word = names(v),freq=v)
  
  pal = brewer.pal(9,"Reds")
  wordcloud(words = d$word,
            freq = d$freq,
            scale = c(8,.8),
            min.freq = 2,
            random.order = F,
            colors = pal)
}

