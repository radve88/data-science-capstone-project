# set jvm size and load libraries
options( java.parameters = "-Xmx4g" )
library(tm)
library(ggplot2)

library(RWeka)
library(R.utils)
library(dplyr)
library(parallel)
library(wordcloud)

connect1 <- file("en_US.twitter.txt", open = "rb")
twitter <- readLines(connect1, skipNul = TRUE, encoding="UTF-8")
close(connect1)

connect2 <- file("en_US.news.txt", open = "rb")
news <- readLines(connect2, skipNul = TRUE, encoding="UTF-8")
close(connect2)

connect3 <- file("en_US.blogs.txt", open = "rb")
blogs <- readLines(connect3, skipNul = TRUE, encoding="UTF-8")
close(connect3)

sampletext <- function(textbody, proportion) {
  taking <- sample(1:length(textbody), length(textbody)*proportion)
  Sampletext <- textbody[taking]
  Sampletext
}

# extract samples
set.seed(12345)
proportion <- 25/50
STwitter <- sampletext(twitter, proportion)
SBlog <- sampletext(blogs, proportion)
SNews <- sampletext(news, proportion)

# combine all samples
SampleCombine <- c(SBlog, SNews, STwitter)

# write combined data to file
writeLines(SampleCombine, "./SampleCombine/SampleCombine.txt")


## Clean Data



cleansing <- function (textcp) {
  textcp <- tm_map(textcp, content_transformer(tolower))
  textcp <- tm_map(textcp, stripWhitespace)
  textcp <- tm_map(textcp, removePunctuation)
  textcp <- tm_map(textcp, removeNumbers)
  textcp
}

SampleCombine <- VCorpus(DirSource("./SampleCombine", encoding = "UTF-8"))

# tokenizing sampled text 
SampleCombine <- cleansing(SampleCombine)

# Define function to make N grams
Ngram <- function (textcp, n) {
  NgramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = n, max = n))}
  ngram <- TermDocumentMatrix(textcp, control = list(tokenizer = NgramTokenizer))
  ngram
}

# Define function to extract the N grams and sort
ngram_sorted_df <- function (ngram) {
  ngram_m <- as.matrix(ngram)
  ngram_df <- as.data.frame(ngram_m)
  colnames(ngram_df) <- "Count"
  ngram_df <- ngram_df[order(-ngram_df$Count), , drop = FALSE]
  ngram_df
}

# Calculate N-Grams
gram1 <- Ngram(SampleCombine, 1)
gram2 <- Ngram(SampleCombine, 2)
gram3 <- Ngram(SampleCombine, 3)
gram4 <- Ngram(SampleCombine, 4)


# Extract term-count tables from N-Grams and sort 
gram1_df <- ngram_sorted_df(gram1)
gram2_df <- ngram_sorted_df(gram2)
gram3_df <- ngram_sorted_df(gram3)
gram4_df <- ngram_sorted_df(gram4)

# Save data frames into r-compressed files

quadgram <- data.frame(rows=rownames(gram4_df),count=gram4_df$Count)
quadgram$rows <- as.character(quadgram$rows)
quadgram_split <- strsplit(as.character(quadgram$rows),split=" ")
quadgram <- transform(quadgram,first = sapply(quadgram_split,"[[",1),second = sapply(quadgram_split,"[[",2),third = sapply(quadgram_split,"[[",3), fourth = sapply(quadgram_split,"[[",4))
quadgram <- data.frame(unigram = quadgram$first,bigram = quadgram$second, trigram = quadgram$third, quadgram = quadgram$fourth, freq = quadgram$count,stringsAsFactors=FALSE)
write.csv(quadgram[quadgram$freq > 1,],"./ShinyApp/quadgram.csv",row.names=F)
quadgram <- read.csv("./ShinyApp/quadgram.csv",stringsAsFactors = F)
saveRDS(quadgram,"./ShinyApp/quadgram.RData")


trigram <- data.frame(rows=rownames(gram3_df),count=gram3_df$Count)
trigram$rows <- as.character(trigram$rows)
trigram_split <- strsplit(as.character(trigram$rows),split=" ")
trigram <- transform(trigram,first = sapply(trigram_split,"[[",1),second = sapply(trigram_split,"[[",2),third = sapply(trigram_split,"[[",3))
trigram <- data.frame(unigram = trigram$first,bigram = trigram$second, trigram = trigram$third, freq = trigram$count,stringsAsFactors=FALSE)
write.csv(trigram[trigram$freq > 1,],"./ShinyApp/trigram.csv",row.names=F)
trigram <- read.csv("./ShinyApp/trigram.csv",stringsAsFactors = F)
saveRDS(trigram,"./ShinyApp/trigram.RData")


bigram <- data.frame(rows=rownames(gram2_df),count=gram2_df$Count)
bigram$rows <- as.character(bigram$rows)
bigram_split <- strsplit(as.character(bigram$rows),split=" ")
bigram <- transform(bigram,first = sapply(bigram_split,"[[",1),second = sapply(bigram_split,"[[",2))
bigram <- data.frame(unigram = bigram$first,bigram = bigram$second,freq = bigram$count,stringsAsFactors=FALSE)
write.csv(bigram[bigram$freq > 1,],"./ShinyApp/bigram.csv",row.names=F)
bigram <- read.csv("./ShinyApp/bigram.csv",stringsAsFactors = F)
saveRDS(bigram,"./ShinyApp/bigram.RData")
