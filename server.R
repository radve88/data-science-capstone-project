

suppressWarnings(library(tm))
suppressWarnings(library(stringr))
suppressWarnings(library(shiny))

# Load Quadgram,Trigram & Bigram Data frame files

quadgram <- readRDS("quadgram.RData");
trigram <- readRDS("trigram.RData");
bigram <- readRDS("bigram.RData");
mesg <<- ""

# Cleaning of user input before predicting the next word

Predict <- function(x) {
  clean_data <- removeNumbers(removePunctuation(tolower(x)))
  split_data <- strsplit(clean_data, " ")[[1]]
  
  if (length(split_data)>= 3) {
    split_data <- tail(split_data,3)
    if (identical(character(0),head(quadgram[quadgram$unigram == split_data[1] & quadgram$bigram == split_data[2] & quadgram$trigram == split_data[3], 4],1))){
      Predict(paste(split_data[2],split_data[3],sep=" "))
    }
    else {mesg <<- "4-gram used to predict next word"; head(quadgram[quadgram$unigram == split_data[1] & quadgram$bigram == split_data[2] & quadgram$trigram == split_data[3], 4],1)}
  }
  else if (length(split_data) == 2){
    split_data <- tail(split_data,2)
    if (identical(character(0),head(trigram[trigram$unigram == split_data[1] & trigram$bigram == split_data[2], 3],1))) {
      Predict(split_data[2])
    }
    else {mesg<<- "3-gram used to predict next word"; head(trigram[trigram$unigram == split_data[1] & trigram$bigram == split_data[2], 3],1)}
  }
  else if (length(split_data) == 1){
    split_data <- tail(split_data,1)
    if (identical(character(0),head(bigram[bigram$unigram == split_data[1], 2],1))) {mesg<<-"No match found. Most common word 'the' is returned."; head("the",1)}
    else {mesg <<- "2-gram used to predict next word"; head(bigram[bigram$unigram == split_data[1],2],1)}
  }
}


shinyServer(function(input, output) {
    output$prediction <- renderPrint({
    result <- Predict(input$inputString)
    output$text2 <- renderText({mesg})
    result
  });
  
  output$text1 <- renderText({
    input$inputString});
}
)