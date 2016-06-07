setwd("/Users/saulgarcia/Desktop/Github/RecommendationSystem")

library(tidyr)
library(data.table)
library(stringr)
library(dplyr)
library(gutenbergr)
library(tm)
library(SnowballC)
library(stringi)
library(stringdist)

### FUNCTIONS ###

#Function to Preprocess Text
clean_text<- function(text)
{
  text = laply(text, function(text)
  {
    # Remove punctuation
    text = gsub('[[:punct:]]', ' ', text)
    # Convert to Latin-ASCII
    text = stri_trans_general(text ,"Latin-ASCII")
    # Convert to lower case:
    text = tolower(text)
    #Stemwords
    text = wordStem(text, language = "english")
    return(text)
  })
  
  ###Stem and remove Stopwords
  stopWords<-stopwords(kind="en")
  #Create function "not in"
  '%nin%' <- Negate('%in%')
  text <-lapply(text, function(x) {
    t <- unlist(strsplit(x, " "))
    t <- wordStem(t, language="english")
    t[t %nin% stopWords]
  })
  
  text <- sapply(text, paste, collapse=" ")
  
  return(text)
}

# Cosine Distance 
cosine_distance<- function(var1,var2){
  x<-stringdist(var1, var2, method= "cosine")
  x[which(x==Inf)] <- 1 
  as.numeric(x)
}

############  DATA ###########
#Dataset of 5 books.
bookIds = c(768, 1260, 888,1010,1020,1200,1300,1400,1500,1600,1800, 1700, 1900,1901:1908)
books <- gutenberg_download(bookIds, meta_fields = "title")

#List of books
books_list <- books %>% group_by(title) %>% distinct() %>% select(title)
books_list

#Preprocessing of text
library(plyr)
books_df = books
books_df$text <- clean_text(books$text)

#Read dictionary
dictionary = read.csv("dictionary.csv", stringsAsFactors = F)
dictionary$WordsStem <- clean_text(dictionary$Words)

#Create a dataframe with Emotion and list of words
Emotions = aggregate(WordsStem~Category, paste, collapse=",",data=dictionary)
Emotions$Category

### Is there a way to do this automatic? If the number of emotions change?
#Generate Cosine distance for each line of the book and

books_df$Anger <- cosine_distance(books_df$text,Emotions[1,2])
books_df$Fear <- cosine_distance(books_df$text,Emotions[2,2])
books_df$Happiness <- cosine_distance(books_df$text,Emotions[3,2])
books_df$Sadness <- cosine_distance(books_df$text,Emotions[4,2])
books_df$ShameAndGuilt <- cosine_distance(books_df$text,Emotions[5,2])

detach(package:plyr)
#Get the total distance for each title
df <- books_df %>% group_by(title) %>% summarise("Anger" = sum(Anger), 
                                           "Fear" = sum(Fear), 
                                           "Happiness" = sum(Happiness), 
                                           "Sadness" = sum(Sadness),
                                          "Shame and Guilt" = sum(ShameAndGuilt))

#Get the name of the minimum distance of each emotion, this will represent the Emotion of the book.
emotionTitle <- apply( as.data.frame(apply(df,1,which.min)) ,2,function(x){names(df)[x]})

#Append The Emotion to the Book dataframe
df$Emotion <- emotionTitle

################# Recommendation System ##################

#User choose a book: (Number from 1 - 11)
booknum = sample(1:nrow(df), 1)

#Get the title and emotion for the choosen book
Userbook <- df[booknum,c(1,7)]
# #Get each as a string variable for filtering
# tit = unlist(Userbook[1]) ; emo = unlist(Userbook[2])
# #Filter the list of books by emotions, and not containing the book read by the user
# subset = df %>% select(title,Emotion) %>%  filter(Emotion == emo & title != tit)
# 
# if(nrow(subset)>=3){
#   #Recommend 3 books
#   subset[sample(nrow(subset)),][1:3,1]  
# } else if(nrow(subset) == 0 ){
#   #Recommend 3 random books
#   subset = df %>% select(title,Emotion) %>%  filter(title != tit)
#   subset[sample(nrow(subset)),][1:3,1] 
# } else{
#   #Recommend the existing books of the category
#   subset[sample(nrow(subset)),][1:nrow(subset),1]
# }
#   

Recommendation <- function(Usersbook, df){
  #Get each as a string variable for filtering
  tit = unlist(Userbook[1]) ; emo = unlist(Userbook[2])
  #Filter the list of books by emotions, and not containing the book read by the user
  subset = df %>% select(title,Emotion) %>%  filter(Emotion == emo & title != tit)
  names(subset) <- c("Recommended","Emotion")
  #For the recomendations, we have 3 scenarios:
 
  if(nrow(subset)>=3){
    #Recommend 3 books
    subset[sample(nrow(subset)),][1:3,1]  
  } else if(nrow(subset) == 0 ){
    #Recommend 3 random books
    subset = df %>% select(title,Emotion) %>%  filter(title != tit)
    subset[sample(nrow(subset)),][1:3,1] 
  } else{
    #Recommend the existing books of the category
    subset[sample(nrow(subset)),][1:nrow(subset),1]
  }
}

#Print Recommendation  
as.data.frame(Recommendation(Usersbook,df))
