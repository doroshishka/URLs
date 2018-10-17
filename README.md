# URLs
Extracting and unshortening URLs from tweets

#Sys.setlocale("LC_CTYPE", "russian")
library(dplyr)
library(tidyr)
library(stringr)
library(tokenizers)

### FUNCTIONS, JUST RUN THESE ###
tokenize_words_mod <- function(x) {
  return(unlist(tokenize_words(x, lowercase = FALSE, strip_punct = FALSE)))} #word tokenizing function

extraire <- function(entree,motif){
  res <- regexec(motif,entree)
  if(length(res[[1]])==2){
    debut <- (res[[1]])[2]
    fin <- debut+(attr(res[[1]],"match.length"))[2]-1
    return(substr(entree,debut,fin))
  }else return(NA)} #function to find url

unshorten <- function(url){
  uri <- getURL(url, header=TRUE, nobody=TRUE, followlocation=FALSE, 
                cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  res <- try(extraire(uri,"\r\nlocation: (.*?)\r\nserver"))
  return(res)} #function to pull out url from html file

###---###---###---### DATA RUNNING STARTS HERE ###---###---###---###

###--- (1) IMPORT csv here
text <- data.frame(read.csv("sockpuppet_euromaidan_corpus.csv", encoding = "UTF-8", header = TRUE, sep = ",")) #imports csv

text_corpus <- data.frame(id = as.character(text$tweet.id), user = as.character(text$user.name), 
                          text = as.character(text$tweet)) #creates new data frame with the data we want

###--- (2) TOKENIZER and UNLISTER
token_full <- lapply(as.character(text_corpus$text), tokenize_words_mod) %>%
  cbind(id = as.character(text_corpus$id)) %>% cbind(user = as.character(text_corpus$user)) %>%
  as.data.frame() #tokenizes the tweets
names(token_full)[1] <- "wordy"

tokenlist <- unnest(token_full, .preserve = c(id, user)) %>% setDT() #unlists each tweet

tokenlist$num <- seq.int(nrow(tokenlist)) #ads a numeric index for each token

###--- (3) URL IDENTIFICATION
wordid <- subset(tokenlist, wordy == "http", select = num) %>%
  mutate(num = num + 6) %>% setDT() #searches for numeric index # with http and the shortener (6 tokens after)

tokenlist[wordid, on = .(num), ':=' (urlyes = TRUE)] #tags tokens with shorteners

url.list <- subset(tokenlist, urlyes == TRUE, select = c(id, user, wordy)) #subsets shorteners
url.list$wordy <- paste("http://t.co/", url.list$wordy, sep = '') #paste http to front to create link

url2 <- flatten(url.list) #flatten list for export

#tic()
#NEWLIST <- lapply(url.list$wordy[1:100], unshorten) #adds the full url 
#toc() #

###--- (4) RETURNS Dataframe of urls
write.csv(url2, "sockpuppet_euromaidan_corpus_url.csv", fileEncoding = "UTF-8")
