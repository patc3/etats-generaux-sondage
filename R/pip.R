#### imports ####
source("R/fn.R")


#### load data ####
df<-load_data()



#### data prep ####
df<-df |> filter(ResponseId!="R_62EVZu8bNfOI8Mu") # empty submission
df$interet_1<-df$interet_1-1

##### combine text vars ####
df$defis<-df[,"defis_recherche_"%p%1:3] |> do.call(what=paste)
df$enjeux<-df["enjeux_"%p%1:3] |> do.call(what=paste)


#### export ####




#### word freq ####

# Get French stop words
stop_fr <- data.frame(word = stopwords("fr", source = "stopwords-iso"))

# more stop words
stop_add<-c("recherche")

# choose var
v<-c("enjeux","defis","idees","commentaire")[2] # toggle

# Tokenize and remove stop words
(word_freq <- df |> 
  unnest_tokens("word", v) |> 
  anti_join(rbind(stop_fr,stop_add), by = "word") |> 
  count(word, sort = TRUE))

# Create word cloud
wordcloud(words = word_freq$word,
          freq = word_freq$n,
          min.freq = 1,
          colors = brewer.pal(8, "Dark2"),
          random.order = FALSE)



