#### imports ####
source("R/fn.R")


#### load data ####
df<-load_data()



#### data prep ####
empty_submissions<-c("R_62EVZu8bNfOI8Mu","R_1IeHLCoHrVAp7sl")
df<-df |> filter(!ResponseId%in%empty_submissions)
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
v<-c("defis","enjeux","idees","commentaire")[1] # toggle

# Tokenize and remove stop words
(word_freq <- df |> 
  unnest_tokens("word", v) |> 
  anti_join(rbind(stop_fr,stop_add), by = "word") |> 
  count(word, sort = TRUE))
word_freq |> copy()

# Create word cloud
wordcloud(words = word_freq$word,
          freq = word_freq$n,
          min.freq = 1,
          colors = brewer.pal(8, "Dark2"),
          random.order = FALSE)



