#install.packages("tidytext")
library(tidyverse)
library(stringr)
library(xml2)
library(rvest)
library(rebus)
library(lubridate)
library(tidytext)
library(ggplot2)

source("script/fonction_extract_USdata.R")

df_speech_ids <- read.csv("URL_discours_voeux.txt", header = F)
df_speech_ids <- rename(df_speech_ids, url_speech = V1)

# création du dataframe qui va recevoir les sentiments
sentiments <- data_frame()

### FONCTIONS ---------------------------------------
get_speech <- function(html){
  html %>% 
    # The relevant tag
    html_nodes('.col1') %>%      
    html_text() %>% 
    # Trim additional white space
    str_trim() %>%                       
    # Convert the list into a vector
    unlist()                             
}

get_speech_sentiment <- function(url)  {
  
  speech_page <- read_html(url)
  text_reviews <- get_speech(speech_page)
  # sub will remove everything before the regular expression mentioned. Text will start with "Française,..."
  president_name <- gsub(".*fonction : (.+). \nFRANCE.*", "\\1", text_reviews)
  speech_date <- str_extract(text_reviews, "décembre \\d\\d\\d\\d")
  speech_date <- substring(speech_date, 9)
  text_df <- sub("^.*\\nti : ", "", text_reviews)
  
  # Tokenization
  tokens <- data_frame(text = text_df) %>% unnest_tokens(word, text)
  
  # import lexicon of french negative and positive word
  sentiment_word_fr <- read.csv("glossaire_sentiments_fr.csv", header = T, fileEncoding = "utf-8")
  
  sentiment <- tokens %>%
    inner_join(sentiment_word_fr) %>% # pull out only sentiment words
    count(sentiment) %>% # count the # of positive & negative words
    spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
    mutate(sentiment_FR = positive - negative) %>% # # of positive words - # of negative words
    mutate(file = url) # add the name of our file
  
  sentiment$year_review <- speech_date # add the year
  sentiment$president <-  president_name # add president
  
  sentiment_cloud <- tokens %>%
    inner_join(sentiment_word_fr) %>%
    dplyr::count(word, sort = T)
  
  # define a nice color palette
  pal <- brewer.pal(8,"Dark2")
  
  # plot the 50 most common words
  #wordcloud <- sentiment_cloud %>% 
    #with(wordcloud(word, n, random.order = FALSE, max.words = 10, colors=pal))
  #wordcloud(sentiment_cloud$word, sentiment_cloud$n, random.order=FALSE, max.words = 10, colors=pal)
  png(filename = paste("wordcloud",sentiment$year_review,".png"), width=6, height=4, units="in", res=300)
  wordcloud(sentiment_cloud$word, sentiment_cloud$n, random.order=FALSE, max.words = 10, colors=pal)
  dev.off()
  
  return(sentiment)
}

###-----------------------------------------------------

for (i in 1:length(df_speech_ids$url_speech)){
  url <- df_speech_ids$url_speech[[i]]
  url <- as.character(url)
  
  # on extrait les entiments de chaque discours et l'insert dans le dataframe
  sentiment_speech <- get_speech_sentiment(url)
  sentiments <- rbind(sentiments, sentiment_speech)
}

sentiments$president[[11]] <- "SARKOZY Nicolas"

sentiments$president <- as.factor(sentiments$president)
sentiments$president <- factor(sentiments$president, levels = c("MITTERRAND François","CHIRAC Jacques", "SARKOZY Nicolas", "HOLLANDE François", "MACRON Emmanuel"))

RPR_UMP <- sentiments %>%
  filter(president == "CHIRAC Jacques" | president == "SARKOZY Nicolas") %>%
  mutate(parti = "RPR_UMP")

PS <- sentiments %>%
  filter(president == "MITTERRAND François" | president =="HOLLANDE François") %>%
  mutate(parti = "PS")  

LREM <- sentiments %>%
  filter(president == "MACRON Emmanuel") %>%
  mutate(parti = "LREM") 

Par_parti <- full_join(RPR_UMP, PS)
Par_parti <- full_join(Par_parti, LREM)


# Ajout des infos US
files <- list.files("State_of_the_Union/")
sentiments_US <- data_frame()

# get the sentiments for each file in our datset
for(i in files){
  sentiments_US <- rbind(sentiments_US, Extract_Sentiments_US(i))
}

# Ajout de la colonne sentiment_US dans la table sentiments (France)
sentiments$sentiment_US <- sentiments_US$sentiment

# plot of sentiment over time & automatically choose a method to model the change
ggplot(sentiments, aes(x = as.numeric(year_review), y = sentiment_FR)) + 
  geom_point(aes(color = president))+ # add points to our plot, color-coded by president
  geom_smooth(method = "auto") + #pick a method & fit a model
  xlab("Speech pronounced on Dec 31st of the year") +
  ylab("Sentiment")


# plot of sentiment by president
ggplot(sentiments, aes(x = president, y = sentiment_FR, color = president)) + 
  geom_boxplot() # draw a boxplot for each president

ggplot(Par_parti, aes(x = parti, y = sentiment_FR, color = parti)) + 
  geom_boxplot() # draw a boxplot for each president

library(wordcloud)

png("MachineLearningCloud.png", width=12, height=8, units="in", res=300)
wordcloud(sentiment_cloud$word, sentiment_cloud$n, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()
