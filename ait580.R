custcomplaint<-read.csv("C:/Users/HP/Desktop/Consumer_Complaints.csv")
custcomplaint
View(custcomplaint)
summary(custcomplaint)


library(tm)  #general text mining functions
library(DT)       # table format display of data
library(SnowballC) #for stemming
library('wordcloud')
library("tidytext")
library("RColorBrewer")
library("dplyr")
#library(plyr)
library("ggplot2")
library("textcat")
#detach("package:plyr", unload=TRUE) 

most5mortgageissue = custcomplaint %>%
  filter(Product == "Mortgage") %>%
  group_by(Issue) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(Issue = reorder(Issue,Count)) %>%
  head(10)
View(most5mortgageissue)
top10mortgageissues = inner_join(most5mortgageissue,custcomplaint)

  
top10studentloanissue = custcomplaint %>%
  filter(Product == "Student loan") %>%
  group_by(Issue) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(Issue = reorder(Issue,Count)) %>%
  head(10)
View(top10studentloanissue)


top10creditcardissue=custcomplaint %>%
  filter(Product == "Credit card") %>%
  group_by(Issue) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(Issue = reorder(Issue,Count)) %>%
  head(10)
View(top10creditcardissue)

####most common words in customer complaint according to company name###
companycomplaint = custcomplaint %>% 
  filter(Company == "WELLS FARGO & COMPANY") %>%
  select(Product,Issue,State,Consumer.complaint.narrative,Company)
View(companycomplaint)

###To get rid of null values from consumercomplaintnarrative
cc<-companycomplaint[!(is.na(companycomplaint$Consumer.complaint.narrative) | companycomplaint$Consumer.complaint.narrative==""), ]
View(cc)
datatable(head(cc), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))
write.csv(cc, file="wellsfargocustcomplaint.csv")
Wellsfargo<- read.csv("wellsfargocustcomplaint.csv", stringsAsFactors = FALSE)
#top 10 most common words of Wellsfargo customer complaints
Wellsfargo %>%
  unnest_tokens(word, Consumer.complaint.narrative) %>%
  filter(!word %in% stop_words$word) %>%
  count(word,sort = TRUE) %>%
  ungroup() %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  head(10)
View(Wellsfargo)
#######Top 10 most common words for company name Bankofamerica 
companycomplaintbofa = custcomplaint %>% 
  filter(Company == "BANK OF AMERICA, NATIONAL ASSOCIATION") %>%
  select(Product,Issue,State,Consumer.complaint.narrative,Company)
View(companycomplaintbofa)

###To get rid of null values from consumercomplaintnarrative
ccbofa<-companycomplaintbofa[!(is.na(companycomplaintbofa$Consumer.complaint.narrative) | companycomplaintbofa$Consumer.complaint.narrative==""), ]
View(ccbofa)
datatable(head(ccbofa), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))
write.csv(ccbofa, file="bofacustcomplaint.csv")
bofa<- read.csv("bofacustcomplaint.csv", stringsAsFactors = FALSE)
#top 10 most common words of bank of america
bofa %>%
  unnest_tokens(word, Consumer.complaint.narrative) %>%
  filter(!word %in% stop_words$word) %>%
  count(word,sort = TRUE) %>%
  ungroup() %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  head(10)
View(bofa)

#Sentiment scores for bofa

calculate_sentiment <- function(bofa)
{
  sentiment_lines  =  bofa %>%
    filter(textcat(Consumer.complaint.narrative) == "english") %>%  # considering only English text
    unnest_tokens(word,Consumer.complaint.narrative ) %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(X) %>%
    summarize(sentiment = mean(score),words = n()) %>%
    ungroup() %>%
    filter(words >= 5)
  
  return(sentiment_lines)
  
}


sentiment_lines = calculate_sentiment(bofa)

head(sentiment_lines)


sentiment_lines = calculate_sentiment(Wellsfargo)

head(sentiment_lines)

equifax = custcomplaint %>% 
  filter(Company == "EQUIFAX, INC.") %>%
  select(Product,Issue,State,Consumer.complaint.narrative,Company)
View(equifax)

###To get rid of null values from consumercomplaintnarrative
equifax1<-equifax[!(is.na(equifax$Consumer.complaint.narrative) | equifax$Consumer.complaint.narrative==""), ]
View(equifax1)
datatable(head(equifax1), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))
write.csv(equifax1, file="equifaxcustcomplaint.csv")
Wellsfargo<- read.csv("wellsfargocustcomplaint.csv", stringsAsFactors = FALSE)


