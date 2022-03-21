library(dplyr)
library(rvest)
library(stringr)
library(tm)
library(wordcloud2)
library(ggplot2)
library(tidytext)
library(reshape2)

setwd('~/R/Folder')

# Read the Cleaned Csv File ----
df <- read.csv("Cleaned_review_output.csv")

#Tokenizing the customer review column and setting the "ngram" to 2 ----
tokens <- df %>% 
  unnest_tokens(words,c(Cleaned_Review),token = 'ngrams', n =2)
print(head(tokens))

# Converting columns to "Factor" data type
tokens$words <-  as.factor(tokens$words)
tokens$Product <- as.factor(tokens$Product)

str(tokens) # Check the structure of the data Frame ----

# Check product sales order to know which sold most..----
as.data.frame(sort(table(tokens$Product),decreasing = T))

# Counting the no of times the tokens appear ----
word_counts <- 
  as.data.frame(sort(table(tokens$words),decreasing = T))
colnames(word_counts) <- c('Word','Frequency')
print(head(word_counts))

# Removing any word that has frequency less than 40 ----
word_filter <- word_counts %>% filter(Frequency > 39)

#Plotting the Word Cloud ----
wordcloud2(word_filter, size = 1.7)


keywords <- # converting the word_filter to a character data type  
  as.character(word_filter$Word)

# Applying the keywords to the data and removing any words that
# does not contains the keywords.
output <- 
  tokens %>% 
  filter(words %in% c(keywords)) %>% 
  count(Product,ReviewStar,words, sort = T)

# Analysis will be performed on only Top two product that the
#customer purchase.

# ggplot graph for First Product ----
output %>% 
  filter(Product == "boAt Rockerz 255" & n > 150) %>% 
  ggplot(aes(x = reorder(words, -n), y = n)) +
  geom_col(aes(fill = ReviewStar)) +
  labs(y = 'FREQUENCY', x = 'CUSTOMER REVIEWS',
       title ="boAt Rockerz 255") + 
  theme(axis.text.x = element_text(angle = 15, 
                                   colour = 'blue',
                                   size = 13))


# ggplot graph for Second Product ----
output %>%
  subset(words!='quality good') %>%
  filter(Product == 'Sennheiser CX 6.0BT' & n > 90) %>% 
  ggplot(aes(x = reorder(words,-n), y = n)) +
  geom_col(aes(fill = ReviewStar)) +
  labs(x = 'CUSTOMERS REVIEW', y = 'FREQUENCY', 
       title = 'Sennheiser CX 6.0BT') +
  theme(axis.text.x = element_text(angle = -25,
                                   color = 'blue',
                                   size = 14,
                                   hjust = 0.2))

# Casting the Keywords according to their reviews ----
output %>%
  dcast(words ~ ReviewStar, value.var = 'n',fill =0,
        fun.aggregate = sum)
