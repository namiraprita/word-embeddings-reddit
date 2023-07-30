################################################################################
# Cleaning of the data
################################################################################

# Load libraries 
library(pacman)
pacman::p_load(dplyr)

library(dplyr)
library(tm) # for text mining
library(SnowballC) # for stemming
library(stringr) # for string manipulation
#install.packages("tidytext")
library(tidytext)
library(tidyr)

# Set working directory
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path ))

#######################
# Harry Potter database
#######################

characters = read.csv('characters.csv')
dialogue = read.csv('dialogue.csv')
movies = read.csv('movies.csv')
places = read.csv('places.csv')
chapters = read.csv('chapters.csv')
spells = read.csv('spells.csv')

dialogue$dialogue_no_spells=gsub(paste(spells$incantation,collapse='|'),"",dialogue$dialogue)

merged = merge(x = dialogue, y = characters, by = "character_id")
merged = merge(x = merged, y = chapters, by = "chapter_id")
merged = merge(x = merged, y = movies, by = "movie_id")
merged = merge(x = merged, y = places, by = "place_id")

df = merged[,c("dialogue_id","movie_title","movie_chapter","chapter_id","place_name","place_category","character_name","gender","house","species","dialogue","dialogue_no_spells")]

#write.csv(df, "cleaned.csv", row.names=FALSE)

clean_text <- function(text) {
  # Convert to lowercase
  text <- tolower(text)
  
  # Remove punctuation
  text <- str_replace_all(text, "[[:punct:]]", "")
  
  # Remove numbers
  text <- str_replace_all(text, "\\d+", "")
  
  # Remove stop words
  text <- removeWords(text, stopwords("english"))
  
  # Stem words
  text <- wordStem(text)
  
  # Remove whitespace
  text <- str_trim(text)
  
  # Return cleaned text
  return(text)
}

# Apply cleaning function to text column
df$dialogue<- iconv(df$dialogue, from = 'ISO-8859-1', to = 'utf8')
df$dialogue_cleaned <- sapply(df$dialogue, clean_text)
class(df$dialogue_cleaned)
# #Tokenization
# df <- df %>%
#   mutate(dialogue_tokens = list(unnest_tokens(word, dialogue_cleaned))) #%>%
#   unnest(dialogue_tokens)

write.csv(df, "cleaned_nlp.csv", row.names=FALSE)

#################
# Reddit database
#################

Reddit_data <- read.csv("typed_posts.csv")

# Remove rows without MBTI values (column: 'type') and filter dataset to get
# title (posts) and type (MBTI)
posts <- Reddit_data %>%
  filter(type != "") %>%
  select(title, type)

# Add post_id and rename columns
posts <-  posts %>%
  mutate(post_id = 1:nrow(posts)) %>%
  rename(post = title,  MBTI = type)

# Apply cleaning function to text column
posts$post_cleaned <- sapply(posts$post, clean_text)
class(posts$post_cleaned)

write.csv(posts, "cleaned_reddit.csv", row.names=FALSE)


