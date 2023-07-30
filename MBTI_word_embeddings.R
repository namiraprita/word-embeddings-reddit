# Document that provided the concepts and code for this project
# https://cbail.github.io/textasdata/word2vec/rmarkdown/word2vec.html


###############################################################################
#MBTI based on the probability of a word to occur given the words surrounding it 
#                           (word embeddings)
###############################################################################

# Load libraries
library(tidytext)
library(dplyr)
library(widyr)
library(irlba) # Loading required package: Matrix
library(broom)
library(ggplot2)
library(pracma)
library(stringr)

# Remove any previously created objects
rm(list = ls())

# Set working directory
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path ))

# Load the HP data
HP_data <- read.csv("cleaned_nlp.csv")

# Filter the dataset to get just dialogue_id, character_name, and dialogue
dialogues <- HP_data[, c('dialogue_id', 'character_name','dialogue_cleaned')]

# Summary of the number of words per dialogue

summary(str_count(string = dialogues$dialogue_cleaned, pattern = '\\S+'))

# The third quartile is 7 words per dialogue, so that's the lenght we use to
# calculate the Skipgram probabilities

#######################################
## Word embeddings for Harry characters
#######################################

character <- 'Ron Weasley'

## Calculate the Skipgram probabilities: how often we find each word next to 
# every other word within the context window. 

# Create context window with length 7
tidy_skipgrams <- dialogues %>%
  filter(character_name == character) %>%
  unnest_tokens(ngram, dialogue_cleaned, token = "ngrams", n = 7) %>%
  mutate(ngramID = row_number()) %>% 
  tidyr::unite(skipgramID, dialogue_id, ngramID) %>%
  unnest_tokens(word, ngram)

# Calculate unigram probabilities (used to normalize skipgram probabilities later)
unigram_probs <- dialogues %>%
  unnest_tokens(word, dialogue_cleaned) %>%
  count(word, sort = TRUE) %>%
  mutate(p = n / sum(n))

# Calculate probabilities
skipgram_probs <- tidy_skipgrams %>%
  pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
  mutate(p = n / sum(n))

# Normalize probabilities according to the unigram probabilities (or the overall 
# frequency of each word in the corpus)

normalized_prob_HP <- skipgram_probs %>%
  filter(n > 3) %>% # Filter for the words that appear at least 4 times
  rename(word1 = item1, word2 = item2) %>%
  left_join(unigram_probs %>%
              select(word1 = word, p1 = p),
            by = "word1") %>%
  left_join(unigram_probs %>%
              select(word2 = word, p2 = p),
            by = "word2") %>%
  mutate(p_together = p / p1 / p2)

# Remove the rows where both word1 and word2 are NA 
normalized_prob_HP <- normalized_prob_HP[!(is.na(normalized_prob_HP$word1) & 
                                       is.na(normalized_prob_HP$word2)), ]

# Example with a specific word
normalized_prob_HP %>% 
  filter(word1 == "ever") %>%
  arrange(-p_together)

# Note: The variable p_together here describes the probability the word2 occurs 
# within the context window of word1.

################################################################################

#######################################
## Word embeddings for the Reddit users
#######################################

# Load the Reddit data
posts <- read.csv("cleaned_reddit.csv")

## Word embeddings for each type of MBTI

personality <- unique(posts$MBTI)
#personality <- "intj"

skipgrams_mbti <- list()

for (i in personality) {
  
## Calculate the Skipgram probabilities: how often we find each word next to 
# every other word within the context window. 

# Create context window with length 7
tidy_skipgrams <- posts %>%
  filter(MBTI == i) %>%
  unnest_tokens(ngram, post, token = "ngrams", n = 7) %>%
  mutate(ngramID = row_number()) %>% 
  tidyr::unite(skipgramID, post_id, ngramID) %>%
  unnest_tokens(word, ngram)

# Calculate unigram probabilities (used to normalize skipgram probabilities later)
unigram_probs <- posts %>%
  unnest_tokens(word, post) %>%
  count(word, sort = TRUE) %>%
  mutate(p = n / sum(n))

# Calculate probabilities
skipgram_probs <- tidy_skipgrams %>%
  pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
  mutate(p = n / sum(n))

# Normalize probabilities according to the unigram probabilities (or the overall 
# frequency of each word in the corpus)
normalized_prob_reddit <- skipgram_probs %>%
  filter(n > 10) %>% 
  rename(word1 = item1, word2 = item2) %>%
  left_join(unigram_probs %>%
              select(word1 = word, p1 = p),
            by = "word1") %>%
  left_join(unigram_probs %>%
              select(word2 = word, p2 = p),
            by = "word2") %>%
  mutate(p_together = p / p1 / p2)

# Remove the rows where both word1 and word2 are NA 
normalized_prob_reddit <- normalized_prob_reddit[!(is.na(normalized_prob_reddit$word1) & 
                                       is.na(normalized_prob_reddit$word2)), ]

skipgrams_mbti[[i]] <- normalized_prob_reddit

}

# Example with a specific word
normalized_prob_reddit %>% 
  filter(word1 == "ever") %>%
  arrange(-p_together)

################################################################################

# Loop to find the words that are most likely to occur within a context window 
# of seven words for each word in the corpus of the Harry characters. 
# Then, compare it with the probabilities for the same word with the 16 MBTI 
# personalities that appear in the Reddit database.

avg_distance <- data.frame()
distance_by_mbti <- data.frame()

for (i in 1:length(names(skipgrams_mbti))) {
  for (word in normalized_prob_HP$word1) {
  #for (word in c('kind', 'think')) {
  df1 <- normalized_prob_HP %>% 
    filter(word1 == word) %>%
    select(word2, p_together)
  
  df2 <- skipgrams_mbti[[i]] %>% 
    filter(word1 == word) %>%
    select(word2, p_together)
  
  merge <- merge(df1, df2, by="word2")
  
  
  if (isempty(merge$word2)) {
    
    distance <- NA
  
  } else {
    
    distance <- abs(merge$p_together.x - merge$p_together.y)
  }
  
  
  extra_row <- cbind(word, distance)
  avg_distance <- rbind(avg_distance, extra_row)
  
}

avg_mbti <- mean(as.numeric(avg_distance$distance), na.rm = T)
distance_by_mbti <- rbind(distance_by_mbti, cbind(character, mbti = names(skipgrams_mbti)[i], avg_mbti))

}

distance_by_mbti <- distance_by_mbti %>%
  arrange(as.numeric(avg_mbti))

write.csv(distance_by_mbti, paste0(character,"_MBTI.csv"), row.names=FALSE)
