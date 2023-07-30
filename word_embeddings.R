
# Document that provided the concepts and code for this project
# https://cbail.github.io/textasdata/word2vec/rmarkdown/word2vec.html 

# Remove any previously created objects
rm(list = ls())

# Load libraries
library(tidytext)
library(dplyr)
library(widyr)
library(irlba) # Loading required package: Matrix
library(broom)
library(ggplot2)

# Set word directory
setwd("C:/New York/APAN/2nd Term/Frameworks and Methods II/Assignments/APAN 5205_Data_Rscript_Group 6/")

# Load the data
HP_data <- read.csv("cleaned_nlp.csv")

# Filter the dataset to get just dialogue_id and dialogue
dialogues <- HP_data[, c('dialogue_id', 'dialogue_cleaned')]

## Calculate the Skipgram probabilities: how often we find each word next to 
# every other word within the context window. 

# Create context window with length 8
tidy_skipgrams <- dialogues %>%
  unnest_tokens(ngram, dialogue_cleaned, token = "ngrams", n = 8) %>%
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
normalized_prob <- skipgram_probs %>%
  filter(n > 20) %>%
  rename(word1 = item1, word2 = item2) %>%
  left_join(unigram_probs %>%
              select(word1 = word, p1 = p),
            by = "word1") %>%
  left_join(unigram_probs %>%
              select(word2 = word, p2 = p),
            by = "word2") %>%
  mutate(p_together = p / p1 / p2)

# Remove the rows where both word1 and word2 are NA 
normalized_prob <- normalized_prob[!(is.na(normalized_prob$word1) & 
                                       is.na(normalized_prob$word2)), ]

# Note: The variable p_together here describes the probability the word2 occurs 
# within the context window of word1.

# Example: these are the words that are most likely to occur within a context 
# window of eight words around 'harry'
normalized_prob %>% 
  filter(word1 == "harry") %>%
  arrange(-p_together)

## Synonyms

# A Singular Value Decomposition from the irlba package will be use to reduce
# the dimensionality of that matrix
pmi_matrix <- normalized_prob %>%
  mutate(pmi = log10(p_together)) %>%
  cast_sparse(word1, word2, pmi)

# Remove missing data
pmi_matrix@x[is.na(pmi_matrix@x)] <- 0

# Run SVD
pmi_svd <- irlba(pmi_matrix, 256, maxit = 500)

# Output the word vectors:
word_vectors <- pmi_svd$u
rownames(word_vectors) <- rownames(pmi_matrix)

# Function to identify synonyms using the word vectors we created above
search_synonyms <- function(word_vectors, selected_vector) {
  
  similarities <- word_vectors %*% selected_vector %>%
    tidy() %>%
    as_tibble() %>%
    rename(token = .rownames,
           similarity = unrowname.x.)
  
  similarities %>%
    arrange(-similarity)    
}

# Let’s see what the top synonyms are for the term 'dark'
#(pres_synonym <- search_synonyms(word_vectors,word_vectors["dark",]))

## Plot all of the words in the model in multidimensional space

# Rerun the SVD in two dimensions for easier plotting and interpretation
pmi_svd <- irlba(pmi_matrix, 2, maxit = 500)

# Output the word vectors:
word_vectors <- pmi_svd$u
rownames(word_vectors) <- rownames(pmi_matrix)

# Grab 100 words
forplot <- as.data.frame(word_vectors[100:500,])
forplot$word<-rownames(forplot)

#now plot
ggplot(forplot, aes(x=V1, y=V2, label=word))+
  geom_text(aes(label=word),hjust=0, vjust=0, color="blue")+
  theme_minimal()+
  xlab("First Dimension Created by SVD")+
  ylab("Second Dimension Created by SVD")
