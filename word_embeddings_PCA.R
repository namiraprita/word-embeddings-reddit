
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
library(pracma)
library(plotly)
library(stringr)

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
  #filter(n > 10) %>%
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
  #filter(word1 == "harry") %>%
  filter(stringr::str_detect(word1, "think")) %>%
  arrange(-p_together)

## Synonyms

# A Principal Component Analysis from the pracma package will be use to reduce
# the dimensionality of that matrix
pmi_matrix <- normalized_prob %>%
  mutate(pmi = log10(p_together)) %>%
  cast_sparse(word1, word2, pmi)

# Remove missing data
pmi_matrix@x[is.na(pmi_matrix@x)] <- 0

# Run PCA
pmi_pca <- prcomp(t(pmi_matrix), center = TRUE, scale. = TRUE, rank = 256)

# Output the word vectors:
word_vectors <- pmi_pca$x
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

# Rerun the PCA in two dimensions for easier plotting and interpretation
pmi_pca <- prcomp(t(pmi_matrix), center = TRUE, scale. = TRUE, rank = 3)

# Output the word vectors:
word_vectors <- pmi_pca$x
rownames(word_vectors) <- rownames(pmi_matrix)

# Grab some words
forplot <- as.data.frame(word_vectors[100:400,])
#forplot <- as.data.frame(word_vectors)
forplot$word <- rownames(forplot)

# 2D plot

# # Convert the data to a plotly object
# forplot_plotly <- plot_ly(forplot, x = ~PC1, y = ~PC2, text = ~word, type = "scatter", mode = "text")
# 
# # Add plot labels and style
# forplot_plotly <- forplot_plotly %>%
#   layout(xaxis = list(title = "First Dimension Created by PCA"),
#          yaxis = list(title = "Second Dimension Created by PCA"),
#          font = list(size = 14),
#          margin = list(l = 50, r = 50, b = 50, t = 50, pad = 2))
# 
# # Display the plot
# forplot_plotly

# 3D plot

# Convert the data to a plotly object
forplot_plotly <- plot_ly(forplot, x = ~PC1, y = ~PC2, z = ~PC3, text = ~word, type = "scatter3d", mode = "text")

# Add plot labels and style
forplot_plotly <- forplot_plotly %>%
  layout(scene = list(xaxis = list(title = "First Dimension Created by PCA"),
                      yaxis = list(title = "Second Dimension Created by PCA"),
                      zaxis = list(title = "Third Dimension Created by PCA")),
         font = list(size = 14),
         margin = list(l = 50, r = 50, b = 50, t = 50, pad = 2))

# Display the plot
forplot_plotly
