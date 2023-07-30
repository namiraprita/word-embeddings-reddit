# Harry Potter Character MBTI Personality Predictor

This project aims to predict the MBTI personality types of lead characters from the Harry Potter movie series, utilizing word embedding techniques with the Skip-Gram approach.

📑 Table of Contents
Problem Statement
Research Questions
Data Description
Analytical Techniques
Findings
Conclusions and Recommendations
Getting Started
Contributing
License

🎯 Problem Statement
Most studies on Harry Potter characters have focused on the books, with little exploration based on the movie scripts, especially in the area of personality analysis. This project seeks to fill this gap by predicting the MBTI personality types of the main characters based on their dialogues in the films.

❓ Research Questions
What are the MBTI personality types of each lead character in the Harry Potter movie sequel?
How well does the model work? Does it predict certain MBTI components better than others?

📊 Data Description
Two datasets are utilized for this project:

Publicly available data on Harry Potter movies dialogues and characters, provided by Maven Analytics. The dataset has been preprocessed for cleaning and stemming, resulting in 7,444 lines of dialogue across 27 variables.
A Reddit dataset provided by Gjurković and Šnajder (2018), which includes posts and the MBTI personality types of their authors. The dataset was preprocessed in a similar manner and includes 366,737 posts across 34 variables.

🧮 Analytical Techniques
The project employs text mining techniques on the two datasets. The main method utilized is a Skip-Gram word embeddings model, which predicts the likelihood of a word appearing next to another within a context window.

📝 Findings
The project successfully associated each character with an MBTI type. The accuracy of the model varied for different MBTI components, with the highest for predicting the Extrovert-Introvert component and the lowest for the Judging-Perceiving component.

💡 Conclusions and Recommendations
To improve the model, future work could address data imbalance, reduce computational time, improve accuracy by analyzing linguistic patterns, adapt the context window size, and include multimodal predictions using audio features.

📄 License
Distributed under the MIT License. See LICENSE for more information.




