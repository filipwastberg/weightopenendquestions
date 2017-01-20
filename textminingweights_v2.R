# Title: One way to combine text data and weights based on meta data
# Date: 2016-01-20
# By: Filip Wästberg

# Consider this example: we have performed a sample from a population and now have 
# 1000 observations of text. In the data we also have a weight depending on gender

library(tm)
library(wordcloud)
library(dplyr)

set.seed(123)

data <- rbind(data.frame(gender = "M",
                         words =  sample(c("education", "money", "family",
                                           "house", "debts", "I love my daughter",
                                           "This text is awesome",
                                           "I would like to know you",
                                           "darkness is upon us",
                                           "help me I'm starving",
                                           "what was the questions?",
                                           "i think that we should drink more",
                                           "help me"),
                                         600, replace = TRUE)),
              data.frame(gender = "F",
                         words =  sample(c("career", "bank", "friends", 
                                           "drinks", "relax",
                                           "the most important things in life is not career but to drink",
                                           "i love my friends",
                                           "i work in a bank so that is important", "i don't know",
                                           "well, I love this my friends",
                                           "i think it is important to have a career"),
                                         400, replace = TRUE)
              )
) %>% mutate(weight = ifelse(gender == "M", 0.7, 
                             ifelse(gender == "F", 1.3, NA)))

# In order to do some standard cleaning we create the following function
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  return(corpus)


# We clean the text and turn it into a matrix which we use for the wordcloud
text <- paste(data$words, collapse = " ")
matrix <- as.matrix(
  TermDocumentMatrix(
    clean_corpus(
      VCorpus(
        VectorSource(text)
      )
    )
  )
)

commonality.cloud(matrix, max.words = 50, color = brewer.pal(6, "Dark2"),
                  random.order = FALSE, rot.per=0, scale=c(5,0.5))

### Weight data
# However, I want the term frequency to take the weights into consideration
# The following code does that

text_df <- as.data.frame(
  as.matrix(
    # In order to work with my weights I want each observation to be one row,
    # hence I use DTM instead of TDM.
    DocumentTermMatrix(
      clean_corpus(
        VCorpus(
          # One important thing is to omit the collapse in the paste function
          VectorSource(paste(data$words)
          )
        )
      )
    )
  )
)
# Because each row is an observation I simply add the weights
text_df$weight <- data$weight

# Since term frequency is binary (either 1 or 0) I mulitply the weights with the term frequency
text_df_mod <- text_df %>%
  mutate_each(funs(.*weight), -weight) %>%
  # Here I remove the weight column
  select(-one_of("weight"))

# In order to sum up the frequencies I turn it back to a matrix and sum the columns
text_mod_sum <- as.matrix(
  colSums(
    as.matrix(text_df_mod)
  )
)

### The wordcloud is now different, since the weights are taken into consideration when counting the term frequency
commonality.cloud(text_mod_sum, max.words = 50,
                  color = brewer.pal(6, "Dark2"),
                  random.order = FALSE, rot.per = 0, scale=c(5, 0.5))
