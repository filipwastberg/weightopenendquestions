## Combine weights with open end questions

R is a powerful tool to analyse text. However, when dealing with surveys where there is a risk for underrepresentation, and you know the marginal distributions of the population, you need to weight the data. Sometimes there may even be a weight variable included in a data set. 

This will not cover the weighting procedure, which is well documented in the `survey` package. 

Nevertheless, my amibition is to provide a way to use your weights in combination with text data. 

```{r message = FALSE, warning = FALSE}
library(tm)
library(dplyr)
library(wordcloud)
```

## Data

For this case I will use some random words. But for the sake of it we can imagine that the question that has been asked in this survey is: What is the most important thing in your life?

The data consists of three variables: `words`, `gender` and `weight`.

```{r}
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
```

## Cleaning text
In order to do some standard cleaning we create the following function

```{r}
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  return(corpus) 
}
```

## Prepare the text to be weighted
In order to weight data we want the text data to be tidy. In other words: each row should be an observation, i.e. a person, and each column a variable, i.e. the word mentioned. To do that we use the `DoucmentTermMatrix()` from the `tm` package.

The following code subdivides every sentence into words so that every column is a word. The values(term frequency), 0 or 1, indicates if the word has been used.

```{r}
text_df <- VectorSource(paste(data$words)) %>%
  VCorpus() %>%
  clean_corpus() %>%
  DocumentTermMatrix() %>%
  as.matrix() %>%
  as.data.frame()
```

## Weight text
Since the term frequency is represented by either 0 or 1 we can add the weights to the data frame and then multiply the weights with the term frequency and then sum it back to a matrix with which we can do a wordcloud.

```{r warning = FALSE, message = FALSE}
text_df$weight <- data$weight

# Multiply the weight with term frequency
text_df_mod <- text_df %>%
  mutate_each(funs(.*weight), -weight) %>%
  select(-one_of("weight"))

# Sum it back to a matrix in order to do a wordcloud
text_mod_sum <- as.matrix(text_df_mod) %>%
  colSums() %>%
  as.matrix()

# Do a wordcloud
commonality.cloud(text_mod_sum, max.words = 50,
                  color = brewer.pal(6, "Dark2"),
                  random.order = FALSE, 
                  rot.per = 0, scale=c(4, 0.4))

```
