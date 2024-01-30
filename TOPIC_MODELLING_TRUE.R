#Step 1 – load the required libraries and the data
library(tidyverse) # organize workflow and for all text work
library(tidytext) # contains the NLP methods we need
library(topicmodels) # for LDA topic modelling – our main package
library(tm) # general text mining functions, DTM work.
library(SnowballC) # for stemming when needed.
library(stringr) # for cleaning

reviews <- read_csv(file.choose()) #load the file

#Step 2 – Clean the data. This is an important step, check column names for your dataset
#clean the review data
reviews$text <- str_replace_all(reviews$text,"[^[:graph:]]", " ")

# function to get & plot the most informative terms by a specified number
# of topics, using LDA
top_terms_by_topic_LDA <- function(input_text, # should be a column from a data frame
                                   plot = T, # return a plot? TRUE by defult
                                   number_of_topics = 4) # number of topics (4 by default)
{
  # create a corpus (type of object expected by tm) and document term matrix
  Corpus <- Corpus(VectorSource(input_text)) # make a corpus object
  DTM <- DocumentTermMatrix(Corpus) # get the count of words/document
  # remove any empty rows in our document term matrix (if there are any
  # we'll get an error when we try to run our LDA)
  unique_indexes <- unique(DTM$i) # get the index of each unique value
  DTM <- DTM[unique_indexes,] # get a subset of only those indexes
  # preform LDA & get the words/topic in a tidy text format
  lda <- LDA(DTM, k = number_of_topics, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta")
  # get the top ten terms for each topic,
  # yes I made up the word informativeness
  top_terms <- topics %>% # take the topics data frame and..
    group_by(topic) %>% # treat each topic as a different group
    top_n(10, beta) %>% # get the top 10 most informative words
    ungroup() %>% # ungroup
    arrange(topic, -beta) # arrange words in descending informativeness
  # if the user asks for a plot (TRUE by default)
  if(plot == T){
    # plot the top ten terms for each topic in order
    top_terms %>% # take the top terms
      mutate(term = reorder(term, beta)) %>% # sort terms by beta value
      ggplot(aes(term, beta, fill = factor(topic))) + # plot beta by theme
      geom_col(show.legend = FALSE) + # as a bar plot
      facet_wrap(~ topic, scales = "free") + # which each topic in a separate plot
      labs(x = NULL, y = "Beta") + # no x label, change y label
      coord_flip() # turn bars sideways
  }else{
    # if the user does not request a plot
    # return a list of sorted terms instead
    return(top_terms)
  }
}

#Test out the function to ensure everything works by starting with two topics.

top_terms_by_topic_LDA(reviews$text, number_of_topics = 2)


#corpus
reviewsCorpus <- Corpus(VectorSource(reviews$text))
reviewsDTM <- DocumentTermMatrix(reviewsCorpus)
# convert the document term matrix to a tidytext corpus
reviewsDTM_tidy <- tidy(reviewsDTM)

custom_stop_words <- tibble(word = c("Trump","trump","President","presiedent", "republican")

reviewsDTM_tidy_cleaned <- reviewsDTM_tidy %>% 
  anti_join(stop_words, by = c("term" = "word")) %>% 
  anti_join(custom_stop_words, by = c("term" = "word"))

cleaned_documents <- reviewsDTM_tidy_cleaned %>%
  group_by(document) %>%
  mutate(terms = toString(rep(term, count))) %>%
  select(document, terms) %>%
  unique()

head(cleaned_documents)