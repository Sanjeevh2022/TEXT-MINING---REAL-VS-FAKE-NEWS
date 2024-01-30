# Install and load the required packages.
pacman::p_load(dplyr, ggplot2, stringr, udpipe, lattice)

# load the fake.csv.
headlines_fake <- read.csv(file.choose(), stringsAsFactors = F)
head(headlines_fake)

headlines_fake <- headlines_fake[,1:4]

headlines_fake$year <- as.numeric(format(as.Date(headlines_fake$date, "%d-%b-%y"), "%Y"))
headlines_fake$month <- as.numeric(format(as.Date(headlines_fake$date, "%d-%b-%y"), "%m"))
headlines_fake$day <- as.numeric(format(as.Date(headlines_fake$date, "%d-%b-%y"), "%d"))

headlines_fake %>% group_by(year) %>%count()
# count the number of total headlines by date as a table


table_2016 <- headlines_fake %>% 
  filter(year == 2016) %>% 
  group_by(month)  %>%
  count()

table_2017 <- headlines_fake %>% 
  filter(year == 2017) %>% 
  group_by(month)  %>%
  count()



# udpipe to annotate the text in the headlines for 2016 

udmodel_english <- udpipe_load_model(file = "english-ewt-ud-2.5-191206.udpipe")

s <- udpipe_annotate(udmodel_english, headlines_fake$title)
x <- data.frame(s)

# extract and display frequencies for universal parts of speech (upos) in text
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "light blue",
         main = "FAKE - UPOS (Universal Parts of Speech)\n frequency of occurrence",
         xlab = "Freq")

# extract and display most occurring nouns in the headlines
## NOUNS
stats <- subset(x, upos %in% c("NOUN"))
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue",
         main = "fake - Most occurring nouns", xlab = "Freq")

# extract and display most occurring adjectives in the headlines
## ADJECTIVES
stats <- subset(x, upos %in% c("ADJ"))
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "purple",
         main = "fake:Most occurring adjectives", xlab = "Freq")

# extract and display most occurring verbs in the headlines
## VERBS
stats <- subset(x, upos %in% c("VERB"))
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "gold",
         main = "fake - Most occurring Verbs", xlab = "Freq")

# RAKE (Rapid Automatic Keyword Extraction algorithm)  
#key phrases in a body of text by analyzing the frequency of word appearance 
# and its co-occurrence with other words in the text.

stats <- keywords_rake(x = x, term = "lemma", group = "doc_id",
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "red",
         main = "fake - Keywords identified by RAKE",
         xlab = "Rake")

#NOUN PHRASES/VERB PHRASES
## display by plot a sequence of POS tags (noun phrases / verb phrases)
x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
stats <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token),
                          pattern = "(A|N)*N(P+D*(A|N)*N)*",
                          is_regex = TRUE, detailed = FALSE)
stats <- subset(stats, ngram > 1 & freq > 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ freq, data = head(stats, 20), col = "magenta",
         main = "fake - Keywords - simple noun phrases", xlab = "Frequency")

#n-gram >2
x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
stats <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token), pattern = "(A|
N)*N(P+D*(A|N)*N)*", is_regex = fake, detailed = FALSE)
stats <- subset(stats, ngram >  2& freq > 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ freq, data = head(stats, 25), col = "light blue", main = "fake - Keywords - simple
noun phrases, ngram>2", xlab = "Frequency")

#n-gram >3
x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
stats <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token), pattern = "(A|
N)*N(P+D*(A|N)*N)*", is_regex = fake, detailed = FALSE)
stats <- subset(stats, ngram >  3& freq > 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ freq, data = head(stats, 25), col = "light blue", main = "fake - Keywords - simple
noun phrases,ngrams>3", xlab = "Frequency")

## Collocation identification – basically words following one another)
stats <- keywords_collocation(x = x, term = "token", group = c("doc_id", "paragraph_id",
                                                               "sentence_id"), ngram_max = 4)
## How frequently do words occur in the same sentence (nouns and adjectives)
stats <- cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ")), term = "lemma", group
                      = c("doc_id", "paragraph_id", "sentence_id"))
## Co-occurrences: How frequent do words follow one another
stats <- cooccurrence(x = x$lemma, relevant = x$upos %in% c("NOUN", "ADJ"))

stats <- cooccurrence(x = x$lemma, relevant = x$upos %in% c("NOUN", "ADJ"), skipgram =
                        2)
head(stats)

#Wordnetwork

pacman::p_load(igraph, ggraph)

wordnetwork <- head(stats, 25)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") + geom_edge_link(aes(width = cooc, edge_alpha =
                                                          cooc), edge_colour = "red") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") +
  labs(title = "fake -Co-occurrences within 3 words distance", subtitle = "Nouns & Adjectives")
