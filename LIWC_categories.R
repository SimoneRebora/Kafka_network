# Kafka in a network of diction

# research question: 
# Kafka's diction was very unique. He did not obviously write like anyone else, he did not use obvious intertextuality markers. 
# yet, he was not an island. The question is: What similarities and network-connections do we observe of Kafka with other selected writers, with teh measures 
# MFW and LIWC, using Network analysis?

## Read LIWC data
require(tidyverse)
data_path = "LIWC_analyses/LIWC_analysis_141.csv"
all = read_csv(data_path) 
all = all %>%
  mutate(
    author = sapply(str_split(all$Filename,'_|\\.'), function(x) x[1]),
    book = sapply(str_split(all$Filename,'_|\\.'), function(x) x[2]),
    year = sapply(str_split(all$Filename,'_|\\.'), function(x) x[3])) %>%
  readr::type_convert()

# add readability data
library(quanteda)
library(readtext)

alltexts <- readtext('corpus/*.txt', docvarsfrom = "filenames", dvsep = "_", docvarnames = c("Author", "Title", "Year", "Original"))
all_corpus <- corpus(alltexts)
all_corpus_summary <- summary(corpus(alltexts), 141) # renders tokens, types, sentences (default is 100 texts)
tok_all <- quanteda::tokens(alltexts$text,remove_punct = TRUE) # tokenizes all texts; removes punctuation; creates a list object

# MSTTR for all texts
MSTTR_all <- textstat_lexdiv(tok_all,
                             measure = "MSTTR",
                             remove_numbers = TRUE,
                             remove_punct = TRUE,
                             remove_symbols = TRUE,
                             remove_hyphens = FALSE,
                             log.base = 10,
                             MSTTR_segment = 100L
)

all_corpus_summary$MSTTR <- MSTTR_all$MSTTR # add MSTTR by author to "all_corpus_summary" add just column #2, the values

# Readability measure
tok_all_full <- tokens(alltexts$text,remove_punct = FALSE) #  including punctuation
readability <- textstat_readability (all_corpus, measure = "SMOG.de", # used formula working for German and tested correlation w/ other formulae (very high)
                                         remove_hyphens = FALSE,
                                         min_sentence_length = 1,
                                         max_sentence_length = 10000,
                                         intermediate = FALSE
)

all_corpus_summary$SMOG.de <- readability$SMOG.de # add readability
#View(all_corpus_summary)

# back to "all"!
all$SMOG.de <- all_corpus_summary$SMOG.de 
all$MSTTR <- all_corpus_summary$MSTTR
#View(all)


## Define dimensions 

# Analysis: we run experiental and epistemic in one network
# summing up the values for each of the categories

# to do: add loop to add all objects as columns to "all" and compute sum of values

# EXPERIENTIAL --------
exp_relate = c('family','friend','female','male') # relate to others. Removed 'social',
exp_body = c('body','health','sexual','ingest') # having a body. Removed 'bio'
exp_imperson = c('number','ipron') # impersonal. From LIWC cats "grammar" and "pronouns"
exp_affect = c('posemo','anx','anger','sad') # former "exp_person". Renamed as "exp_affect", removed "affect", "negemo" (left in all basic level cats)
exp_space = c('see','hear','feel','motion','space') # being in and experiencing space. Removed 'percept'
exp_drives = c('affiliation','achiev','power','reward','risk') # motives and occupation (reassigned to match LIWC umbrella cats). Removed 'drives'
exp_concerns = c('work','leisure','home','money') # added cat: reassigned to match LIWC umbrella cats. Removed 'relig','death' (included in "exp_transc")
exp_transc = c('relig','death') # transcendental

experiential = c(exp_relate, exp_body, exp_affect, exp_space, exp_drives, exp_concerns, exp_transc) # without exp_imperson, !added exp_transc

# EPISTEMIC --------
epistemic = c('tentat','discrep','differ','certain','cause','insight','adverb','negate','quant') # used all LIWC cats included in cogproc, plus adverb, negate (from "ling dim"), quant ("other grammar". Removed 'cogproc'.

# LITERARINESS --------
#lit_temp = c('focuspast', 'focusfuture', 'focuspresent') # Time orientation
literariness = c('WPS','Sixltr','assent','AllPunc','MSTTR','readability') # withoutw lit_temp (for now)


# the loop

all$exp_relate = 0
all$exp_body = 0
all$exp_imperson = 0
all$exp_affect = 0
all$exp_space = 0
all$exp_drives = 0
all$exp_concerns = 0
all$exp_transc = 0
all$experiential = 0
all$epistemic = 0
all$literariness = 0

for(category in colnames(all)[105:115]){
  for(i in 1:length(all$Filename)){
    all[i,category] <- sum(all[i,which(colnames(all) %in% get(category))])
  }
}

final_df <- all[,c(1:99,103:115)]
write.csv(final_df, file = "LIWC_analyses/LIWC_analysis_141_more_categories.csv", row.names = F)

final_df <- all[,c(1:3, which(colnames(all) %in% c(experiential, epistemic)), 105:114)]
write.csv(final_df, file = "LIWC_analyses/LIWC_analysis_141_experiential_epistemic.csv", row.names = F)
