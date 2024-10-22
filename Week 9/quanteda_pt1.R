# Part 1 Import ---------------------------------------------------------------

library(tidyverse)  # for data manipulation
library(quanteda)  # for cleaning and analysis of text
library(quanteda.textstats) # extra quanteda commands
library(quanteda.textplots) # for plotting the keyness statistic


#custom function for stemming and then unstemming a dfm
stem_unstem<-function(dfm, matchby='frequency'){
  if(matchby == 'shortest'){
    vocab<-colnames(dfm)[order(nchar(colnames(dfm)), decreasing=T)]
  }
  if(matchby == 'frequency'){
    vocab<-names(sort(featfreq(dfm), decreasing=T))
    
  }
  stemmed<-char_wordstem(vocab, check_whitespace=FALSE)
  dfm<-dfm_replace(dfm, vocab, stemmed)
  
  dfm<- dfm_replace(dfm, featnames(dfm), vocab[match(featnames(dfm), stemmed)])
  return(dfm)
}



poldf<-read_csv('https://github.com/Neilblund/APAN/raw/main/news_sample.csv')

# normalizing some apostrophes and quotes (surprisingly important!)
poldf$text<-str_replace_all(poldf$text,"\u201C|\u201D", '"')|>
  str_replace_all("\u2019|\u2018", "'")


# convert to a corpus
pol_corpus<-with(poldf ,corpus(text, docvars = data.frame(url, headline,  date, source)))

# tokenize the texts
pol_tokens<-tokens(pol_corpus, 
                   what = 'word',
                   # this keeps characteristics about the documents like the headline and url
                   remove_punct =TRUE,
                   remove_symbols = TRUE,
                   remove_numbers = TRUE,
                   remove_url = TRUE,
                   remove_separators = TRUE,
                   include_docvars = TRUE
)


## Using KWIC ------

# Use keywords in context to get a pattern of text in context:
kw_immig <- kwic(pol_tokens, pattern =  "immig*", window=3)
# view the first 10
head(kw_immig, 5)


# Using a dictionary --------

## Policy agendas -----

# download the policy agendas codebook (discussed here #https://www.almendron.com/tribuna/wp-content/uploads/2017/05/CAP2013v2.pdf)
policyAgendas<-readRDS(url('https://github.com/Neilblund/APAN/raw/main/policy_agendas_dictionary.rds'))

immig_tokens<-tokens_subset(pol_tokens, 
                            #  get a subset of articles that mention Hunter Biden
                            subset  = str_detect(pol_corpus, "immigr*"))

# convert to a quanteda dictionary format
policyagendas.dict <- dictionary(policyAgendas)

# apply the dictionary
agenda_politics <- tokens_lookup(pol_tokens, dictionary = policyagendas.dict, levels = 1)

# check out the first result: 
agenda_politics[[1]]



# Using a sentiment dictionary from elsewhere ----
#devtools::install_github("kbenoit/quanteda.dictionaries") 
#remotes::install_github("quanteda/quanteda.sentiment")
library(quanteda.sentiment)
# load the lexicoder sentiment dictionary
lsd<-quanteda.sentiment::data_dictionary_LSD2015

polarity<-textstat_polarity(pol_corpus, dictionary=lsd)|>
  # add polarity to existing document variables
  bind_cols(docvars(pol_corpus))|>
  
  # also add a column for mentions of immigration
  mutate(mentions_immigration = str_detect(pol_corpus,"immigr*"))

# plot polarity for all four groups
ggplot(polarity, aes(x=source, y=sentiment, fill=mentions_immigration)) + 
  geom_boxplot(notch=TRUE) +
  theme_minimal()


##Generating a custom dictionary------
# So if you want to create your own for a specific domain or language, or 
# extend an existing one, then you can create one using a named list like this:

custom_list<-list(COVID  =c('covid','coronavirus', 'vaccin*', 'lock down'),
                  Jan6 = c( "jan* 6*", "capitol", 'insurrection*'))



custom_dict <- dictionary(custom_list)
tok<-tokens_lookup(pol_tokens, dictionary=custom_dict)


table(unlist(tok))



#Part 2 Making a DFM -----------------------------------------------------------

# remove stop words 
toks_nostop <- tokens_select(pol_tokens, pattern = stopwords("en"), selection = "remove")
# convert to a document-feature matrix
pol_dfm<- dfm(toks_nostop)
# stem and then unstem words (using custom function in preamble to this document)
pol_dfm<-stem_unstem(pol_dfm) 
# if you don't want to use the stem_unstem function, run this instead:  
# pol_dfm<-dfm_wordstem(dfm)  

# remove features that occur less than 10 times
pol_dfm<-dfm_trim(pol_dfm, min_docfreq= 10)
# remove features that occur in more than 10% of documents 
# increasing max_docfreq may improve results, but it will take longer for models to converge
pol_dfm<- dfm_trim(pol_dfm, max_docfreq = 0.1, docfreq_type = "prop")


## Calculating frequent terms ----

tstat_freq <- textstat_frequency(pol_dfm, n = 5, groups = source)

tstat_freq


## modeling keyness ----



tstat_key <- textstat_keyness(pol_dfm, 
                              # find distinctive terms for Fox news compared to CNN :
                              target = docvars(pol_dfm)$source == "Fox News")
textplot_keyness(tstat_key)




