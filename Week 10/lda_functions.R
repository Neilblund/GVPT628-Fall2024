library(kableExtra)
library(tidyverse)
lda_top_docs<-function(theta, num_docs =5, topics=colnames(theta), docnames=rownames(theta)){

  result<-theta|>
    as.data.frame()|>
    select(all_of(topics))|>
    mutate(docnames = docnames)|>
    pivot_longer(cols=-docnames, names_to='topic', values_to='prob')|>
    group_by(topic)|>
    arrange(desc(prob))|>
    slice_head(n = num_docs)|>
    mutate(rank = seq(n()))
  
  return(result)
  
}

lda_top_words<-function(phi , num_words=5, topics=rownames(phi)){
  result <- phi |>
    as.data.frame() |>
    rownames_to_column(var = '__topic__') |>
    pivot_longer(cols = -`__topic__`,
                 names_to = 'term',
                 values_to = 'prob') |>
    group_by(`__topic__`) |>
    arrange(desc(prob)) |>
    # number of terms you want to include
    slice_head(n = num_words) |>
    mutate(rank = seq(n()), term_label = reorder(paste(term, `__topic__`, sep =
                                                         "__"), -rank)) |>
    rename(topic = `__topic__`)
  return(result)
}


topics_renamer<-function(topics){
  numbers<-str_extract(topics, "topic([0-9]+)", group=1)
  paste0("topic ", str_pad(numbers, width=max(nchar(numbers)), pad='0', side='left'))
}

linker<-function(text, url){
  sprintf("<a href='%s'>%s</a>", url, text)
}


makeKable<-function(terms, docs, topics_rename=FALSE, fontsize=9){
  
  terms_collapsed<-terms|>
    group_by(topic)|>
    summarise(terms = paste(term, collapse=", "))
  
  topics_collapsed<-docs|>
    group_by(topic)|>
    summarise(documents = paste(docnames, collapse="<br>"))
  
  combined<-inner_join(terms_collapsed, topics_collapsed)
  if(topics_rename ==TRUE){
    combined$topic<-topics_renamer(combined$topic)
  }
  combined|>
    kbl(escape=FALSE)|>
    kable_styling(font_size = fontsize)|>
    kable_paper("striped", full_width = F)|>
    collapse_rows(columns=1:2)
  
}

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



topic_coherence<-function(lda_model){
  tw<-apply(lda_model$phi, 1, function(x) colnames(lda_model$phi)[order(x, decreasing=T)[1:5]])
  
  
  dtm<-lda_model$data
  
  for (i in 1:ncol(tw)) {
    real = tw[,i]
    intruder = sample(setdiff(colnames(dtm), real), 1)
    options = sample(c(real, intruder), 6)
    cat(paste(paste(1:6, options, sep=':  '), collapse='\n'))
    answer = readline(prompt="Which is the intruder? [1-6]  ")
    if (options[as.numeric(answer)] == intruder) 
      message("CORRECT!!\n") 
    else 
      message(sprintf('WRONG!! it was "%s"\n', intruder))
  }
  
}



