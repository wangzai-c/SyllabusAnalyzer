

#install package 
#install.packages("dplyr")
#install.packages("tidytext")
#install.packages("textstem")
#install.packages("readr")
#install.packages("text2vec")
#install.packages("stringr")
#install.packages("magrittr")
#install.packages("stopwords")
#install.packages("textstem")
#install.packages("tm")
#install.packages("NLP")
#install.packages("tokenizers")
#install.packages("expss")
library(dplyr)
library(tidytext)
library(textstem)
library(readr)
library(text2vec)
library(stringr)
library(magrittr)
library(stopwords)
library(tm)
library(NLP)
library(tokenizers)
library(expss)


#read file
syllabus_occupation_tf = function(syllabus_file){
  cased <- read.csv(file.path("cased.csv"),stringsAsFactors = F)
  syllabus_f <- read_file(syllabus_file)
  #make syllabus content a dataframe
  syllabus_fdf <- tibble(job = "applied_project",description = syllabus_f)
  
  #combine syllabus and occupation description
  cased_d_syllabus <- rbind(syllabus_fdf, cased)
  
  #data cleaning function
  prep_fun = function(x){
    #make text lower case
    x = str_to_lower(x)
    #remove non-alphanumeric symbols
    x = str_replace_all(x,"[^[:alnum:]]"," ")
    #remove numbers
    x = gsub(patter="\\d",replace=" ",x)
    #remove stopwords
    x = removeWords(x,stopwords("en"))
    #remove single character
    x = gsub(patter="\\\b[A-z]\\b{1}",replace=" ",x)
    #collapse multiple spaces
    str_replace_all(x,"\\s+"," ")
    #lemmatize words
    x = lemmatize_strings(x)
  }
  
  # clean occupation description data and create a new column
  cased_d_syllabus$description_clean = prep_fun(cased_d_syllabus$description)
  
  #use vocabulary_based vectorization
  it_syllabus = itoken(cased_d_syllabus$description_clean, progressbar = FALSE)
  
  v_syllabus = create_vocabulary(it_syllabus)
  v_syllabus = prune_vocabulary(v_syllabus, doc_proportion_max = 0.1, term_count_min = 5)
  vectorizer_syllabus = vocab_vectorizer(v_syllabus)
  
  #apply TF_IDF transformation
  dtm_syllabus = create_dtm(it_syllabus, vectorizer_syllabus)
  tfidf = TfIdf$new()
  dtm_tfidf_syllabus = fit_transform(dtm_syllabus,tfidf)
  
  #compute similarity-score against each row
  syllabus_tfidf_cos_sin = sim2(x=dtm_tfidf_syllabus, method = c("cosine"), norm = c("l2"))
  
  syllabus_tfidf_cos_sin[1:15,1:15]
  
  #create a new column for similarity_score of dataframe
  cased_d_syllabus["similarity_score"] = syllabus_tfidf_cos_sin[1:1111]
  
  #sort the dataframe by similarity score
  cased_sim_syllabus = cased_d_syllabus[order(-cased_d_syllabus$similarity_score),]
  
  #get top 20 occupation from sorted dataframe
  top_20 = head(cased_sim_syllabus, n=21)
  top_20 = top_20[-1,]
  
  #get term frequency of all occupation description with order
  desc_tf <- tibble(txt = top_20$description_clean)
  word_tf <- desc_tf %>%
    unnest_tokens(word,txt) %>%
    count(word,sort = TRUE)
  
  word_tf <- subset(word_tf, word_tf$n >= 4)
  
  #syllabus
  #count_words(cased_d_syllabus$description_clean[1])
  #count_if(word_tf$word[1],word_list$word)
  #str_count(cased_d_syllabus$description_clean[1],word_tf$word[1])
  
  #for loop get top hot words in syllabus
  syllabus_tf <- list()
  for (desc_tf in word_tf$word){
    tf <- str_count(cased_d_syllabus$description_clean[1],desc_tf)
    syllabus_tf <- append(syllabus_tf,tf)
  }
  
  df_syllabus <- data.frame(matrix(unlist(syllabus_tf),nrow=length(syllabus_tf),byrow=TRUE),stringsAsFactors = FALSE)
  
  # combine syllabus tf with word tf
  word_tf <- cbind(word_tf,df_syllabus)
  
  # rename result df for easier understanding of data
  word_tf <- word_tf %>%
    rename(occupation_desc_tf = n)
  
  word_tf <- word_tf %>%
    rename(syllabus_tf = matrix.unlist.syllabus_tf...nrow...length.syllabus_tf...byrow...TRUE.)
  
  
  return(word_tf)
}

#syllabus_occupation_tf("/Users/qihuisun/Desktop/NYU/applied_project/syllabus_analyzer/applied_project.txt")





