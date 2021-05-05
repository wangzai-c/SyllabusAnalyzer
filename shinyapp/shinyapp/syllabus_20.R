

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
syllabus_20_occupation = function(syllabus_file){
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

  return(top_20)
}


#syllabus_20_occupation("/Users/qihuisun/Desktop/NYU/applied_project/syllabus_analyzer/applied_project.txt")
  




