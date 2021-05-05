library("tidyverse")
library("shiny")
library("writexl")
library('rsconnect')

function(input,output,session){
  
  source("syllabus_20.R")
  source("syllabus_tf.R")
  
  observeEvent(input$upload_syllabus,{
    
    req(input$upload_syllabus)
    top_20<- syllabus_20_occupation(input$upload_syllabus$datapath)
    word_tf <- syllabus_occupation_tf(input$upload_syllabus$datapath)
    
    output$top20 <- renderTable({
      
      top_20
    })
  
    
    output$wordtf <- renderTable({
  
      word_tf
    })
  
    
    output$download_result1_table <- downloadHandler(
      filename = "top20.xlsx",
      content = function(file1){
         write_xlsx(top_20,file1)
         
     })
    
    output$download_result2_table <- downloadHandler(
      filename = "word_tf.xlsx",
      content = function(file2){
        write_xlsx(word_tf,file2)
      })
  })
  
}