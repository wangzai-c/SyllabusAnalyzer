library("shiny")
library("DT")
library("tidyverse")


# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Uploading Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      #Input: Select a file ----
      fileInput("syllabus", "Choose Syllabus File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      downloadButton("download",
                     label = "Download Result Table"),
      # Output: Data file ----
      DTOutput("resultable")
      
    )
    
  )
) 

# Define server logic to read selected file ----
server <- function(input, output) {
    output$download <- downloadHandler(
      filename = "resultdata.csv",
      content = function(file){
        file.copy("cased.csv",
                  file)
      }
    )  
  
  
    output$resultable <- renderDT({
        read_csv(input$syllabus$datapath) %>%
          datatable(rownames = FALSE)
    })
}

# Create Shiny app ----
shinyApp(ui, server)