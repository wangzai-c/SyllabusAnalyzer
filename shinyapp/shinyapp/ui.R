navbarPage(
  "Syllabus Analyzer",
  tabPanel(
    "Upload syllabus",
    fluidPage(
      wellPanel(p("Please upload your syllabus file as a plain text file."),
                p("When finished, you can download two result tables as excel file.")),
      sidebarLayout(
        sidebarPanel(
          
          fileInput("upload_syllabus",
                    label = "Upload Syllabus",
                    multiple = FALSE,
                    accept = c(".txt")
        ),
        
        tags$hr(),
        
        downloadButton("download_result1_table",
                       "Download Top 20 Occupation"),
        downloadButton("download_result2_table",
                       "Download Word Frequency"),
      ),
        
      
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Top 20 Occupations",
            tableOutput("top20")
          ),
          
          tabPanel(
            "Syllabus Occupation Keyword Frequency",
            tableOutput("wordtf")
          )
        )
        
      )
    )
  ),
  collapsible = TRUE
)
)