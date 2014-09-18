library(shiny)

shinyUI(fluidPage(
  titlePanel("RDML import"),
  sidebarLayout(
    sidebarPanel(
      fileInput("rdml.file", "Choose RDML File",
                accept=c("application/zip", ".rdml")),
      textInput("pattern", "Name Pattern:", "%NAME%__%TUBE%"),      
      tags$ul("Possible keys in Name Pattern:",              
              tags$li('%NAME% - name of the sample inputted in the qPCR software (ex.: "Sample 1")'),
              tags$li('%ID% - tube ID (ex.: "23")'),
              tags$li('%TUBE% - tube name (ex.: "B11")'),
              tags$li('%TARGET% - PCR target (ex.: "GAPDH")'),
              tags$li('%TYPE% - type of the sample (ex.: "unkn")')),
      checkboxInput("flat.table", "Flat Table", FALSE),
      checkboxInput("omit.ntp", "Omit 'ntp' Type Samples", TRUE),
      actionButton("update", "Update")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("RDML object structure", verbatimTextOutput('rdml.structure.out')),
        tabPanel("RDML object summary", verbatimTextOutput('rdml.summary.out')),
        tabPanel("Table/Plots",                 
                 tags$h4("Filter by:"),
                 uiOutput("ui.targets"),
                 uiOutput("ui.types"),
                 textInput("names.filter", "Names:", ""),
                 tableOutput("overview.table"),
                 plotOutput("overview.plot"))                
      )
    )
  )
))