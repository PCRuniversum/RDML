library(shiny)

shinyUI(fluidPage(
  titlePanel("RDML import"),
  sidebarLayout(
    sidebarPanel(
      fileInput("rdml.file", "Choose RDML File",
                accept=c("application/zip", ".rdml")),
      textInput("pattern", "Name Pattern:", "%NAME%__%TUBE%"),
      HTML('<p>Possible keys in Name Pattern:<ul>
<li>%NAME% - name of the sample inputted in the qPCR software (ex.: "Sample 1")</li>
<li>%ID% - tube ID (ex.: "23")</li>
<li>%TUBE% - tube name (ex.: "B11")</li>
<li>%TARGET% - PCR target (ex.: "GAPDH")</li>
<li>%TYPE% - type of the sample (ex.: "unkn")</li></ul><p>'),
      checkboxInput("flat.table", "Flat Table", FALSE),
      checkboxInput("omit.ntp", "Omit 'ntp' Type Samples", TRUE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("RDML object structure", verbatimTextOutput('rdml.structure.out')),
        tabPanel("RDML object summary", verbatimTextOutput('rdml.summary.out'))
      )
    )
  )
))