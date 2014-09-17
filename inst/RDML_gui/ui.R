library(shiny)

shinyUI(fluidPage(
  titlePanel("RDML import"),
  sidebarLayout(
    sidebarPanel(
      fileInput("rdml_file", "Choose RDML File",
                accept=c("application/zip", ".rdml")),
      textInput("pattern", "Name Pattern:", "%NAME%__%TUBE%"),
      checkboxInput("flat.table", "Flat Table", FALSE),
      checkboxInput("omit.ntp", "Omit 'ntp' Type Samples", FALSE)
    ),
    mainPanel(
      helpText('RDML object summary:'),
      verbatimTextOutput('rdml.summary.out')
    )
  )
))