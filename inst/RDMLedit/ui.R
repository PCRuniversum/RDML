library(shiny)
library(shinythemes)
library(rhandsontable)
library(dplyr)

shinyUI(
  tags$div(
  navbarPage(
    title = "RDMLedit",
    theme = shinytheme("cerulean"),
    tabPanel("Files",
             includeMarkdown("md/files.md"),
             fileInput("rdmlFiles",
                       h4("Upload RDML File:"),
                       multiple = TRUE),
             rHandsontableOutput("filesTbl"),
             textOutput("selectedFileText"),
             downloadLink("downloadRDML", "Download RDML")),
    navbarMenu("Metadata",
               tabPanel("ID",
                        rHandsontableOutput("idTbl")),
               tabPanel("Experimenter",
                        rHandsontableOutput("experimenterTbl")),
               tabPanel("Documentation",
                        "sfas"),
               tabPanel("Dye",
                        "sfas"),
               tabPanel("Sample",
                        "sfas"),
               tabPanel("Target",
                        "sfas"),
               tabPanel("Thermal Cycling Conditions",
                        "sfas")),
    tabPanel("Fluorescence Points")
  ),
  uiOutput("logText")
  )
)
