library(shiny)
library(shinythemes)
library(rhandsontable)
library(dplyr)

shinyUI(
  navbarPage(
    title = "RDMLedit",
    theme = shinytheme("cerulean"),
    tabPanel("Files",
             includeMarkdown("md/files.md"),
             fileInput("rdmlFiles",
                       h4("Upload RDML File:"),
                       multiple = TRUE),
             rHandsontableOutput("filesTbl"),
             downloadLink('downloadRDML', 'Download RDML'),
             textOutput("errorText")),
    navbarMenu("Metadata",
               tabPanel("ID",
                        rHandsontableOutput("idTbl"),
                        textOutput("tr")),
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
  )
)
