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
             selectizeInput("rdmlFileSlct",
                            "Show File",
                            choices = "",
                            options= list(
                              create =TRUE
                            )),
             selectInput("mergeRdmlsSlct",
                         "Merge RDMLs",
                         multiple = TRUE,
                         choices = ""),
             textInput("dateMadeText", "Date Made",
                       ""),
             textInput("dateUpdatedText", "Date Updated",
                       ""),
             actionButton("removeRDMLBtn",
                          "Remove RDML"),
             actionButton("updateRDMLBtn",
                          "Update"),
             downloadLink("downloadRDML", "Download RDML")),
    navbarMenu("Metadata",
               tabPanel("ID",
                        selectizeInput("idSlct",
                                       "ID",
                                       choices = "",
                                       options= list(
                                         create =TRUE
                                       )),
                        textInput("idPublisherText", "Publisher",
                                  ""),
                        textInput("idSerialNumberText", "Serial Number",
                                  ""),
                        textInput("idMD5HashText", "MD5 Hash",
                                  ""),
                        actionButton("removeIDBtn",
                                     "Remove ID")),
               tabPanel("Experimenter",
                        selectizeInput("experimenterSlct",
                                       "ID",
                                       choices = "",
                                       options= list(
                                         create =TRUE
                                       )),
                        textInput("experimenterIdText", "ID",
                                  ""),
                        textInput("experimenterFirstNameText", "Serial Number",
                                  ""),
                        textInput("experimenterLastNameText", "MD5 Hash",
                                  ""),
                        textInput("experimenterEmailText", "MD5 Hash",
                                  ""),
                        textInput("experimenterLabNameText", "MD5 Hash",
                                  ""),
                        textInput("experimenterLabAdressText", "MD5 Hash",
                                  ""),
                        actionButton("removeExperimenterBtn",
                                     "Remove ID")),
               tabPanel("Documentation",
                        rHandsontableOutput("documentationTbl")),
               tabPanel("Dye",
                        rHandsontableOutput("dyeTbl")),
               tabPanel("Sample",
                        rHandsontableOutput("sampleTbl")),
               tabPanel("Target",
                        "sfas"),
               tabPanel("Thermal Cycling Conditions",
                        "sfas")),
    tabPanel("Fluorescence Points")
  ),
  uiOutput("logText")
  )
)
