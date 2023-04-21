#list of packages required
list.of.packages <- c("shiny","bslib","shinydashboard","shinyjs", "shinyWidgets", "data.tree", "stringr") # "data.table",

#checking missing packages from list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#install missing ones
# if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)

# load required packages' libraries
# lapply(as.list(list.of.packages), library, character.only = T)
library(shinydashboard)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(data.tree)
library(stringr)

# header with title
header <- dashboardHeader(title = "Data harmonisation")

# sidebar contains menu items
sidebar <- dashboardSidebar(
  useShinyjs(),
  sidebarMenu(id = "tabs",
              menuItem("Upload your file(s)", tabName = "Upload", icon = icon("upload")),
              menuItem("Identify headers", tabName = "headers", icon = icon("arrows-alt")),
              # menuItem("Apply corrections", tabName = "Correct", icon = icon("check-circle")),
              menuItem("Visualise inputs", tabName="Visualise", icon = icon("eye")),
              menuItem("Save Profile", tabName="Save", icon = icon("save"))#,
              # menuItem("Help", tabName = "Manual", icon = icon("book"))
  )
)

body <- dashboardBody(
  tags$head(
    tags$style(
      HTML(".shiny-notification {
             position:fixed;
             top: calc(10%);
             left: calc(25%);}
             .dropdown-menu span {width: 94%;}"
      )
    ) # to make notification show up at top of page
  ),
  tabItems(

    tabItem(tabName = "Upload",

            fluidRow(
              column(width = 6,
                     actionBttn(
                       inputId = "inactivebutton",
                       label = "1",
                       style = "pill",
                       color = "danger"),
                     strong("How many tables do you wish to upload?"),
                     numericInput(inputId = "nTable",
                                  label = "",
                                  value = 1,
                                  min = 1,
                                  max = NA
                     )
              )),

            fluidRow(
              column(width = 12,
                     actionBttn(
                       inputId = "inactivebutton",
                       label = "2",
                       style = "pill",
                       color = "danger"),
                     strong("Upload your tables"),
              uiOutput("ui_uploadTables"))),


            fluidRow(
              column(width = 3,
                     actionBttn(
                       inputId = "inactivebutton",
                       label = "3",
                       style = "pill",
                       color = "danger"),
                     actionBttn(
                       inputId = "submit",
                       label = "submit",
                       style = "material-flat",
                       color = "success"
                     ))

            )

    ),  ## end of "upload" panel

    tabItem(tabName = "headers",
            fluidRow(
              column(width = 12,
                     h5("This may take a few seconds to show... BUT MAKE SURE YOU CLICKED ON 'SUBMIT' ON PREVIOUS TAB!"),
                     br(),
                     h4("In the menus below, select the option(s) that apply to each of your columns."),
                     p("Multiple choice allowed -  To clear a field, deselect all of the options."),
                     uiOutput("uiheader")
            ))
            )
    ,

    # tabItem(tabName = "Correct",
    #         radioButtons(inputId = "taper", label = "Apply taper corrections?", choices = list("Yes" = "Yes", "No" = "No"), selected = "No")
    #         ),

    tabItem(tabName = "Visualise",

            fluidRow(
                     actionBttn(
                       inputId = "update",
                       label = "update",
                       style = "material-flat",
                       color = "success"
                     ),
                     tableOutput("visualiseInput")

              # uiOutput(outputId = "visualiseInput")
              # actionButton("UpdateTable", label = "Update table!", style = "color: #fff; background-color: #009e60; border-color: #317256;   position: fixed")
            )


    ),  ## end of "visualize" panel

    tabItem(tabName = "Save",
            fluidRow(
              column(width = 3,
                     # box(title = "Save file",
                     #     width = NULL,
                     #     status = "primary",
                     #     solidHeader = TRUE,
                     #     downloadButton(outputId = "dbFile", label = "Save file")),
                     box(title = "Save profile",
                         width = NULL,
                         status = "primary",
                         solidHeader = TRUE,
                         downloadButton(outputId = "dbProfile", label = "Save profile"))#,
                     # box(title = "Save code",
                     #     width = NULL,
                     #     status = "primary",
                     #     solidHeader = TRUE,
                     #     downloadButton(outputId = "dbCode", label = "Save code")),
                     # box(title = "Save metadata",
                     #     width = NULL,
                     #     status = "primary",
                     #     solidHeader = TRUE,
                     #     downloadButton(outputId = "dbMetadata", label = "Save metadata"))
              )
            )
    ) # end of "save" panel

  )
)


ui <- dashboardPage(header, sidebar, body)
