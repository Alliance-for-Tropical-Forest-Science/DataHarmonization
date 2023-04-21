#list of packages required
list.of.packages <- c("shiny","bslib","DT","shinydashboard","shinyjs", "shinyWidgets", "data.table")

#checking missing packages from list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#install missing ones
# if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)

# load required packages' libraries
lapply(as.list(list.of.packages), require, character.only = T)



# header with title
header <- dashboardHeader(title = "Data harmonisation")

# sidebar contains menu items
sidebar <- dashboardSidebar(
  useShinyjs(),
  sidebarMenu(
              menuItem("Upload your file", tabName = "Upload", icon = icon("upload")),
              menuItem("Identify headers", tabName = "headers", icon = icon("arrows-alt")),
              menuItem("Apply corrections", tabName = "Correct", icon = icon("check-circle")),
              menuItem("Visualise results", tabName="Visualise", icon = icon("eye")),
              menuItem("Save codes and data", tabName="Save", icon = icon("save")),
              menuItem("Help", tabName = "Manual", icon = icon("book"))
  )
)

body <- dashboardBody(
  tags$head(
    tags$style(
      HTML(".shiny-notification {
             position:fixed;
             top: calc(10%);
             left: calc(25%);
             }
             "
      )
    ) # to make notification show up at top of page
  ),
  tabItems(

    tabItem(tabName = "Upload",

            fluidRow(

              column(width = 3,

                     # load button for main data file (csv format)
                     box(title = "Upload your data",
                         width = NULL,
                         fileInput(inputId = "file1", "Choose CSV File", accept = ".csv"),
                         # does the dataframe have a header?
                         checkboxInput("header", "Header", TRUE),
                         # choose separator
                         selectInput(inputId = "cbSeparator",
                                     label = "Separator",
                                     choices = c("auto", ",", "\t",  "|", ";", ":"), # pb with tab
                                     selected = "auto"
                         )
                     )
              ),

              column(width = 9,
                     DTOutput(outputId = "tabData")
              )

            )
    ),  ## end of "upload" panel

    tabItem(tabName = "headers",
            fluidRow(
              # inform if profile already exists
              box(width = 12,
                  radioButtons(inputId = "predefinedProfile",
                               label = div("Use a predifined format?", br(), em("(if your data follows one of the following network template)")),
                               choices = list("No thanks!" = "No", "ATDN: The Amazon Tree Diversity Network" = "ATDN", "ForestGEO: The Smithsonian Forest Global Earth Observatory" = "ForestGEO", "RBA: Red de Bosques Andinos" = "RBA"),selected = "No"),

                  # load a profile it one already exists
                  fileInput(inputId = "profile", div("Load your own profile", br(), em("(if you already used this app and saved your profile (.rds))")), accept = ".rds")
                  ),

              # inform if long or wide format
              box(width = 12,
                  radioButtons(inputId = "format",
                           label = div(actionBttn(
                             inputId = "inactivebutton",
                             label = "1",
                             style = "pill",
                             color = "danger"),
                             "Is your data in long or wide format?", br(), em("(Wide format not implemented yet)")),
                           choices = list("Long" = "long", "Wide" = "wide"))),
              column(width = 6,
                     actionBttn(
                       inputId = "inactivebutton",
                       label = "2",
                       style = "pill",
                       color = "danger")
                     ,   strong("  Match your columns to ours (if you can)"),
                     br(),
                     br(),
                     box(
                       # title = "Match your columns to ours (if you can)",
                         width = NULL,
                         # status = "primary",
                         # solidHeader = TRUE,
                         uiOutput("ui1"))
              ),
              column(width = 6,
                     actionBttn(
                       inputId = "inactivebutton",
                       label = "3",
                       style = "pill",
                       color = "danger")
                     ,   strong("  Fill in information that is not in your columns"),
                     br(),
                     br(),
                     box(
                       # title = "",
                         width = NULL,
                         # column(width = 5,
                         # box(title = "Tell us more about your plot",
                         #   width = NULL,
                         #   status = "primary",
                         #   solidHeader = TRUE,
                         #   h4("Only fill this infomation if it is not in a column!"),
                         uiOutput("ui2"),
                         #)
                         # ),


                         # column(width = 5,
                         # box(title = "Tell us about your units",
                         #   width = NULL,
                         #   status = "primary",
                         #   solidHeader = TRUE,
                         #   h4("Only fill this infomation if it is not in a column!"),
                         #   p("Note: we are not able to handle units varying by rows yet..."),
                         uiOutput("ui3"),
                         #        ),
                         # column(width = 5,
                         # box(title = "A couple more things...",
                         #     width = NULL,
                         #     status = "primary",
                         #     solidHeader = TRUE,
                         uiOutput("ui4"),
                         # )
                         # ),
                         # column(width = 5,
                         # box(title = "and lastly...",
                         #     width = NULL,
                         #     status = "primary",
                         #     solidHeader = TRUE,
                         uiOutput("ui5")
                         # )


                     ),


              actionButton("LaunchFormating", label = "Launch formating!", style = "color: #fff; background-color: #009e60; border-color: #317256") #;   position: fixed
              )


    )),

    tabItem(tabName = "Correct",
            radioButtons(inputId = "taper", label = "Apply taper corrections?", choices = list("Yes" = "Yes", "No" = "No"), selected = "No")
            ),

    tabItem(tabName = "Visualise",

            fluidRow(

              column(width = 10,
                     DTOutput(outputId = "tabDataFormated")
              ),
              actionButton("UpdateTable", label = "Update table!", style = "color: #fff; background-color: #009e60; border-color: #317256;   position: fixed")
            )


    ),  ## end of "visualize" panel

    tabItem(tabName = "Save",
            fluidRow(
              column(width = 3,
                     box(title = "Save file",
                         width = NULL,
                         status = "primary",
                         solidHeader = TRUE,
                         downloadButton(outputId = "dbFile", label = "Save file")),
                     box(title = "Save profile",
                         width = NULL,
                         status = "primary",
                         solidHeader = TRUE,
                         downloadButton(outputId = "dbProfile", label = "Save profile")),
                     box(title = "Save code",
                         width = NULL,
                         status = "primary",
                         solidHeader = TRUE,
                         downloadButton(outputId = "dbCode", label = "Save code")),
                     box(title = "Save metadata",
                         width = NULL,
                         status = "primary",
                         solidHeader = TRUE,
                         downloadButton(outputId = "dbMetadata", label = "Save metadata"))
              )
            )
    ) # end of "save" panel

  )
)


ui <- dashboardPage(header, sidebar, body)
