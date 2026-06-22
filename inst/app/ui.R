
# header with title
header <- dashboardHeader(title = "Data harmonisation",
                          tags$li(class = "dropdown",
                                  dropdownMenu(type = "messages",
                                               # from for first line, message 2nd line smaller font
                                               messageItem(
                                                 from = "Project in Github",
                                                 message = "Documentation, Source, Citation",
                                                 icon = icon("github"),
                                                 href = "https://github.com/Alliance-for-Tropical-Forest-Science/DataHarmonization"),
                                               messageItem(
                                                 from = "Issues",
                                                 message = "Report Issues",
                                                 icon = icon("exclamation-circle"),
                                                 href = "https://github.com/Alliance-for-Tropical-Forest-Science/DataHarmonization/issues"),
                                               badgeStatus = NULL,
                                               icon = icon("info-circle"),
                                               # icon = fontawesome::fa("info-circle"),
                                               headerText = "App Information"
                                  )
                                  ,
                                  if(isDebugging()) {
                                    tags$li(class = "dropdown", actionButton("browser", "browser", icon  =  icon("r-project")))
                                  }
                          )
                          # tags$li(class = "dropdown",
                          #
                          #         pickerInput ("languages", NULL, width = "auto",
                          #                     choices = languages,
                          #
                          #                     choicesOpt = list(content =
                          #                                         mapply(languages, flags, FUN = function(country, flagUrl) {
                          #                                           HTML(paste(
                          #                                             tags$img(src=flagUrl, width=20, height=15),
                          #                                             country
                          #                                           ))
                          #                                         }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
                          #
                          #                     ))
                          # )
                          )

# sidebar contains menu items
sidebar <- dashboardSidebar(
  useShinyjs(),
  askBeforeClose(),
  sidebarMenu(id = "tabs", # see here for icons https://fontawesome.com/v5/search
              menuItem("Upload your file(s)", tabName = "Upload", icon = icon("upload")),
              menuItem("Stack tables", tabName = "Stacking", icon = icon("layer-group")),
              menuItem("Merge tables", tabName = "Merging", icon = icon("key")),
              menuItem("Tidy table", tabName = "Tidying", icon = icon("check")),
              menuItem("Headers and Units", tabName = "Headers", icon = icon("arrows-alt")),
              menuItem("Codes", tabName = "Codes", icon = icon("table", verify_fa = F)),
              menuItem("Corrections", tabName = "Correct", icon = icon("check-circle")),
              menuItem("Output format", tabName = "OutputFormat", icon = icon("sign-out", verify_fa = FALSE)),
              # menuItem("Visualise results", tabName="Visualise", icon = icon("eye")),
              menuItem("Download", tabName="Save", icon = icon("save")),
              menuItem("Help", tabName = "Help", icon = icon("book"))
  )
)

body <- dashboardBody(
  tags$head(

    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/select2/4.0.13/css/select2.min.css"),# this is to edit Codes table
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/select2/4.0.13/js/select2.min.js"), # this is to edit Codes table
    # tags$script(src = "https://code.jquery.com/jquery-3.5.1.js"), # this is to allow grouping of rows in Code translation table
    # tags$script(src = "https://cdn.datatables.net/1.12.1/js/jquery.dataTables.min.js"), # this is to allow grouping of rows in Code translation table
    tags$style(
      HTML(".shiny-notification {
             position:fixed;
             top: calc(10%);
             left: calc(25%);
             @import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);
      }

             .dropdown-menu{z-index:10000 !important;}
             .sw-dropdown-content {z-index: 3000005 !important;}
             .sw-dropdown-in {z-index: 3000006 !important;}
             .vscomp-search-container {z-index: 3000005 !important;}
             .vscomp-dropbox-container {z-index: 3000005 !important;}
             tr.dtrg-group {cursor: pointer;}
             table-layout: fixed;

           "

      )
    ) # to make notification show up at top of page
  ),
  tabItems(

    tabItem(tabName = "Upload",

            fluidRow(
              box(width = 12,
              actionBttn(
                inputId = "inactive1",
                label = div(
                  strong("If your connexion is slow and/or your data is very large, you may want to run this app locally. For that, open R Studio and type:"),
                  br()),
                style = "stretch",
                color = "success"),

                  helpText(code('devtools::install_github("Alliance-for-Tropical-Forest-Science/DataHarmonization", build_vignettes = TRUE)'),
                           br(),
                           code('DataHarmonization::run_app()'),
                           br(),
                           br(),
                           p("You may need to install devtools package first."),
                           p("Installing the DataHarmonization R package may ask you to update a list packages."),
                           strong("Please, re-install the package every once in a while, to get the latest version of the app.")),
                  tags$head(tags$style("#CodeRunApp{
                  color: red;
                  font-family: courier;
                  font-size: 100%;
                                 }"
                  ))),
              br(),
              br(),
              box(title = "checklist",
                  width = 12,

                  strong("You don't actually need to check the boxes here. This is just a guide to help you get prepared before you start using the app."),
                  br(),
                  br(),

                 # dropdownButton(width = NULL,
                 checkboxInput(
                   inputId = "ChckLst1",
                   label = "All relevant data is gathered in one of more tables (measurements, species info, plot info...)"
                   # status = "warning"
                 ),
                 checkboxInput(
                   inputId = "ChckLst2",
                   label = "Input table(s) are prepared as CSV file(s).",
                   # status = "warning"
                 ),
                 checkboxInput(
                     inputId = "ChckLst3",
                     label = "If tables will need to be stacked, assure they have the exact same columns and in same order."
                     # status = "warning"
                   ),
                 checkboxInput(
                     inputId = "ChckLst4",
                     label = "If tables need to be merged, assure you have the list of all key column(s) that are present in both tables."
                     # status = "warning"
                   ),
                 checkboxInput(
                   inputId = "ChckLst5",
                   label = "Remove 'empty' columns without column names"
                   # status = "warning"
                 ),
                 checkboxInput(
                   inputId = "ChckLst6",
                   label = "Save your csv in UTF-8 if you have special characters"
                   # status = "warning"
                 ),
                 checkboxInput(
                   inputId = "ChckLst7",
                   label = "If your stem identifiers use points (e.g. 123.1, 123.2, 123.3,..., 123.10), please change '.' to '_' so the columns is read as a character and not as a numeric, which would cause the loss of the infromation of the 10th stem as 123.10 would be changed to 123.1"
                   # status = "warning"
                 ),
                 checkboxInput(
                   inputId = "ChckLst8",
                   label = "..."
                   # status = "warning"
                 ),

                circle = TRUE, status = "danger",
                label  = tags$h2("Checklist before you upload"),
                icon = icon("cog"),
                inline =T,
                tooltip = tooltipOptions(title = "Click to see checklist !")

                ),
              br(),
              br(),

              box(width = 12,
                  solidHeader = T,
                  status = "primary",
                  title = "I - Load your data",

              column(width = 6,
                     actionBttn(
                       inputId =  "inactive2",
                       label = "1",
                       style = "pill",
                       color = "warning"),
                     strong("How many tables do you need to upload?"),
                     numericInput(inputId = "nTable",
                                  label = "",
                                  value = 1,
                                  min = 1,
                                  max = NA
                     )
              ),
              column(width = 6,
                     actionBttn(
                       inputId =  "inactive3",
                       label = "2",
                       style = "pill",
                       color = "warning"),
                     strong("What is your deepest level of measurements?"),
                     br(),
                     br(),
                     p("You may still upload higher level table(s) along."),
                     radioButtons(
                       inputId = "MeasLevel",
                       label = "",
                       choices = c("Plot", "Species", "Tree", "Stem"),
                       selected  = character(0)
                     )
              ),


              column(width = 6,
                     actionBttn(
                       inputId =  "inactive4",
                       label = "3",
                       style = "pill",
                       color = "warning"),
                     strong("Upload your tables"),

                     p("To keep track of your tables in the next steps, you can rename them at the top of each box."),

                     uiOutput("uiUploadTables")),
              column(6,
                     uiOutput("uiViewTables"))

            ),
            box(width = 12,
                solidHeader = T,
                status = "primary",
                title = "II - Load your profiles",
                box( width = 12, status = "success",
                     title = "A - INPUT profile",
                    strong("Does your data already have a description in this app?"),
                        h4("If yes, pick from the list below"),
box(width = 12,
                    radioButtons(inputId = "predefinedProfile",
                                 label = div("Use a predifined INPUT profile", br(), em("(if your data follows one of the following network template)")),
                                 choices = list("No thanks, I'll upload a .rds file or start from scratch!" = "No",
                                                # "ATDN: The Amazon Tree Diversity Network" = "ATDN",
                                                "ForestGEO: The Smithsonian Forest Global Earth Observatory" = "ForestGeo",
                                                "App's profile (if the data you upload was downloaded from this app, using this app's standards)" = "App"#,
                                                # "RBA: Red de Bosques Andinos" = "RBA"
                                 ),
                                 selected = "No"),
                    # load a profile it one already exists
                    fileInput(inputId = "profile", div("Upload your own INPUT profile", br(), em("(if you already used this app and saved your profile (.rds))")), accept = ".rds"),
                    span(textOutput("RDSWarning"), style="color:red")),

                    h4("If no, you'll built your own profile in the in tab 'headers and units' ")),
                box( width = 12, status = "warning",
                     title = "B - OUTPUT profile",
                     strong("Pick or upload description of your desired output"),
                     p("Pick from the list, or provide a .rds object. This is typically a consensus format in collaborations, and may be provided to you by the person(s) responsible for data aggregation. It could also be the format of a network or repository in which you want to integrate your data."),
                     box(width = 12,
                     radioButtons(inputId = "predefinedProfileOutput",
                                  label = div("Use a predifined OUTPUT profile"),
                                  choices = list("No thanks! I'll upload a profile I have handy." = "No",
                                                 "This App's standard" = "App",
                                                 # "ATDN: The Amazon Tree Diversity Network" = "ATDN",
                                                 "ForestGEO: The Smithsonian Forest Global Earth Observatory" = "ForestGeo"#,
                                                 # "RBA: Red de Bosques Andinos" = "RBA"
                                  ),
                                  selected = "No"),

                     # load a profile it one already exists
                     div(id = "profileOutputfileInput",
                         fileInput(inputId = "profileOutput", div("Or, load a profile you have on your machine", br(), em("(if you or a colleague already used this app and saved a profile (.rds))")), accept = ".rds"))),
                     span(textOutput("RDSOutputWarning"), style="color:red"))),
box(width = 12,
    solidHeader = T,
    status = "primary",
    title = "III - Submit",

    # actionBttn(
    #   inputId =  makeUniqueID("inactive"),
    #   label = "4",
    #   style = "pill",
    #   color = "warning"),
    # strong("Click on Submit when it appears"),
    hidden(actionBttn(
      inputId = "submitTables",
      label = "submit",
      style = "material-flat",
      color = "success"
    ))
)
)
    ),  ## end of "upload" panel


      tabItem(tabName = "Stacking",

              div(
                actionBttn(
                  inputId =  "inactive5",
                  label = " ! ",
                  style = "pill",
                  color = "danger"),
                strong("make sure you clicked on 'Submit' in Upload tab"),


                column(width = 12,
                       includeMarkdown("www/Stack.md"),

                       code("If you have no tables to stack, skip this step."),
                       checkboxGroupButtons("TablesToStack", choices = ""),
                       actionBttn(
                         inputId = "Stack",
                         label = "Stack tables",
                         style = "material-flat",
                         color = "success"
                       ),
                       actionBttn(
                         inputId = "SkipStack",
                         label = "Skip this step",
                         style = "material-flat",
                         color = "warning"
                       ),
                       hidden( actionBttn(
                         inputId = "GoToMerge",
                         label = "Go To Merge",
                         style = "material-flat",
                         color = "success"
                       ),
                       actionBttn(
                         inputId = "SkipMerge",
                         label = "Skip Merging since all your data is now stacked",
                         style = "material-flat",
                         color = "warning"
                       ))

                )
                ),
              fluidRow(

                column(width = 12,
                       h4("View of your stacked tables:"),
                       DT::DTOutput(outputId = "StackedTables"),
                       h4("summary of your stacked tables:"),
                       verbatimTextOutput("StackedTablesSummary")
                )
              )


      ),  ## end of "Stacking" panel


    tabItem(tabName = "Merging",

            div(
              actionBttn(
                inputId =  "inactive6",
                label = " ! ",
                style = "pill",
                size ='xs',
                color = "danger"),
              strong("make sure you clicked on 'Sumbit' in Upload tab (and `Stack tables` in Stack tab, if used) "),

              column(width = 12,
                     includeMarkdown("www/Merge.md"),

                     box(width = 12,

                         fluidRow(column(3, pickerInput("leftTable", "Merge this table", choices = "")),
                                  column(2, br(), actionButton("selectLeft", "", icon = icon("arrow-right"),class = "btn-info", style = "color: #fff")),
                                  column(7,  hidden(shinyWidgets::virtualSelectInput("leftKey", div("Using this/these KEY column(s)", br(), em("Please, select all columns common to both tables. The order you select them matters")), choices = "", multiple = T, search = T, optionsCount = 6)))),

                         fluidRow(column(3, pickerInput("rightTable", "And this table", choices = "")),
                                  column(2, br(), actionButton("selectRight", "", icon = icon("arrow-right"),class = "btn-info", style = "color: #fff")),
                                  column(7,  hidden(shinyWidgets::virtualSelectInput("rightKey", div("Using this/these KEY column(s)", br(), em("Please, select all columns common to both tables. The order you select them matters")), choices = "", multiple = T, search = T, optionsCount = 6)))),

                         actionBttn(
                           inputId = "Merge",
                           label = "Merge tables",
                           style = "material-flat",
                           color = "success")
                     ),
                     fluidRow(
                       hidden(actionBttn(inputId = "addMerge",  label =  span(em("Add a Merging relationship", strong("(You need to end up with only one table)"))),
                                         style = "material-flat",
                                         color = "danger")),
                     ),
                     hidden(div(id ="Merge2Div", box(width = 12,

                                                     fluidRow(column(3, pickerInput("leftTable2", "Merge this table", choices = "")),
                                                              column(2, br(),actionButton("selectLeft2", "", icon = icon("arrow-right"),class = "btn-info", style = "color: #fff")),
                                                              column(7,  hidden(shinyWidgets::virtualSelectInput("leftKey2", div("Using this/these KEY column(s)", br(), em("Please, select all columns common to both tables. The order you select them matters.")), choices = "", multiple = T, search = T, optionsCount = 6)))),

                                                     fluidRow(column(3, pickerInput("rightTable2", "And this table", choices = "")),
                                                              column(2, br(),actionButton("selectRight2", "", icon = icon("arrow-right"),class = "btn-info", style = "color: #fff")),
                                                              column(7,  hidden(shinyWidgets::virtualSelectInput("rightKey2", div("Using this/these KEY column(s)", br(), em("Please, select all columns common to both tables. The order you select them matters")), choices = "", multiple = T, search = T, optionsCount = 6)))),
                                                     actionBttn(
                                                       inputId = "Merge2",
                                                       label = "Merge tables",
                                                       style = "material-flat",
                                                       color = "success"
                                                     )
                     ))),

                     hidden( actionBttn(
                       inputId = "GoToTidy",
                       label = "Go To Tidy",
                       style = "material-flat",
                       color = "success"
                     ))
              )
            ),
            fluidRow(

              column(width = 12,
                     h4("View of your merged tables:"),
                     DT::DTOutput(outputId = "mergedTables"),
                     h4("summary of your merged tables:"),
                     verbatimTextOutput("mergedTablesSummary")
              )
            )


    ),  ## end of "Merging" panel

    tabItem(tabName = "Tidying",

            includeMarkdown("www/Tidy.md"),

            code("If you already have one observation per row, skip this step."),

            actionBttn(
              inputId = "SkipTidy",
              label = "Skip this step",
              style = "material-flat",
              color = "warning"
            ),

            hidden(actionBttn(
              inputId = "Tidy",
              label = "Tidy",
              style = "material-flat",
              color = "success"
            ),
            hidden( actionBttn(
              inputId = "GoToHeaders",
              label = "Go To Headers",
              style = "material-flat",
              color = "success"
            ))),
            box(width = 12,
                radioButtons(
              "VariableName",
              "Why do you have repeated column?",
              choices = c("One column per census (new column name will be 'CensusID')" = "CensusID", "One column per height of measurement, measurement method, ... (new column name will be 'MeasureID')" = "MeasureID", "One column per stem (new column name will be 'StemID')" = "StemID", "One column per year (new column name will be 'Year')" = "Year"),
              selected = "",
              inline = FALSE
            ),
            actionButton("ClearValueName","Clear")),
            br()
,            h3("Tick the grouping(s) that should be applied and fix the prefilled information if necessary."),

            uiOutput("uiMelt"),



            fluidRow(

              column(width = 12,
                     h4("View of your tidy table:"),
                     DT::DTOutput(outputId = "TidyTable"),
                     h4("summary of your tidy table:"),
                     verbatimTextOutput("TidyTableSummary")
              ))


            ), ## end of "Tidy" panel
    tabItem(tabName = "Headers",
            fluidRow(
              # column(width = 12,
              #        h3("Indicate the meaning of your headers and your units.")),
              # # inform if profile already exists
              # box(width = 12,
              #     radioButtons(inputId = "predefinedProfile",
              #                  label = div("Use a predifined profile", br(), em("(if your data follows one of the following network template)")),
              #                  choices = list("No thanks!" = "No",
              #                                 # "ATDN: The Amazon Tree Diversity Network" = "ATDN",
              #                                 "ForestGEO: The Smithsonian Forest Global Earth Observatory" = "ForestGeo",
              #                                 "App's profile (if the data you upload was downloaded from this app, using this app's standards)" = "App"#,
              #                                 # "RBA: Red de Bosques Andinos" = "RBA"
              #                  ),
              #                  selected = "No"),
              #
              #     # load a profile it one already exists
              #     fileInput(inputId = "profile", div("Upload your own profile", br(), em("(if you already used this app and saved your profile (.rds))")), accept = ".rds"),
              #     span(textOutput("RDSWarning"), style="color:red"),
              # ),

              column(width = 6,
                     actionBttn(
                       inputId = "inactive7",
                       label = "1",
                       style = "pill",
                       color = "warning"),
                     strong("  Match your columns to ours (when you can)"),
                     br(),
                     br(),
                     box(width = NULL, title = "Focusing on a subset of columns",
                         pickerInput("SelectedColumns", "You may select the columns you want to focus on here:", choices = "", options = list(`actions-box` = TRUE),multiple = T)),
                     hidden(actionBttn(
                       inputId = "UseProfile",
                       label = "Click to use your INPUT profile",
                       style = "pill",
                       color = "success")
                     ),
                     br(),
                     h4("Check (or fill out if you did not upload a profile) each drop-down menus below."),
                     h4("Work one step at a time."),
                     br(),

                     box(
                         width = NULL,
                         # uiOutput("ui1"),
                         div(id="mainWrapper",

                        lapply(unique(x1$Group), function(g) {div(h3(g),
                          dropdown(
                            h3(g),
                            do.call(div, lapply(which(x1$Group %in% g), function(i) {

                              eval(parse(text = paste0(x1$ItemType[i], "(inputId = x1$ItemID[i], label = ifelse(x1$helpText[i] %in% '', x1$Label[i], paste0(x1$Label[i], ' (', x1$helpText[i], ')')),", x1$Argument[i]," ='",  x1$Default[i],"'", ifelse(x1$Multiple[i] %in% TRUE, ", multiple = TRUE)", ")"))))

                            })),
                            label = g,
                            icon = icon("sliders", verify_fa = FALSE),
                            size = "lg",
                            circle = FALSE,
                            tooltip = tooltipOptions(title = "Click to see inputs !")

                         )
                        )
                         })
                         )

                           # lapply(1:nrow(x1), function(i) {
                           #
                           #   eval(parse(text = paste0(x1$ItemType[i], "(inputId = x1$ItemID[i], label = ifelse(x1$helpText[i] %in% '', x1$Label[i], paste0(x1$Label[i], ' (', x1$helpText[i], ')')),", x1$Argument[i]," ='",  x1$Default[i],"'", ifelse(x1$Multiple[i] %in% TRUE, ", multiple = TRUE)", ")"))))
                           #
                           # })

                         # actionBttn("Header1Next", "next", style = "fill", color = "primary")
                         ),
              ),

              column(width = 6,
                     actionBttn(
                       inputId = "inactive8",
                       label = "Info",
                       style = "pill",
                       color = "default"),
                     strong("  Visualization of your original headers and content"),
                     br(),
                     br(),
                     box(
                       width = NULL,
                       DT::DTOutput(outputId = "VizForHeaders")
                     )
              ),
              column(width = 12,
                     div(
                       actionBttn(
                         inputId = "inactive9",
                         label = "2",
                         style = "pill",
                         color = "warning")
                       ,   strong("  Fill in information that is not in your columns"),
                       p("ATTENTION: do this after completing step 1 otherwise this will get overwritten."),
                       br(),
                       hidden(div( id = "AttentionDates",
                                   box(width = 12,
                                       actionBttn(
                                         inputId =  "inactive10",
                                         label = "!",
                                         style = "pill",
                                         color = "danger"),
                                       strong("pay attention to your Date format and double check it in step 2, even if you imported a profile."),
                                       p("A sample or your dates look like this:"),
                                       textOutput("sampleDates")))),
                       br(),


                       # lapply(which(x$ItemID %in% unlist(lapply(list(x2, x3, x4, x5, x6), "[[", "ItemID"))), function(i) {
                       #
                       #   eval(parse(text = paste0(x$ItemType[i], "(inputId = x$ItemID[i], label = ifelse(x$helpText[i] %in% '', x$Label[i], paste0(x$Label[i], ' (', x$helpText[i], ')')),", x$Argument[i], "='", x$Default[i], "'", ifelse(x$Options[i] != FALSE, paste0(", options = ", x$Options[i]), ""), ifelse(x$Multiple[i] %in% TRUE, paste0(", multiple = TRUE, selected = '", x$Default[i], "')"), ")"))))
                       #
                       # }),

                       div(id="seconColumnWrapper",

                           lapply(unique(secondColumn$GroupSecondColumn), function(g) {div(h3(g),
                                                                       # h3(g),
                                                                       do.call(div, lapply(which(secondColumn$GroupSecondColumn %in% g), function(i) {

                                                                         eval(parse(text = paste0(secondColumn$ItemType[i], "(inputId = secondColumn$ItemID[i], label = ifelse(secondColumn$helpText[i] %in% '', secondColumn$Label[i], paste0(secondColumn$Label[i], ' (', secondColumn$helpText[i], ')')),", secondColumn$Argument[i], "='", secondColumn$Default[i], "'", ifelse(secondColumn$Options[i] != FALSE, paste0(", options = ", secondColumn$Options[i]), ""), ifelse(secondColumn$Multiple[i] %in% TRUE, paste0(", multiple = TRUE, selected = '", secondColumn$Default[i], "')"), ")"))))

                                                                       })),
                                                                       label = g,
                                                                       icon = icon("sliders", verify_fa = FALSE),
                                                                       size = "lg",
                                                                       circle = FALSE,
                                                                       tooltip = tooltipOptions(title = "Click to see inputs !")


                           )
                           }))
                       ,
                       div(actionBttn(
                         inputId = "inactive11",
                         label = "3",
                         style = "pill",
                         color = "warning"),
                         strong("  Finalize"),
                     box(title = "Save your profile",
                         width = NULL,
                         status = "primary",
                         solidHeader = TRUE,
                         downloadButton(outputId = "dbProfile", label = "Save profile")),

                       actionBttn("LaunchFormating", label = "Apply changes!", style = "material-flat", color = "success") #style = "color: #fff; background-color: #009e60; border-color: #317256")
                     ),
                     hidden( actionBttn(
                       inputId = "GoToCodes",
                       label = "Next",
                       style = "material-flat",
                       color = "success"
                     )))),

                     box(width = 12,

                       # column(width = 12,
                              dropdownButton( title = h1("Our standard units"), icon = icon("info-circle"), size  ="sm", width = "500px",
                                              datatable(x[!x$Unit %in% c("", "-"), c("ItemID", "Unit")],
                                                        rownames = F,
                                                        width = 300)
                                            ),
                              h4("View of your formatted table:"),
                              DT::DTOutput(outputId = "FormatedTable"),
                              h4("summary of your formatted table:"),
                              verbatimTextOutput("FormatedTableSummary")
                       )
              # )


    )),
tabItem("Codes",

        includeMarkdown("www/Codes.md"),

        hidden(actionBttn(inputId = "UseProfileCodes" , label = "Use your INPUT profile")),
        actionBttn(
          inputId = "GoToCorrect",
          label = "Go To Correct",
          style = "material-flat",
          color = "success"
        ),
        hidden(downloadButton(outputId = "dbProfile1", label = "Save profile again")),

        # uiOutput("uiCodes"),
        br(),
        box(width = NULL,
            DT::DTOutput("CodeTable", height =  "600px"))
        # tags$hr(),
        # h2("Edited table:"),
        # tableOutput("NewCodeTable")

        # tableOutput("NewCodeTable")
        ),

    tabItem(tabName = "Correct",

            div(includeMarkdown("www/Corrections.md"),
                actionBttn(
                  inputId = "SkipCorrections",
                  label = "Skip Corrections",
                  style = "material-flat",
                  color = "warning"
                )
            ),
            fluidRow(
            hidden(actionBttn(
              inputId = "ApplyCorrections",
              label = "Apply Corrections",
              style = "material-flat",
              color = "success"
            )),
            hidden(actionBttn(
              inputId = "GoToOutput",
              label = "Go To Output format",
              style = "material-flat",
              color = "success"
            ))
            ),
            lapply(unique(xCorr$Function), function(f) {
              box(
                title = f,
                radioButtons(inputId = f, label = paste("Apply", f, "?"), choices = list("Yes" = "Yes", "No" = "No"), selected = "No"),
                hidden(div(id = paste0(f, "Yes"),

                lapply(which(xCorr$Function %in% f), function(i) {
                  # eval(parse(text = paste0(xCorr$ItemType[i], "(inputId = xCorr$ItemID[i], label = div(HTML(xCorr$Label[i])),", xCorr$Argument[i], " = eval(parse(text = '", xCorr$Default[i], "'))", ifelse(xCorr$Argument2[i] != FALSE, paste0(", ", xCorr$Argument2[i], " = eval(parse(text = '",xCorr$Default[i], "'))"), ""), ifelse(xCorr$Options[i] != FALSE, paste0(", options = ", xCorr$Options[i]), ""), ifelse(xCorr$Multiple[i] %in% TRUE, ", multiple = TRUE)", ")"))))
                  eval(parse(text = paste0(xCorr$ItemType[i], "(inputId = xCorr$ItemID[i], label = div(HTML(xCorr$Label[i])),", xCorr$Argument[i], ifelse(grepl("input", xCorr$Default[i]), " = 'pending'", paste0(" = eval(parse(text = '", xCorr$Default[i], "'))")), ifelse(xCorr$Argument2[i] != FALSE, paste0(", ", xCorr$Argument2[i], ifelse(grepl("input", xCorr$Default[i]), " = 'pending'", paste0(" = eval(parse(text = '", xCorr$Default[i], "'))"))), ""), ifelse(xCorr$Options[i] != FALSE, paste0(", options = ", xCorr$Options[i]), ""), ifelse(xCorr$Multiple[i] %in% TRUE, ", multiple = TRUE)", ")"))))
                })
              )
              ))
            }),

            fluidRow(

              column(width = 12,
                     h4("Vizualize the corrections:"),
                     p("Figures may take a while to render..."),
                     tabsetPanel(id = "CorrectionPlots"),

                     # withSpinner(uiOutput(outputId = "CorrectionPlots"), color="#0dc5c1", id = "spinner"),
                     h4("View of your corrected table:"),
                     withSpinner(DT::DTOutput(outputId = "CorrectedTable"), color="#0dc5c1", id = "spinner"),
                     h4("summary of your corrected table:"),
                     withSpinner(verbatimTextOutput("CorrectedTableSummary"), color="#0dc5c1", id = "spinner")
              ))
            ),

    tabItem(tabName = "OutputFormat",

            fluidRow(box(width = 12,
                         # radioButtons(inputId = "predefinedProfileOutput",
                         #              label = div("Use a predifined output profile"),
                         #              choices = list("No thanks! I'll upload a profile I have handy." = "No",
                         #                             "This App's standard" = "App",
                         #                             # "ATDN: The Amazon Tree Diversity Network" = "ATDN",
                         #                             "ForestGEO: The Smithsonian Forest Global Earth Observatory" = "ForestGeo"#,
                         #                             # "RBA: Red de Bosques Andinos" = "RBA"
                         #              ),
                         #              selected = "No"),
                         #
                         # # load a profile it one already exists
                         # div(id = "profileOutputfileInput",
                         #   fileInput(inputId = "profileOutput", div("Or, load a profile you have on your machine", br(), em("(if you or a colleague already used this app and saved a profile (.rds))")), accept = ".rds")),
                         # span(textOutput("RDSOutputWarning"), style="color:red"),
                         br(),
                        actionBttn(
                           inputId = "UseProfileOutput",
                           label = "Apply OUTPUT Profile",
                           style = "pill",
                           color = "success"),
                        hidden(actionBttn(
                           inputId = "DontUseProfileOutput",
                           label = "Don't use OUTPUT profile",
                           style = "pill",
                           color = "warning")),
                         hidden(actionBttn(
                           inputId = "GoToDownload",
                           label = "Go To Download",
                           style = "material-flat",
                           color = "success"
                         ))

            )
            ),
            fluidRow(hidden(div(id = "CodeTranslationsDiv",
                                box(width = 12,
                                    includeMarkdown("www/Code_translation.md"),

                                    fileInput("UserCodeTranslationTable", "If you have already been throught this and have a .csv file of your code translation table, you can upload it here to fill the table automatically."),
                                    hidden(actionBttn("updateCT", label = "Update")),


                                    uiOutput("uiCodeTranslations"))))),
            fluidRow(

              column(width = 12,
                     h4("View of your final table:"),
                     withSpinner(DT::DTOutput(outputId = "DataOutput"),color="#0dc5c1", id = "spinner"),
                     h4("summary of your final table:"),
                     withSpinner(verbatimTextOutput("DataOutputSummary"),color="#0dc5c1", id = "spinner")
              ))
    ), # end of "OutputFormat" panel
tabItem(tabName = "Save",
        fluidRow(
          column(width = 5,
                 box(title = "Save all outputs as zipfile",
                     width  = NULL,
                     status = "primary",
                     solidHeader = T,
                     downloadButton(outputId = "dbZIP", label = "Save all"))
                 )
        ),
        includeMarkdown("www/Download.md")), # end of "save" panel
    tabItem(tabName = "Help",
            tabsetPanel(
              tabPanel(title = "General",
                       imageOutput("AppGeneralWorkflow")
                     ),

              tabPanel_helper("Upload"),
              tabPanel_helper("Stack"),
              tabPanel_helper("Merge"),
              tabPanel_helper("Tidy"),
              tabPanel_helper("Headers"),
              tabPanel_helper("Codes"),
              tabPanel_helper("Corrections"),
              tabPanel(title = "Output",
                       tabsetPanel(
                         tabPanel_helper("Selecting_an_output_profile"),
                         tabPanel_helper("Code_translation")
                       )),
              tabPanel_helper("Download")
            )

    ) # end of "Help" panel
  )
)


ui <- dashboardPage(header, sidebar, body, skin = "black")





