
# Fichier pour gérer les interactions de l'application Shiny


# increase size limit to 191MB
options(shiny.maxRequestSize=1910*1024^2)

# my function to change first letter in uppercase (e.g for updatePickerInput)
firstUpper <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

# my function to repeat headers at bottom
FooterWithHeader <- function(x) {
  paste0("<table>",
         myTableHeader(x), # see definition in global.R
         myTableFooter(x), # see definition in global.R
         "</table>"
  )
}


# create a couple functions that allows to edit the tree codes table in the Codes tab
selector <- function(id, CodeOptions){ # --- this is to edit CODES table
  CodeOptionSplit <- split(CodeOptions, CodeOptions$OptionGroup)

  options <- HTML(paste0(unlist(lapply(1:length(CodeOptionSplit), function(I) {

    opt <- tags$optgroup(label = names(CodeOptionSplit)[I],
                         lapply( 1:nrow(CodeOptionSplit[[I]]),
                                 function(i){
                                   value <- CodeOptionSplit[[I]]$Definition[i]
                                   title <- CodeOptionSplit[[I]]$Source[i]
                                   if(i == 1L & I == 1){
                                     tags$option(value = value, title=title, selected = "selected", value)
                                   }else{
                                     tags$option(value = value, title = title, value)
                                   }
                                 } ))
    as.character(opt)
  })), collapse = ""))
  as.character(tags$select(id = id, options))
}

js <- c( # --- this is to edit CODES table
  "function(settings) {",
  "  var table = this.api().table();",
  "  var $tbl = $(table.table().node());",
  "  var id = $tbl.closest('.datatables').attr('id');",
  "  var nrows = table.rows().count();",
  "  function selectize(i) {",
  "    var $slct = $('#slct' + i);",
  "    $slct.select2({",
  "      width: '100%',",
  "      tags: true,",
  "      closeOnSelect: true",
  "    });",
  "    $slct.on('change', function(e) {",
  "      var info = [{",
  "        row: i,",
  "        col: 4,",
  "        value: $slct.val()",
  "      }];",
  "      Shiny.setInputValue(id + '_cell_selection:DT.cellInfo', info);",
  "    });",
  "  }",
  "  for(var i = 1; i <= nrows; i++) {",
  "    selectize(i);",
  "  }",
  "}"
)


# start server code here
if(isDebugging()) {
  onStop(function() {cat("Session stopped\n") ;close_sink_and_quit()})
}

server <- function(input, output, session) { # server ####

  # automatically close SERVER when user closes the UI if run locally
  autoCloseApp()

  # open browser #

  if(isDebugging()) {
    observeEvent(input$browser,{
      browser()
    })
  }

  # upload tab ####

  # show as meany upload widgets as asked for
  output$uiUploadTables <- renderUI({

    lapply(1:input$nTable, function(i) {

      column(width = 6,
             # load button for main data file (csv format)
             box(#title = paste("Table", i),
               textInput(inputId = paste0("TableName", i),
                         # label = "Give an explicit UNIQUE and SHORT name to this table. No space, no special character, no accent.",
                         label = NULL,
                         value = paste0("Table", i)
               ),
               width = NULL,
               fileInput(inputId = paste0("file", i), "Choose CSV File (max 100MB)", accept = ".csv"),
               # does the dataframe have a header?
               dropdownButton( icon = icon("cog"), size  ="sm",
                               checkboxInput( paste0("header", i), "Header", TRUE),
                               # choose separator
                               selectInput(inputId = paste0("cbSeparator", i),
                                           label = "Separator",
                                           choices = c("auto", ",", "\t",  "|", ";", ":"), # pb with tab
                                           selected = "auto"
                               )),
               span(textOutput(outputId = paste0("CSVWarning", i)), style="color:red")
             )
      )


    })



  })

  # View tables that are uploaded
  output$uiViewTables <- renderUI({
    req(input$file1)
    do.call(tabsetPanel, c(id='t', type = "tabs", lapply(names(Data()), function(i) {
      tabPanel(
        title=i,
        DT::DTOutput(outputId = i)
      )
    })))


  })


  ## read file(s)

  # give a red text if not a csv file
  observe({
    lapply(1:input$nTable, function(i){
      file <- input[[paste0("file", i)]]
      req(file)
      ext <- tools::file_ext(file$datapath)
      if(ext != "csv") output[[paste0("CSVWarning", i)]] <-renderText( "This is not a csv file!!")
    })

  })

  # give a pop up error if a files is not a csv file
  observe({
    lapply(1:input$nTable, function(i){
      file <- input[[paste0("file", i)]]
      req(file)
      ext <- tools::file_ext(file$datapath)
      if(ext != "csv") sendSweetAlert(
        session = session,
        title = "Oups !",
        text = "The is not a CSV file!",
        type = "error"
      )
    })

  })

  # fill in Data, as a list, with one drawer per uploaded table, named as provided by user

  Data <- reactiveVal()
  observe({
    req(input$file1)
    Data(setNames(
      lapply(reactiveValuesToList(input)[paste0("TableName", 1:input$nTable)], function(n) {

        i = which(reactiveValuesToList(input)[paste0("TableName", 1:input$nTable)] %in% n)
        file <- input[[paste0("file", i)]]
        ext <- tools::file_ext(file$datapath)

        req(file)
        shiny::validate(need(ext == "csv", "Please upload a csv file"))
        x <- data.table::fread(file$datapath,
                               header = input[[paste0("header", i)]],
                               sep = input[[paste0("cbSeparator", i)]],
                               check.names = T)
        colnames(x) <- iconv(colnames(x), from = '', to = 'ASCII//TRANSLIT')
        return(x)
      }),
      reactiveValuesToList(input)[paste0("TableName", 1:input$nTable)])
    )
  })

  observe({
    req(Data()[[1]])

    cnames = lapply(Data(), colnames)

    lapply(names(Data()), function(i) output[[i]] <- DT::renderDT(Data()[[i]] , rownames = FALSE,
                                                              options = list(pageLength = 8, scrollX=TRUE,
                                                                             autoWidth = TRUE),
                                                              container = FooterWithHeader(Data()[[i]]),
                                                              selection = "none")
    )

    shinyjs::show("submitTables")
  })


  # move on to next tab
  observeEvent(input$submitTables, {

    if(is.null(input$MeasLevel)) sendSweetAlert(
      session = session,
      title = "Oups !",
      text = "You forgot to enter something in step 2...",
      type = "error"
    )

    if(!is.null(input$MeasLevel)) {

      if(input$nTable == 1 & length(Data()) == 1) {
        updateTabItems(session, "tabs", "Tidying")
        shinyjs::runjs("window.scrollTo(0, 0)")

      } else {
        updateCheckboxGroupButtons(session, "TablesToStack",
                                   choices = unname(reactiveValuesToList(input)[paste0("TableName", 1:input$nTable)]))

        updateTabItems(session, "tabs", "Stacking")
        shinyjs::runjs("window.scrollTo(0, 0)")

      }

    }

  })

  # stack tab ####
  StackedTables <- reactiveVal()

  observeEvent(input$Stack, {
    withCallingHandlers({
      StackedTables(do.call(rbind, Data()[input$TablesToStack]))
    },
    warning = function(warn){
      showNotification(paste0(warn, collapse = "; "), type = 'warning', duration = NULL)
    },
    error = function(err){
      shiny:::reactiveStop(showNotification(paste0(err, collapse = "; "), type = 'err', duration = NULL))
    })
  })

  observeEvent(input$Stack, {
    shinyjs::hide("SkipStack")

    output$StackedTables <- DT::renderDT(StackedTables(), rownames = FALSE,
                                     options = list(pageLength = 8, scrollX=TRUE,
                                                    autoWidth = TRUE),
                                     container = FooterWithHeader(StackedTables()),
                                     selection = "none")

    output$StackedTablesSummary <- renderPrint(summary(StackedTables()))


    if(all(names(Data()) %in% input$TablesToStack)) {

      shinyjs::show("SkipMerge")
      shinyjs::hide("GoToMerge")
    } else {

      shinyjs::show("GoToMerge")
      shinyjs::hide("SkipMerge")

    }

  })

  # move on to next tab
  observe({
    if(is.null(input$TablesToStack))   shinyjs::show("SkipStack")
    if(!is.null(input$TablesToStack))   shinyjs::hide("SkipStack")
  })

  observeEvent(input$GoToMerge | input$SkipStack, {


    updateTabItems(session, "tabs", "Merging")
    shinyjs::runjs("window.scrollTo(0, 0)")




    options_to_merge <- names(Data())
    column_options_list <- lapply(Data(), colnames)


    if(!is.null(input$TablesToStack)){

      options_to_merge <- c(names(Data())[!names(Data()) %in% input$TablesToStack], "StackedTables")
      column_options_list[names(Data()) %in% input$TablesToStack] <- NULL
      column_options_list <- c(column_options_list, StackedTables = list(colnames(StackedTables())))

    }

    updatePickerInput(session, "leftTable", choices = options_to_merge, selected =  "")
    updatePickerInput(session, "rightTable", choices = options_to_merge, selected =  "")

    observeEvent(input$selectLeft, {
      commonColumns <- intersect(column_options_list[[input$leftTable]], column_options_list[[input$rightTable]])

      updateVirtualSelect("leftKey", choices = column_options_list[[input$leftTable]], selected = ifelse(length(commonColumns)>0, commonColumns, ""))
    })

    observeEvent(input$selectRight, {
      commonColumns <- intersect(column_options_list[[input$leftTable]], column_options_list[[input$rightTable]])

      updateVirtualSelect( "rightKey", choices = column_options_list[[input$rightTable]], selected =  ifelse(length(commonColumns)>0, commonColumns, ""))
    })


    n_tables_after_stack(length(options_to_merge))

  }, ignoreInit = T)

  observeEvent(input$leftTable, {
    if(!input$leftTable %in% "") updateActionButton(session, "selectLeft", label = "click me!")
  })
  observeEvent(input$selectLeft, {
    shinyjs::show("leftKey")
    updateActionButton(session, "selectLeft", label = "")
  })

  observeEvent(input$rightTable, {
    if(!input$rightTable %in% "") updateActionButton(session, "selectRight", label = "click me!")
  })
  observeEvent(input$selectRight, {
    shinyjs::show("rightKey")
    updateActionButton(session, "selectRight", label = "")
  })

  observeEvent(input$leftTable2, {
    if(!input$leftTable2 %in% "") updateActionButton(session, "selectLeft2", label = "click me!")
  })
  observeEvent(input$selectLeft2, {
    shinyjs::show("leftKey2")
    updateActionButton(session, "selectLeft2", label = "")
  })


  observeEvent(input$rightTable2, {
    if(!input$rightTable2 %in% "") updateActionButton(session, "selectRight2", label = "click me!")
  })
  observeEvent(input$selectRight2, {
    shinyjs::show("rightKey2")
    updateActionButton(session, "selectRight2", label = "")
  })

  # merge tab ####
  n_tables_after_stack <- reactiveVal()

  observeEvent("addMerge", {


  }, ignoreInit = T)

  MergedTables <- reactiveVal()

  observeEvent(input$Merge2, {

    if(input$leftTable2 == "StackedTables") x <-  get(input$leftTable2)()
    if(input$leftTable2 == "MergedTables") x <-  get(input$leftTable2)()
    if(input$leftTable2 != "MergedTables" & input$leftTable2 != "StackedTables") x <- Data()[[input$leftTable2]]


    if(input$rightTable2 == "StackedTables") y <-  get(input$rightTable2)()
    if(input$rightTable2 == "MergedTables") y <-  get(input$rightTable2)()
    if(input$rightTable2 != "MergedTables" & input$rightTable2 != "StackedTables") y <- Data()[[input$rightTable2]]

    # MergedTables(merge(x, y, by.x=input$leftKey2, by.y=input$rightKey2, all.x=TRUE, suffixes = c("", ".y")))

    withCallingHandlers({
      MergedTables(merge(x, y, by.x=input$leftKey2, by.y=input$rightKey2, all.x=TRUE, all.y = FALSE, suffixes = c("", ".y")))
    },
    warning = function(warn){
      showNotification(paste0(warn, collapse = "; "), type = 'warning', duration = NULL)
    },
    error = function(err){
      shiny:::reactiveStop(showNotification(paste0(err, collapse = "; "), type = 'err', duration = NULL))
    })

    shinyjs::show("GoToTidy")
    shinyjs::hide("addMerge")



  }, ignoreInit = T)

  observeEvent(input$Merge, {

    if(input$leftTable == "StackedTables") x <-  get(input$leftTable)() else x <- Data()[[input$leftTable]]
    if(input$rightTable == "StackedTables") y <-  get(input$rightTable)() else y <- Data()[[input$rightTable]]

    # MergedTables(merge(x, y, by.x=input$leftKey, by.y=input$rightKey, all.x=TRUE, suffixes = c("", ".y")))

    withCallingHandlers({
      MergedTables(merge(x, y, by.x=input$leftKey, by.y=input$rightKey, all.x=TRUE, all.y = FALSE, suffixes = c("", ".y")))
    },
    warning = function(warn){
      showNotification(paste0(warn, collapse = "; "), type = 'warning', duration = NULL)
    },
    error = function(err){
      shiny:::reactiveStop(showNotification(paste0(err, collapse = "; "), type = 'err', duration = NULL))
    })

    if(n_tables_after_stack() > 2 ) {
      shinyjs::show("addMerge")
      shinyjs::show("Merge2Div")
      shinyjs::hide("GoToTidy")


      options_to_merge <- names(Data())
      column_options_list <- lapply(Data(), colnames)


      if(input$leftTable == "StackedTables" | input$rightTable == "StackedTables"){

        options_to_merge <- c(names(Data())[!names(Data()) %in% c(input$TablesToStack, input$leftTable, input$rightTable)], "MergedTables")
        column_options_list[names(Data()) %in% c(input$TablesToStack, input$leftTable, input$rightTable)] <- NULL
        column_options_list <- c(column_options_list, MergedTables = list(colnames(MergedTables())))

      } else {
        if(!is.null(input$TablesToStack)) {
          options_to_merge <- c(names(Data())[!names(Data()) %in% c(input$leftTable, input$rightTable)], "MergedTables", "StackedTables")
          column_options_list[names(Data()) %in% c(input$leftTable, input$rightTable)] <- NULL
          column_options_list <- c(column_options_list, MergedTables = list(colnames(MergedTables())), StackedTables = list(colnames(StackedTables())))
        } else {
          options_to_merge <- c(names(Data())[!names(Data()) %in% c(input$leftTable, input$rightTable)], "MergedTables")
          column_options_list[names(Data()) %in% c(input$leftTable, input$rightTable)] <- NULL
          column_options_list <- c(column_options_list, MergedTables = list(colnames(MergedTables())))
        }

      }

      updatePickerInput(session, "leftTable2", choices = options_to_merge, selected =  "")
      updatePickerInput(session, "rightTable2", choices = options_to_merge, selected =  "")

      observeEvent(input$selectLeft2, {
        commonColumns <- intersect( column_options_list[[input$leftTable2]],  column_options_list[[input$rightTable2]])

        updateVirtualSelect("leftKey2", choices = column_options_list[[input$leftTable2]], selected = ifelse(length(commonColumns)>0, commonColumns, ""))
      })

      observeEvent(input$selectRight2, {
        commonColumns <- intersect( column_options_list[[input$leftTable2]],  column_options_list[[input$rightTable2]])

        updateVirtualSelect( "rightKey2", choices = column_options_list[[input$rightTable2]], selected =  ifelse(length(commonColumns)>0, commonColumns, ""))
      })


      # n_tables_after_stack(length(options_to_merge))

    }

    if(n_tables_after_stack() == 2 ) {
      shinyjs::hide("addMerge")
      shinyjs::hide("Merge2Div")
      shinyjs::show("GoToTidy")
    }

    output$mergedTablesSummary <- renderPrint(summary(MergedTables()))

    output$mergedTables <- DT::renderDT(MergedTables(), rownames = FALSE,
                                    options = list(pageLength = 8, scrollX=TRUE,
                                                   autoWidth = TRUE),
                                    container = FooterWithHeader(MergedTables()),
                                    selection = "none")


    shinyjs::show("SelectColumns")

    updatePickerInput(session, "SelectedMergedColumns", choices =colnames(MergedTables()), selected = colnames(MergedTables())[!grepl("\\.y$", colnames(MergedTables()))])


  }, ignoreInit = T)

  # move on to next tab

  observeEvent(input$GoToTidy | input$SkipMerge, {
    updateTabItems(session, "tabs", "Tidying")
    shinyjs::runjs("window.scrollTo(0, 0)")
  }, ignoreInit = T)


  # tidy tab ####

  OneTable <- reactiveVal()

  observeEvent(input$submitTables | input$GoToTidy | input$SkipMerge,

  # OneTable <- eventReactive(input$submitTables | input$GoToTidy | input$SkipMerge,
                            {

    if(input$nTable == 1 & length(Data()) == 1) {
      OneTable(Data()[[1]])
    } else {
      if(length(MergedTables()) > 0 ) OneTable(MergedTables()) else  OneTable(StackedTables())
    }


  }, ignoreInit = TRUE)



  observe({
    req(length(OneTable()) > 0)
    groupNames <- split(names(OneTable()), cutree(hclust(stringdistmatrix(names(OneTable()))), h = 2))
    groupNames <- groupNames[sapply(groupNames, length) > 1]
    names(groupNames) <- sapply(groupNames, function(x) paste(Reduce(intersect, strsplit(x,"")), collapse=""))
    groupNames[(length(groupNames)+1):(length(groupNames)+(ncol(OneTable())/2))] <- ""

    output$uiMelt <- renderUI({
      lapply(c(1:length(groupNames)), function(i)
      {
        box(width = 12,
                   pickerInput(
                     inputId = paste0("Variablecolumns", i),
                     label = paste("List variables that belong to the same group or measurement at different [", input$VariableName, "]. For example, 'dbh_in_cm_1' and 'dbh_in_cm_2'."),
                     choices = colnames(OneTable()),
                     selected = groupNames[[i]],
                     multiple = T
                   ),
            column(11, textInput(paste0("ValueName", i), "Under which name do you want to group these variables? For example, 'dbh_in_cm' (Give a column name without space)", value = names(groupNames)[i])
            ),

               awesomeCheckbox(
                 inputId = paste0("TickedMelt", i),
                 label = "Tick me if you want to apply this grouping",
                 value = FALSE,
                 status = "info"
               ))
      })

    })
  })

  observeEvent(input$ClearValueName,{
    updateRadioButtons(session,"VariableName",selected = character(0))
  })

  observe({
    if(!is.null(input$VariableName)) {
      shinyjs::hide("SkipTidy")
      shinyjs::show("Tidy")

    }
    if(is.null(input$VariableName)) {
      shinyjs::show("SkipTidy")
      shinyjs::hide("Tidy")

    }


  })

  TidyTable <- reactiveVal()

  observeEvent(input$Tidy, {

    Variablecolumns <- reactiveValuesToList(input)[sort(grep("Variablecolumns\\d{1,}$", names(input), value = T))]

    ValueName <- unlist(reactiveValuesToList(input)[sort(grep("ValueName\\d{1,}$", names(input), value = T))])

    TickedMelt <- unlist(reactiveValuesToList(input)[sort(grep("TickedMelt\\d{1,}$", names(input), value = T))])

    names(Variablecolumns) <- ValueName


    withCallingHandlers({
      TidyTable(melt(OneTable(), measure.vars	= Variablecolumns[TickedMelt], variable.name =  input$VariableName, variable.factor = FALSE)) #,  value.name = names(Variablecolumns[TickedMelt])
    },
    warning = function(warn){
      showNotification(paste0(warn, collapse = "; "), type = 'warning', duration = NULL)
    },
    error = function(err){
      shiny:::reactiveStop(showNotification(paste0(err, collapse = "; "), type = 'err', duration = NULL))
    })
  }, ignoreInit = TRUE)

  observeEvent(input$Tidy, {
    shinyjs::show("GoToHeaders")

    output$TidyTable <- DT::renderDT(TidyTable(), rownames = FALSE,
                                 options = list(pageLength = 8, scrollX=TRUE,
                                                autoWidth = TRUE),
                                 container = FooterWithHeader(TidyTable()),
                                 selection = "none")

    output$TidyTableSummary <- renderPrint(summary(TidyTable()))
  })

  output$VizForHeaders <- DT::renderDT(TidyTable()[, input$SelectedColumns, with = F], rownames = FALSE,
                                       options = list(pageLength = 8, scrollX=TRUE,
                                                      autoWidth = TRUE),
                                       container = FooterWithHeader(TidyTable()[, input$SelectedColumns, with = F]),
                                       selection = "none")

  # move on to next tab
  observeEvent(input$SkipTidy, {
    TidyTable(OneTable())
  }, ignoreInit = TRUE)


  observe({
    updatePickerInput(session, "SelectedColumns", choices = colnames(TidyTable()), selected = colnames(TidyTable()))
  })

  observeEvent(input$GoToHeaders | input$SkipTidy, {
    updateTabItems(session, "tabs", "Headers")
    shinyjs::runjs("window.scrollTo(0, 0)")

  }, ignoreInit = T)


  # Headers tab ####


  # create options to choose from

  # ColumnOptions <- eventReactive(TidyTable(), { c("none", colnames(TidyTable())) })

  ColumnOptions <- eventReactive({
    TidyTable()
    input$SelectedColumns
    }, { list("Focused columns" = as.list(c("none",  input$SelectedColumns)), "others"  = as.list(setdiff(colnames(TidyTable()), input$SelectedColumns))) })

  UnitOptions <- eventReactive(TidyTable(),
                               {c("none", "mm", "cm", "dm", "m")
                               })

  AreaUnitOptions <- eventReactive(input$PlotArea,
                                   {c("none", "mm2","cm2", "m2", "ha", "km2")
                                   })

  DensityUnitOptions <- eventReactive(input$PlotArea,
                                      {c("none", "individual/cm2", "individual/m2", "individual/ha", "individual/km2")
                                      })

  AreaByAreaUnitOptions <- reactiveVal(c("none",
                                         "mm2/m2", "cm2/m2", "m2/m2",
                                         "mm2/ha", "cm2/ha", "m2/ha",
                                         "mm2/km2", "cm2/km2", "m2/km2"))

  VolumeByAreaUnitOptions<- reactiveVal(c("none",
                                          "mm3/m2", "cm3/m2", "m3/m2",
                                          "mm3/ha", "cm3/ha", "m3/ha",
                                          "mm3/km2", "cm3/km2", "m3/km2"))

  MassByAreaUnitOptions <- reactiveVal(c("none",
                                         "g/m2", "kg/m2", "Mg/m2",
                                         "g/ha", "kg/ha", "Mg/ha",
                                         "gC/m2", "kgC/m2", "MgC/m2",
                                         "gC/ha", "kgC/ha", "MgC/ha"))

  LifeFormOptions <- reactiveVal(c("trees",
                                   "palms",
                                   "lianas and/or vines",
                                   "bamboos",
                                   "seedlings",
                                   "forbs and/or herbs",
                                   "annuals",
                                   "graminoids",
                                   "geophytes",
                                   "hydrophytes",
                                   "parasites",
                                   "epiphytes",
                                   "hemiepiphytes",
                                   "lithophites",
                                   "succulents",
                                   "ferns",
                                   "cycads",
                                   "fungi",
                                   "mosses",
                                   "lichens"
                                   ))


  TreeCodesSepOptions <- reactiveVal(c("Punctuation character (,;-/...)" = "[[:punct:]]",
                                       "No character (codes are concatenated)" = ""))


  LifeStatusOptions <- eventReactive(input$LifeStatus, {
    sort(unique(TidyTable()[[input$LifeStatus]]))})

  DeadStatusOptions <- eventReactive(input$DeadStatus, {
    sort(unique(TidyTable()[[input$DeadStatus]]))})

  CommercialOptions <- eventReactive(input$CommercialSp, {
    sort(unique(TidyTable()[[input$CommercialSp]]))
  })

  OtherNumericOptions <- reactiveVal(-999)
  OtherCharacterOptions <- reactiveVal("")

  LogicalOptions <- reactive(c(TRUE, FALSE))

  # update each element base on the options created above
  # observe({

    lapply(1:nrow(x1), function(i) {

      eval(parse(text = paste(paste0("observe({update", firstUpper(x1$ItemType[i])), "(session, inputId = x1$ItemID[i],", x1$Argument[i],"= get(x1$argValue[i])()) })")))

    })

  # })

    itemsToUpdate <- reactiveVal()
    itemsToResetAndHide <- reactiveVal()

    observe({
      itemsToUpdate(c(x2$ItemID[reactiveValuesToList(input)[x2$if_X1_is_none] %in% "none"],

                      x3$ItemID[reactiveValuesToList(input)[x3$if_X1_is_none] %in% "none" &
                                  !reactiveValuesToList(input)[x3$if_X2_isnot_none] %in% "none"],

                      x4$ItemID[!reactiveValuesToList(input)[x4$if_X2_isnot_none] %in% "none"],

                      x5$ItemID[reactiveValuesToList(input)[x5$if_X1_is_none] %in% "none" &
                                  reactiveValuesToList(input)[x5$if_X2_is_none] %in% "none"],


                      x6$ItemID[reactiveValuesToList(input)[x6$if_X2_is_none] %in% "none" &
                                  !reactiveValuesToList(input)[x6$if_X2_isnot_none] %in% "none"]))
    })

    observe({

      itemsToResetAndHide(secondColumn$ItemID[!secondColumn$ItemID%in% itemsToUpdate()])

      })



    observe({
      for(id in itemsToUpdate()) {
        i <- which(secondColumn$ItemID %in% id)

        # first update the options

        eval(parse(text = paste(paste0("update", firstUpper(secondColumn$ItemType[i])), "(session, inputId = secondColumn$ItemID[i],", secondColumn$Argument[i], "= get(secondColumn$argValue[i])())")))

        # then update value if user provided profile

        if(!is.null(UserProfile()[[id]])) {
            eval(parse(text = paste(paste0("update", firstUpper(secondColumn$ItemType[i])), "(session, inputId = secondColumn$ItemID[i],",  ifelse(secondColumn$Argument[i] %in% "choices", "selected", "value"), "= UserProfile()[[id]])")))
        }


        # and make sure we can see the item
        shinyjs::show(id)
      }
    })

    observe({

      for(id in itemsToResetAndHide()) {
        i <- which(secondColumn$ItemID %in% id)

        eval(parse(text = paste0(paste0("update", firstUpper(secondColumn$ItemType[i])), "(session, inputId = secondColumn$ItemID[i],", secondColumn$Argument[i], "='",  secondColumn$default[i], "')")))

        shinyjs::hide(id)
      }})



    # observe({
    #
    #   for(g in unique(x2$GroupSecondColumn)) {
    #     idx = which(x2$GroupSecondColumn %in% g)
    #
    #
    #     lapply(idx, function(i) {
    #       if(input[[x2$if_X1_is_none[i]]] %in% "none") {
    #         eval(parse(text = paste(paste0("update", firstUpper(x2$ItemType[i])), "(session, inputId = x2$ItemID[i],", x2$Argument[i], "= get(x2$argValue[i])())")))
    #
    #         shinyjs::show( x2$ItemID[i])
    #
    #       } else {
    #         eval(parse(text = paste0(paste0("update", firstUpper(x2$ItemType[i])), "(session, inputId = x2$ItemID[i],", x2$Argument[i], "='",  x2$default[i], "')")))
    #
    #         shinyjs::hide( x2$ItemID[i])
    #       }
    #
    #     })
    #
    #
    #
    #   }
    #
    # })
    #
    # observe({
    #   for(g in unique(x3$GroupSecondColumn)) {
    #     idx = which(x3$GroupSecondColumn %in% g)
    #
    #     lapply(idx, function(i) {
    #       if(input[[x3$if_X1_is_none[i]]] %in% "none" & !input[[x3$if_X2_isnot_none[i]]] %in% "none" ) {
    #
    #         eval(parse(text = paste(paste0("update", firstUpper(x3$ItemType[i])), "(session, inputId = x3$ItemID[i],", x3$Argument[i], "= get(x3$argValue[i])())")))
    #
    #         shinyjs::show( x3$ItemID[i])
    #
    #
    #       } else {
    #         eval(parse(text = paste0(paste0("update", firstUpper(x3$ItemType[i])), "(session, inputId = x3$ItemID[i],", x3$Argument[i], "='",  x3$default[i], "')")))
    #
    #         shinyjs::hide( x3$ItemID[i])
    #       }
    #
    #     })
    #
    #   }
    #
    # })
    # observe({
    #   for(g in unique(x4$GroupSecondColumn)) {
    #     idx = which(x4$GroupSecondColumn %in% g)
    #
    #
    #     lapply(idx, function(i) {
    #       if(!is.null(input[[x4$if_X2_isnot_none[i]]]) && !input[[x4$if_X2_isnot_none[i]]] %in% "none") {
    #         eval(parse(text = paste0(paste0("update", firstUpper(x4$ItemType[i])), "(session,inputId = x4$ItemID[i],", x4$Argument[i], "= get(x4$argValue[i])())")))
    #
    #         shinyjs::show( x4$ItemID[i])
    #
    #       } else {
    #         eval(parse(text = paste0(paste0("update", firstUpper(x4$ItemType[i])), "(session,inputId = x4$ItemID[i],", x4$Argument[i], "='", x4$Default[i], "')")))
    #
    #         shinyjs::hide( x4$ItemID[i])
    #
    #       }
    #
    #     })
    #
    #
    #   }
    # })
    # observe({
    #   for(g in unique(x5$GroupSecondColumn)) {
    #     idx = which(x5$GroupSecondColumn %in% g)
    #
    #
    #     lapply(idx, function(i) {
    #       if(input[[x5$if_X1_is_none[i]]] %in% "none" &  input[[x5$if_X2_is_none[i]]] %in% "none") {
    #         eval(parse(text = paste0(paste0("update", firstUpper(x5$ItemType[i])), "(session,inputId = x5$ItemID[i],", x5$Argument[i], "= get(x5$argValue[i])())")))
    #
    #         shinyjs::show( x5$ItemID[i])
    #
    #       } else {
    #
    #         eval(parse(text = paste0(paste0("update", firstUpper(x5$ItemType[i])), "(session,inputId = x5$ItemID[i],", x5$Argument[i], "='", x5$Default[i], "')")))
    #
    #         shinyjs::hide( x5$ItemID[i])
    #
    #       }
    #
    #     })
    #
    #   }
    # })
    #
    # observe({
    #   if(nrow(x6) > 0 ) {
    #     for(g in unique(x6$GroupSecondColumn)) {
    #       idx = which(x6$GroupSecondColumn %in% g)
    #
    #
    #       lapply(idx, function(i) {
    #         if(input[[x6$if_X2_is_none[i]]] %in% "none" & !input[[x6$if_X2_isnot_none[i]]] %in% "none") {
    #           eval(parse(text = paste0(paste0("update", firstUpper(x6$ItemType[i])), "(session,inputId = x6$ItemID[i],", x6$Argument[i], "= get(x6$argValue[i])())")))
    #
    #           shinyjs::show( x6$ItemID[i])
    #
    #         } else {
    #
    #           eval(parse(text = paste0(paste0("update", firstUpper(x6$ItemType[i])), "(session,inputId = x6$ItemID[i],", x6$Argument[i], "='", x6$Default[i], "')")))
    #
    #           shinyjs::hide( x6$ItemID[i])
    #
    #         }
    #
    #       })
    #
    #     }
    #   }
    # })


  ## handle upload and use of profile (updating what is selected)

  # gimme_value <- reactiveVal(0)
  #
  observe( {
    if(input$predefinedProfile != "No" )
      shinyjs::show("UseProfile")
  #   updateActionButton(session, inputId = "UseProfile", label = "Click Twice here to use Profile")
  #   gimme_value(0)
  })
  #
  observeEvent(input$profile, {
    shinyjs::show("UseProfile")
  #   updateActionButton(session, inputId = "UseProfile", label = "Click Twice here to use Profile")
  #   gimme_value(0)
  })

  UserProfile <- reactiveVal()

  observe({

    req(input$profile$datapath)
    file <- input$profile$datapath
    ext <- tools::file_ext(file)


    if(ext != "rds") sendSweetAlert(
      session = session,
      title = "Oups !",
      text = "The is not a .rds file!",
      type = "error")

    if(ext != "rds") output$RDSWarning <- renderText("This is not a .rds file! Please upload a .rds file.")
    if(ext == "rds") output$RDSWarning <- renderText("")


  })

  observeEvent({
    input$UseProfile
    input$SelectedColumns
    }, {

    if(input$predefinedProfile == "No") {
      file <- input$profile$datapath
      ext <- tools::file_ext(file)
    } else {
      file <- paste0("data/", input$predefinedProfile, "Profile.rds")
      ext <- tools::file_ext(file)
    }



    profile <- tryCatch({ readRDS(file)},
                        error = function(err){
                          shiny:::reactiveStop(showNotification("This is not a .rds file! Please upload a .rds file.", type = 'err', duration = NULL))
                        })

    if(!is.null(profile$AllCodes)) {
      if(!profile$AllCodes[1,1] %in% "You have not selected columns for codes" & !all(profile$AllCode$Definition == ""))
        shinyjs::show("UseProfileCodes")
    }


    if(!is.null(profile$SelectedColumns)) updatePickerInput(session, "SelectedColumns", selected = profile$SelectedColumns)

    ValidItemID <- names(profile)[sapply(profile, function(p) all(p %in% c(names(TidyTable()), "none"))) | grepl("Man", names(profile))] # this is to avoid the app from crashing if we have new items in x, that do not exist in data

    InValidItemID <- setdiff(names(profile), ValidItemID)
    InValidItemID <- InValidItemID[InValidItemID %in% x$ItemID]

    if(length(InValidItemID) > 0 ) { #& gimme_value() == 1) {

      if(!(length(InValidItemID) < 50 & (input$predefinedProfile %in% "App"|profile$ProfileName %in% "App"))) {
        showNotification(paste("The profile you selected does not seem to correspond to your data. The items that do not match your data are:", paste0(InValidItemID, " (in ", x$Group[match(InValidItemID, x$ItemID)], ")",  collapse = ",\n"), ".\n Please, fill out those items by hand (or make sure you picked the right profile). Also, please double check that the info in the second column is filled out properly."), type = 'err', duration = NULL)}
    }
    #
    # for(i in which(x$ItemID %in% names(profile) & reactiveValuesToList(input)[x$ItemID] %in% names(TidyTable()))) {

    for(i in which(x$ItemID %in% ValidItemID)) {    # used to be for(i in which(x$ItemID %in% names(profile)))
      eval(parse(text = paste0(paste0("update", firstUpper(x$ItemType[i])), "(session,inputId = x$ItemID[i],", ifelse(x$Argument[i] %in% "choices", "selected", "value"), "= profile[[x$ItemID[i]]])")))

      # eval(parse(text = paste0("updateTextInput(session, '", x$ItemID[i], "', value = profile$", x$ItemID[i], ")")))
      # updateTextInput(session, "Site", value = profile$Site)
    }

    # if(gimme_value() == 1) {
    #   updateActionButton(session, inputId = "UseProfile", label = "Thanks!")
    # }
    #
    # if(gimme_value() == 0) {
    #   updateActionButton(session, inputId = "UseProfile", label = "click one more time!")
    #   gimme_value(gimme_value() + 1)
    # }

    UserProfile(profile)

    shinyjs::runjs('document.getElementById("LaunchFormating").scrollIntoView({ behavior: "instant", block: "end", inline: "end" })')

  })


  observe({
    req(length(UserProfile()) > 0 & length(itemsToResetAndHide()) <27 ) # itemsToResetAndHide starts at 27
    MissingItemIDProfile <- setdiff(x$ItemID, names(UserProfile()))
    MissingItemIDProfile <- setdiff(MissingItemIDProfile, itemsToResetAndHide())

    if(length(MissingItemIDProfile) > 0 ){ #& gimme_value() == 1) {
      sendSweetAlert(
        session = session,
        title = "Sorry !",
        text =  tags$span(
          "We've updated the app since you last saved your profile...",
          tags$br(),
          "The profile you selected is missing the following latest items:",
          tags$br(),
          tags$br(),
          tags$ul(

            lapply(paste0(MissingItemIDProfile, " (in ", x$Group[match(MissingItemIDProfile, x$ItemID)], ")"), tags$li)
          ),
          tags$br(),
          "Please, fill out those items by hand and double check that the info in the second column is filled out properly. Then, save your new profile."),


        html = T,
        type = "error")
      # showNotification(paste("The profile you selected is missing the following latest items:\n", paste0(MissingItemIDProfile, " (in ", x$Group[match(MissingItemIDProfile, x$ItemID)], ")",  collapse = ",\n"), ".\n Please, fill out those items by hand and double check that the info in the second column is filled out properly. Then, save your new profile."), type = 'err', duration = NULL)
    }
  })

  observe({
    if(!input$Date %in% "none") {
           shinyjs::show("AttentionDates")

    useful_dates_idx <- sapply(strsplit(as.character(TidyTable()[[input$Date]]), "\\W"), function(x) any(as.numeric(x) > 12 & as.numeric(x) < 32))

    if(sum(useful_dates_idx, na.rm = T) > 0) sampleDates <- sample(TidyTable()[[input$Date]][which(useful_dates_idx)], 6) else sampleDates <- sample(TidyTable()[[input$Date]], 6)

    output$sampleDates <- renderText(as.character(sampleDates))
    }

  })

  # format data
  DataFormated <- reactiveVal()


  observeEvent(input$LaunchFormating, {
  # DataFormated <- eventReactive(input$LaunchFormating | input$UpdateTable, {

    withCallingHandlers({
      DataFormated(RequiredFormat(Data = TidyTable(), isolate(reactiveValuesToList(input)), x))
    },
    warning = function(warn){
      showNotification(paste(gsub("simpleWarning in RequiredFormat\\(Data = TidyTable\\(\\), isolate\\(reactiveValuesToList\\(input\\)\\), :", "", warn), collapse = ". "), type = 'warning', duration = NULL)
    },
    error = function(err){
      shiny:::reactiveStop(showNotification(gsub("in RequiredFormat\\(Data = TidyTable\\(\\), isolate\\(reactiveValuesToList\\(input\\)\\),", "", err), type = 'err', duration = NULL))
    })

  }, ignoreInit = T)

  FormatedColumnOptions <- reactiveVal()
  observe({
    input$LaunchFormating
    FormatedColumnOptions(names(DataFormated()))
    })


  FormatedScientificNameOptions <- reactiveVal()
  observe({
    input$LaunchFormating
    if("ScientificName_DataHarmonizationCor" %in% names(DataFormated())) FormatedScientificNameOptions(sort(unique(DataFormated()$ScientificName_DataHarmonizationCor))) else
    FormatedScientificNameOptions(sort(unique(DataFormated()$ScientificName)))
    })


  # Visualize output
  output$FormatedTable <- DT::renderDT(DataFormated()[,lapply(.SD, function(x) {if(all(is.na(x))) {NULL} else {x}} )], rownames = FALSE,
                                   options = list(pageLength = 8, scrollX=TRUE),
                                   container = FooterWithHeader(DataFormated()[,lapply(.SD, function(x) {if(all(is.na(x))) {NULL} else {x}} )]),
                                   selection = "none")

  output$FormatedTableSummary <- renderPrint(summary(DataFormated()[,lapply(.SD, function(x) {if(all(is.na(x))) {NULL} else {x}} )]))


  # update stuff in the Corrections tab, based on the formated data
  observeEvent(input$LaunchFormating , {

    lapply(which(xCorr$Argument %in% "choices"), function(i) {

      eval(parse(text = paste0(paste0("update", firstUpper(xCorr$ItemType[i])), "(session,inputId = xCorr$ItemID[i],", xCorr$Argument[i], ifelse(xCorr$ReactiveArgValue[i], "= get(xCorr$argValue[i])()", "= eval(str2lang(xCorr$argValue[i]))"), ifelse(xCorr$Argument2[i] != FALSE, paste0(", ", xCorr$Argument2[i], ifelse(xCorr$Default[i] %in% c("TRUE", "FALSE"), paste0(" = '", xCorr$Default[i], "'"), paste0(" = eval(parse(text = '",xCorr$Default[i], "'))")), ")")))))
    })

  })

  # move on to next tab
  observeEvent(input$LaunchFormating , {
    shinyjs::show("GoToCodes")
  }, ignoreInit = T)

  observeEvent(input$GoToCodes, {
    if(length(input$TreeCodes) > 0) {
      updateTabItems(session, "tabs", "Codes")
      shinyjs::runjs("window.scrollTo(0, 0)")

    } else {
      if(input$MeasLevel %in% c("Tree", "Stem")) {
        updateTabItems(session, "tabs", "Correct")
        shinyjs::runjs("window.scrollTo(0, 0)")

      } else {
        updateTabItems(session, "tabs", "OutputFormat")
        shinyjs::runjs("window.scrollTo(0, 0)")

        DataDone(DataFormated())
      }
    }
  }, ignoreInit = TRUE)



  # codes tab ####


  AllCodes <- reactiveVal(data.frame(Column = "You have not selected columns for codes",
                                     Value = "You have not selected columns for codes",
                                     Definition = "You have not selected columns for codes"))



  observe({
    req(input$TreeCodes)
    AllCodes(cbind(rbindlist(apply(TidyTable()[,input$TreeCodes, with = F], 2, function(x) data.frame(Value = unique(unlist(strsplit(as.character(x), input$TreeCodesSepMan))))), idcol = "Column" ), Definition = ""))

  })

  observeEvent(input$UseProfileCodes, {
    dat <- AllCodes()
    m <- match(paste(dat$Column, dat$Value), paste(UserProfile()$AllCodes$Column, UserProfile()$AllCodes$Value))



    withCallingHandlers({
      if(any(is.na(m))) warning(paste(paste("WARNING: The following codes are not in your profile, you will need to fill them manually in the table:", paste(paste(dat$Value[is.na(m)], "in column", dat$Column[is.na(m)]), collapse = "\n")), collapse = ". "))    },
    warning = function(warn){
      showNotification(paste(gsub("Warning in withCallingHandlers\\(\\{ \\:", "", warn), collapse = ". "), type = 'warning', duration = NULL)
    })


    # if(any(is.na(m))) showNotification(paste("WARNING: The following codes are not in your profile, you will need to fill them manually in the table:", paste(paste(dat$Value[is.na(m)], "in column", dat$Column[is.na(m)]), collapse = ", ")), type = 'err', duration = NULL)

    ExtraCodesInProfile <- setdiff(UserProfile()$AllCodes$Value, dat$Value)


    withCallingHandlers({
      if(length(ExtraCodesInProfile)>0) warning(paste(paste("WARNING: The following codes are in your profile, but are not currently in your data. They will be ignored:", paste(paste(ExtraCodesInProfile, "in column", UserProfile()$AllCodes$Column[match(ExtraCodesInProfile, UserProfile()$AllCodes$Value)]), collapse = "\n ")), collapse = ". "))    },
      warning = function(warn){
        showNotification(paste(gsub("Warning in withCallingHandlers\\(\\{ \\:", "", warn), collapse = ". "), type = 'warning', duration = NULL)
      })

    # if(length(ExtraCodesInProfile)>0) showNotification(paste("WARNING: The following codes are in your profile, but are not currently in your data. They will be ignored:", paste(paste(ExtraCodesInProfile, "in column", UserProfile()$AllCodes$Column[match(ExtraCodesInProfile, UserProfile()$AllCodes$Value)]), collapse = ", ")), type = 'err', duration = NULL)


    dat$Definition <- UserProfile()$AllCodes$Definition[m]
    AllCodes(dat)
  })


  observe({
    dat <- AllCodes()
    # AllCodes(dat)
    for(i in 1L:nrow(dat)){
      dat$DefinitionSelector[i] <-
        # selector(id = paste0("slct", i), values = CodeOptions$Definition, titles = CodeOptions$Source)
        selector(id=paste0("slct", i), CodeOptions = CodeOptions)
    }

    AllCodes(dat)

  })


  output[["CodeTable"]] <- DT::renderDT({
    datatable(
      data =
        AllCodes(),
      selection = "none",
      escape = FALSE,
      rownames = FALSE,
      container = FooterWithHeader(AllCodes()),
      options = list(
        paging = F,
        searching = F,
        initComplete = JS(js),
        preDrawCallback = JS(
          "function() { Shiny.unbindAll(this.api().table().node()); }"
        ),
        drawCallback = JS(
          "function() { Shiny.bindAll(this.api().table().node()); }"
        )
      )
    )
  }, server = TRUE)

  observeEvent(input[["CodeTable_cell_selection"]], {
    info <- input[["CodeTable_cell_selection"]]
    dToEdit <- AllCodes()
    dToEdit[info$row,info$col-1] <- info$value # have to add the +1 because for some reason the indexing starts at 0 (probably because of the rbindlist function)
    AllCodes(dToEdit)

    shinyjs::show("dbProfile1")

  })


  output[["NewCodeTable"]] <- renderTable({
    AllCodes()[, c(1:3)]
  })


  # move on to next tab
  observeEvent(input$GoToCorrect, {
    updateTabItems(session, "tabs", "Correct")
    shinyjs::runjs("window.scrollTo(0, 0)")

  }, ignoreInit = TRUE)



  # Correction tab ####


  # show corrections arguments or not

  observe({
    for(f in unique(xCorr$Function)) {
      if(input[[f]] %in% "Yes") shinyjs::show(paste0(f, "Yes"))
      else shinyjs::hide(paste0(f, "Yes"))

    }

    if(any(unlist(reactiveValuesToList(input)[unique(xCorr$Function)]) %in% "Yes")) {
      shinyjs::show("ApplyCorrections")
      shinyjs::hide("SkipCorrections")
    } else {
      shinyjs::hide("ApplyCorrections")
      shinyjs::show("SkipCorrections")
    }
  })

  # apply corrections
  DataCorrected <- reactiveVal()
  CorrectionPlots <- reactiveVal()

  observeEvent(input$ApplyCorrections, {

    removeTab(inputId = "CorrectionPlots", target = "BotanicalCorrection")
    removeTab(inputId = "CorrectionPlots", target = "StatusCorrection")
    removeTab(inputId = "CorrectionPlots", target = "DiameterCorrection")

    CorrectionPlots(NULL)


  # DataCorrected <- eventReactive(input$ApplyCorrections, {
    Rslt <- DataFormated()
    lapply(
      unique(xCorr$Function),
      FUN = function(f){


        if(input[[f]] %in% "Yes") {

          print(f)

          # cl <- str2lang(paste0(f, "(", paste("Data = Rslt,", paste(paste(gsub(f, "", xCorr$ItemID[xCorr$Function %in% f]), "=",reactiveValuesToList(input)[xCorr$ItemID[xCorr$Function %in% f]]), collapse = ", ")),")"))
          cl <- paste0(f, "(", paste("Data = Rslt,", gsub("list\\(", "", paste(deparse(setNames(reactiveValuesToList(input)[xCorr$ItemID[xCorr$Function %in% f]], gsub(f, "", xCorr$ItemID[xCorr$Function %in% f]))), collapse = ""))))
          cl <- gsub('"FALSE"', "FALSE", cl)
          cl <- gsub('"TRUE"', "TRUE", cl)
          cl <- gsub('\"function', "function", cl)
          cl <- gsub(')\"', ")", cl)
          cl <- gsub(' (\\d*)L', " \\1", cl)

          # if(grepl('Source = "WFO"', cl)) {
          #   ext <- tools::file_ext(input$BotanicalCorrectionWFOData$datapath)
          #
          #   if(ext %in% "rds") WFOData = setDT(readRDS(input$BotanicalCorrectionWFOData$datapath))
          #   if(ext %in% "csv") WFOData = fread(input$BotanicalCorrectionWFOData$datapath)
          #   if(!ext %in% c("rds", "csv")) sendSweetAlert(
          #     session = session,
          #     title = "Oups !",
          #     text = "The is not a .rds file!",
          #     type = "error")
          #
          #   cl <- gsub("WFOData = .*)", "WFOData =WFOData)", cl) # this is to deal with the upload of world flora
          #
          # }




          cl <- str2lang(str2lang(deparse(cl)))


          withCallingHandlers({
            withProgress(message = paste("running", f),
                         detail = 'This may take a while...', value = 0, {
                          if(f %in% "BotanicalCorrection") Rslt <<- eval(cl)$Data else  Rslt <<- eval(cl)
                         })
          },
          warning = function(warn){
            showNotification(paste0(warn, collapse = "; "), type = 'warning', duration = NULL)
          },
          error = function(err){
            shiny:::reactiveStop(showNotification(paste0(err, collapse = "; "), type = 'err', duration = NULL))
          })

          if(f %in% c("DiameterCorrection", "StatusCorrection")) {

            p <- list(eval(str2lang(paste0(f, "Plot(Rslt)"))))
            names(p) <- f


            CorrectionPlots(append(CorrectionPlots(), p))

            p <- p[[f]]


            removeTab(inputId = "CorrectionPlots", target = f) # this to avoid several tabs to be added as function is turned off and on

            appendTab(inputId = "CorrectionPlots",
                      tabPanel(f,
                               do.call(tabsetPanel, c(id= paste0(f, 'IndCorrPlots'), type = "tabs", lapply(seq_len(p$nPages), function(k) {
                                 tabPanel(
                                 title = paste("Page", k)
                               )
                               }))),
                               plotOutput(paste0("IndCorrPlot", f))
                               ))
          }
          if(f %in% "BotanicalCorrection") {
            removeTab(inputId = "CorrectionPlots", target = f) # this to avoid several tabs to be added as function is turned off and on

            appendTab(inputId = "CorrectionPlots",
                      tabPanel(f,
                               uiOutput(outputId = "BotanicalCorrectionPlots")))
          }


      } else {
        removeTab(inputId = "CorrectionPlots", target = f)

      }

      }
    )
    DataCorrected(Rslt)
  })



  output$CorrectedTable <- DT::renderDT(DataCorrected()[,lapply(.SD, function(x) {if(all(is.na(x))) {NULL} else {x}} )], rownames = FALSE, options = list(pageLength = 8, scrollX=TRUE), container = FooterWithHeader(DataCorrected()[,lapply(.SD, function(x) {if(all(is.na(x))) {NULL} else {x}} )]), selection = "none")

  output$CorrectedTableSummary <- renderPrint(summary(DataCorrected()[,lapply(.SD, function(x) {if(all(is.na(x))) {NULL} else {x}} )]))




  observe({

    req(length(CorrectionPlots())> 0)
    # req(input$CorrectionPlots) %in% c("StatusCorrection", "DiameterCorrection")
    req(input[[paste0(input$CorrectionPlots, "IndCorrPlots")]])


    if(  req(input$CorrectionPlots) %in% c("StatusCorrection", "DiameterCorrection")) {
       p <- CorrectionPlots()[[input$CorrectionPlots]]

    print(input$CorrectionPlots )

        output[[paste0("IndCorrPlot", input$CorrectionPlots)]]<- renderPlot({
          # p$p +   ggforce::facet_wrap_paginate(vars(get(p$ID), ScientificName), scales = "free", ncol = min(p$n,3), nrow = p$i, page = as.numeric(gsub("Page ", "", input[[paste0(input$CorrectionPlots, "IndCorrPlots")]])))
          p$p[[as.numeric(gsub("Page ", "", input[[paste0(input$CorrectionPlots, "IndCorrPlots")]]))]]
        })
    }



  })

  observe({print(input$CorrectionPlots )})


  observe({

    req(DataCorrected())
    req(input$BotanicalCorrection %in% "Yes")
    req(input$CorrectionPlots  %in% "BotanicalCorrection")

    Rslt <- BotanicalCorrectionPlot(DataCorrected())
    DataCor <- Rslt$DataCor
    DataCorIncongruence <- Rslt$DataCorIncongruence

    output[["BotanicalCorrectionPlots"]]  <- renderUI(
      fluidRow(

        box(width = NULL,
            h2("Standardized scientific names"),
            p("The following boxes' title are all the corrected scientific names that were sligthly different than the ones listed in your data.  Within each box is a list of the orginal species names, and underneath each species name is the list of tree ID's that are affected by the correction."),
            p("Note that if a tree ID had a conflicting species ID, it will be listed here but also in the next section below."),
            p("You can collapse each box."),
            lapply(unique(DataCor$ScientificName_DataHarmonizationCor), function(spcor) box(
              title = spcor,
              "The following tag(s) had their scientific name corrected:",
              lapply(unique(DataCor[ScientificName_DataHarmonizationCor %in% spcor, ScientificName]), function(sp)
                box(title = sp,
                    width = NULL,
                    lapply(unique(DataCor[ScientificName_DataHarmonizationCor %in% spcor &  ScientificName %in% sp, IdTree]), function(x) p(x)),
                    collapsible = TRUE,
                    status = "warning")),
              solidHeader = TRUE,
              collapsible = TRUE,
              status = "primary"))
        ),


        box(width = NULL,
            h2("Conflicting species within a tree ID"),
            p("The following boxes list all the tree IDs that had conflicting scientific names withing your data."),
            p("Each boxe's title is the corrected scientific names.  Each box contains a list of tree IDs that had conflicting species ID and those species names are listed underneath each tree ID."),
            p("You can collapse each box."),
            lapply(unique(DataCorIncongruence$ScientificName_DataHarmonizationCor), function(sp) box(
              title = sp,
              "The following tag(s) had a conflicting species ID:",
              lapply(unique(DataCorIncongruence[ScientificName_DataHarmonizationCor %in% sp, IdTree]), function(id)
                box(title = id,
                    width = NULL,
                    lapply(unique(DataCorIncongruence[ScientificName_DataHarmonizationCor %in% sp &  IdTree %in% id, ScientificName]), function(x) p(x)),
                    collapsible = TRUE,
                    status = "warning")),
              solidHeader = TRUE,
              collapsible = TRUE,
              status = "warning"))

        )
      )
    )
  }, priority = 1)


  # observe({
  #
  #   lapply(names(CorrectionPlots()), function(f){
  #
  #     p <- CorrectionPlots()[[f]]
  #
  #   for(k in seq_len( p$nPages)) {
  #     output[[paste0(f, k)]] <- renderPlot({
  #       p$p +   ggforce::facet_wrap_paginate(vars(get(p$ID), ScientificName), scales = "free", ncol = min(p$n,3), nrow = p$i, page = k)
  #       })
  #   }
  #
  #
  #   })
  # })






  # place holder to put either corrected data or non corrected data
  DataDone <- reactiveVal()

  #decide what DataDone is going to be

  observeEvent(input$SkipCorrections,
               {
                 DataDone(DataFormated())
                 updateTabItems(session, "tabs", "OutputFormat")

                 shinyjs::runjs("window.scrollTo(0, 0)")


               })

  observeEvent(input$ApplyCorrections,{
    shinyjs::show("GoToOutput")
    DataDone(DataCorrected())
  })


  # move on to next tab
  observeEvent(input$GoToOutput, {
    updateTabItems(session, "tabs", "OutputFormat")
    shinyjs::runjs("window.scrollTo(0, 0)")


  })
  observeEvent(input$GoToDownload, {
    updateTabItems(session, "tabs", "Save")
    shinyjs::runjs("window.scrollTo(0, 0)")



  }, ignoreInit = T)



  # output tab ####

  DataOutput <- reactiveVal()
  profileOutput <- reactiveVal()
  UserCodeTranslationTable <- reactiveVal()
  CodeTranslationFinal <- reactiveValues(dt = NULL, output = NULL)

  observe( {
    if(input$predefinedProfileOutput != "No" )
      shinyjs::show("UseProfileOutput")
  })

  observeEvent(input$profileOutput, {
    shinyjs::show("UseProfileOutput")
  })

  observe({

    req(input$profileOutput$datapath)
    file <- input$profileOutput$datapath
    ext <- tools::file_ext(file)


    if(ext != "rds") sendSweetAlert(
      session = session,
      title = "Oups !",
      text = "The is not a .rds file!",
      type = "error")

    if(ext != "rds") output$RDSOutputWarning <- renderText("This is not a .rds file! Please upload a .rds file.")
    if(ext == "rds") output$RDSOutputWarning <- renderText("")


  })

  observe({

    req(input$UserCodeTranslationTable)
    file <- input$UserCodeTranslationTable$datapath

    ext <- tools::file_ext(file)


    if(ext != "csv") sendSweetAlert(
      session = session,
      title = "Oups !",
      text = "The is not a .csvs file!",
      type = "error") else UserCodeTranslationTable(read.csv(file))
  })


  observe({
    req(length(UserCodeTranslationTable())>0)
    shinyjs::show("updateCT")
  })

  observe({
    if(input$predefinedProfileOutput != "No") shinyjs::hide("profileOutputfileInput")
    if(input$predefinedProfileOutput == "No") shinyjs::show("profileOutputfileInput")

  })

  observeEvent(input$UseProfileOutput, {


    if(input$predefinedProfileOutput == "No") {

      if(is.null(input$profileOutput)) {

        sendSweetAlert(
          session = session,
          title = "Oups !",
          text = "You forgot to upload an output Profile!",
          type = "error"
        )

      }
}}, priority = 1)

  observeEvent(input$UseProfileOutput, {

    shinyjs::show("DontUseProfileOutput")
    shinyjs::hide("UseProfileOutput")

    if(input$predefinedProfileOutput == "No") {

      if(is.null(input$profileOutput)) shinyjs::hide("DontUseProfileOutput")


      file <- input$profileOutput$datapath
      ext <- tools::file_ext(file)



    } else {
      file <- paste0("data/", input$predefinedProfileOutput, "Profile.rds")
      ext <- tools::file_ext(file)


    }

    req(file)


    profileOutput(tryCatch({ readRDS(file)},
                           error = function(err){
                             shiny:::reactiveStop(showNotification("This is not a .rds file! Please upload a .rds file.", type = 'err', duration = NULL))
                           }))

    missingItemsInOutputProfile <- setdiff(x$ItemID, names(profileOutput()))

    # ignore the ones not needed (not perfect but is catchin most)
    missingItemsInOutputProfile <- missingItemsInOutputProfile[profileOutput()[x$if_X1_is_none[x$ItemID %in% missingItemsInOutputProfile]] %in% "none" | x$if_X1_is_none[x$ItemID %in% missingItemsInOutputProfile] %in% "none"]
    missingItemsInOutputProfile <- missingItemsInOutputProfile[profileOutput()[x$if_X2_is_none[x$ItemID %in% missingItemsInOutputProfile]] %in% "none" | x$if_X2_is_none[x$ItemID %in% missingItemsInOutputProfile] %in% "none"]
    missingItemsInOutputProfile <- missingItemsInOutputProfile[!profileOutput()[x$if_X2_isnot_none[x$ItemID %in% missingItemsInOutputProfile]] %in% "none" | x$if_X2_isnot_none[x$ItemID %in% missingItemsInOutputProfile] %in% "none"]



    if(length(missingItemsInOutputProfile) > 0) {
      sendSweetAlert(
        session = session,
        title = "Sorry !",
        text =  tags$span(
          "We have updated the app since that profile was created... It is missing the following latest elements:",
          tags$br(),
          tags$br(),
          tags$ul(

            lapply(paste0(missingItemsInOutputProfile, ": ", x$Label[x$ItemID %in% missingItemsInOutputProfile], " (in ", x$Group[x$ItemID %in% missingItemsInOutputProfile], " section);"), tags$li)
            #(Value should be a ", x$DefaultClass[x$ItemID %in% missingItemsInOutputProfile], " \"",  x$Default[x$ItemID %in% missingItemsInOutputProfile], "\");"), tags$li)

           ),
          tags$br(),
          "If this is a preloaded profile, please post an issue", tags$a("here", href="https://github.com/Alliance-for-Tropical-Forest-Science/DataHarmonization/issues"), ". But If you uploaded the file yourself, whoever built that profile needs to update it by reprocessing their data in the app, or they can contact us and we can help adding the missing element(s) 'manually'."
         #  tags$br(),
         #  "Here is a code example:",
         #
         #  tags$ul(
         #
         # lapply(lapply(c("input <- readRDS(file)", paste0("input$", missingItemsInOutputProfile, "<- ", ifelse(!lapply(reactiveValuesToList(input)[missingItemsInOutputProfile], class) %in% c("numeric", "integer", "double"), "\"", ""), x$Default[x$ItemID %in% missingItemsInOutputProfile], ifelse(!lapply(reactiveValuesToList(input)[missingItemsInOutputProfile], class) %in% c("numeric", "integer", "double"), "\"", "")), "save(RDS, file)"), tags$code), tags$li),
         # style="list-style-type: none"),
         # style = "text-align: left"
          ),


        html = T,
        type = "error")
    }


    if(paste(input$MeasLevel, profileOutput()$MeasLevel) %in% apply(rbind(
      expand.grid(i = c("Stem", "Tree"), o = c("Species", "Plot")),

      expand.grid(i = c("Species", "Plot"), o = c("Stem", "Tree"))), 1, paste, collapse = " ")) {



      sendSweetAlert(
        session = session,
        title = "Sorry !",
        text =  paste("The profile you selected is at the",  profileOutput()$MeasLevel, "level while yours is at the", input$MeasLevel, "level. We are not able to handle this yet."
        ),
        type = "error")

      DataOutput(NULL)

    } else {


      if(input$predefinedProfileOutput == "App") {
        DataOutput(DataDone())

        } else {
      # DataOutput(ReversedRequiredFormat(DataDone(), profileOutput(), x))

          withCallingHandlers({
            DataOutput(ReversedRequiredFormat(DataDone(), profileOutput(), x))
          },
          warning = function(warn){
            showNotification(paste0(warn, collapse = "; "), type = 'warning', duration = NULL)
          },
          error = function(err){
            shiny:::reactiveStop(showNotification(paste0(err, collapse = "; "), type = 'err', duration = NULL))
          })

      }

      # show and work on Codes translation if necessary
      if(!(profileOutput()$AllCodes[1,1] %in% "You have not selected columns for codes" || AllCodes()[1,1] %in% "You have not selected columns for codes")) {


        shinyjs::show("CodeTranslationsDiv")

        output$uiCodeTranslations <-  renderUI({
          div(
            DT::DTOutput("CodeTranslationTable"),
            fluidRow(
              box(width = NULL, title = "Output columns legend:",
                  DT::DTOutput("CodeTranslationTableLegend"))),
            # uiOutput("uiCodeTranslationTable"),
            br(),
            actionBttn("SeeCodeDefs", "See definitions/Update",
                       style = "material-flat",
                       size = "sm",
                       color = "default"),
            br(),
            hidden(DT::DTOutput("CodeTranslationFinal")),
            hidden(actionBttn(
              inputId = "ApplyCodeTranslation",
              label = "Apply Code Translation",
              style = "material-flat",
              size = "sm",
              color = "success"
            )),
            hidden(
              actionBttn(
                inputId = "RevertCodeTranslation",
                label = "Revert Code Translation",
                style = "material-flat",
                size = "sm",
                color = "warning"
              )

            )
          )
        })

      }
    }

    }, priority = 2)


  observe({

    req(!(profileOutput()$AllCodes[1,1] %in% "You have not selected columns for codes" || AllCodes()[1,1] %in% "You have not selected columns for codes"))

    AllCodesInput <- AllCodes()
    AllCodesOutput <- profileOutput()$AllCodes

    AllCodesInput$Value[is.na(AllCodesInput$Value)] <- "NA"

    # prepare this for later
    CodeTranslationFinal$dt <- data.frame(InputColumn = AllCodesInput$Column,
                                          InputValue = AllCodesInput$Value,
                                          InputDefinition = AllCodesInput$Definition)



    # now prepare the code translation table
    CodeTranslationTable <- matrix(paste(AllCodesOutput$Column, AllCodesOutput$Value, sep = "_mysep_"), ncol = nrow(AllCodesOutput),
                                   nrow = nrow(AllCodesInput), dimnames = list(AllCodesInput$Value, AllCodesOutput$Value), byrow = T)


      # if no user provided translation table, do our best looking at the definitions  - I added !llCodesInput$Definition[i] %in% "" to make sure empty definitions don't get matched
      for (i in seq_len(nrow(CodeTranslationTable))) {
        for(j in seq_len(ncol(CodeTranslationTable))) {
          if(AllCodesInput$Definition[i] %in% AllCodesOutput$Definition[j] & ! AllCodesInput$Definition[i] %in% "") CodeTranslationTable[i, j] = sprintf(
            '<input type="radio" name="%s_mysep_%s" value="%s" checked="checked" data-waschecked="true"/>',
            AllCodesInput$Column[i], AllCodesInput$Value[i], CodeTranslationTable[i,j]) else CodeTranslationTable[i, j] = sprintf(
              '<input type="radio" name="%s_mysep_%s" value="%s"/>',
              AllCodesInput$Column[i],  AllCodesInput$Value[i], CodeTranslationTable[i,j])
        }
      }


    # sketch if we keep one big codeTRanslationTable (not sepating into tabs)
    sketch = HTML(paste0("<table><thead><tr><th colspan = 2></th>", paste(paste0("<th class = 'coloredcolumn' colspan =", table(AllCodesOutput$Column)[unique(AllCodesOutput$Column)], " style='text-align:left'>",unique(AllCodesOutput$Column), "</th>"), collapse = ""), "</tr><tr><th></th><th></th>",paste(paste0("<th style= font-weight:400 title= '",AllCodesOutput$Definition, "'>", colnames(CodeTranslationTable), "</th>"), collapse = ""), "</tr></thead><tfoot><tr><th></th><th></th>",paste(paste0("<th style= font-weight:400>", colnames(CodeTranslationTable), "</th>"), collapse = ""), "</tr></tfoot></table>")) # title is for tooltips



    output$CodeTranslationTable <- DT::renderDT({
      datatable(
        data = cbind(AllCodesInput$Column, rownames(CodeTranslationTable), CodeTranslationTable),
        rownames = F,
        selection = 'none',
        escape = FALSE,
        extensions = c('RowGroup', 'FixedColumns'),
        options = list(dom = 't', paging = FALSE, ordering = FALSE, scrollX=TRUE,
                       rowGroup = list(dataSrc=c(0)),
                       # columnDefs = list(list(visible=FALSE, targets=c(1))),
                       fixedColumns = list(leftColumns = 2),
                       initComplete =JS(readLines("data/CodeTranslationTable_initcomplete.js"))),
        container = sketch,
        callback = JS("


              // Add radiobuttons

          table.rows().every(function(i, tab, row) {
          var $this = $(this.node());
          $this.attr('id', this.data()[0]+'_mysep_'+this.data()[1]);
          $this.addClass('shiny-input-radiogroup');
          });

            // allow radiobuttons to be deselected when clicked a second time

          $('input[type=radio]').on('click', function () {
            if ($(this).data('waschecked') == true) {
              $(this).prop('checked', false);
              $(this).data('waschecked', false);
              Shiny.setInputValue($(this).attr('name'), '');

            } else {
              $(this).data('waschecked', true);
                   }
          });


            // collapse rows of same column


            table.table().on('click', 'tr.dtrg-group', function () {
             // $(this).children('td').innerHTML('Hello world!)

              var rowsCollapse = $(this).nextUntil('.dtrg-group');
              $(rowsCollapse).toggleClass('hidden');
            });

           // Not sure what this is but it is needed

          Shiny.unbindAll(table.table().node());
          Shiny.bindAll(table.table().node());")
      )
      }, # this is generating the radio buttons in the body of the table
      server = FALSE)



    output$CodeTranslationTableLegend <- DT::renderDT({
      datatable(
        data = matrix(unique( AllCodesOutput$Column), nrow = floor(sqrt(length(unique( AllCodesOutput$Column)))), byrow = T),
        escape = FALSE,

        rownames = F,
        selection = 'none',

        options = list(paging = FALSE, ordering = FALSE, scrollX=TRUE),

        callback = JS('

        // this is to make a legend for the colors of columns in the table above. This list of colors needs to match the one in www/CodeTranslationTable_initcomplete.js
        // colors were found here: https://hihayk.github.io/scale/#10/10/15/0/-156/276/100/-60/FFFF9F/255/255/255/white

        var colors =
        ["#A0ADC0",
"#A0B9C6",
"#A0C9CC",
"#A0D2C9",
"#A0D8C0",
"#A0DFB3",
"#A0E5A3",
"#B1EBA0",
"#C7F2A0",
"#E1F8A0",
"#FFFF9F",
"#FFD39A",
"#FF9F95",
"#FF91C0",
"#FF8CFD",
"#EC87FF",
"#A782FF",
"#7DA1FF",
"#79EEFF",
"#74FFFF",
"#6FFFBC"];



        table.$("thead").css({"display":"none"});

        table.$("td").each(function(index) {
                         $(this).css({"background-color": colors[index]});
                          });


        Shiny.unbindAll(table.table().node());
        Shiny.bindAll(table.table().node());
                      ')
        )

    },
    server = FALSE)

  })

  observeEvent(input$updateCT, {

    AllCodesInput <- AllCodes()
    AllCodesOutput <- profileOutput()$AllCodes

    # first reset all radio buttons

    for(id in paste0(AllCodesInput$Column,  "_mysep_", AllCodesInput$Value)) {
      updateRadioButtons(session, inputId = id, selected = character(0))
    }


    # then figure out the ones that should be turned on

    idx.i <- match(paste(UserCodeTranslationTable()$InputColumn, UserCodeTranslationTable()$InputValue), paste(AllCodesInput$Column, AllCodesInput$Value))
    idx.j <-  match(paste(UserCodeTranslationTable()$OutputColumn, UserCodeTranslationTable()$OutputValue), paste(AllCodesOutput$Column, AllCodesOutput$Value))

    # matches are when both idx.i and idx,j are not NA --> those shold be checked
    # non matches is when one or the other is NA (technically only the output should be empty... but that is okay)

    idx.checked <- !is.na(idx.i) & !is.na(idx.j)
    idx.unchecked <- is.na(idx.i) | is.na(idx.j)

    if(!all((idx.checked + idx.unchecked) == 1)) stop("this probbably means the .csv file with your code translation is not matching the input and output data well...")

    for (i in seq_len(nrow(AllCodesInput))) {
      for(j in seq_len(nrow(AllCodesOutput))) {

        if(paste(i, j) %in% paste(idx.i[idx.checked], idx.j[idx.checked])) {

          # set input value, which already exists, as that match

          updateRadioButtons(session, inputId = paste0(AllCodesInput$Column[i],  "_mysep_", AllCodesInput$Value[i]), selected =  paste0(AllCodesOutput$Column[j],  "_mysep_", AllCodesOutput$Value[j]))

        }
        # else {
        #
        #   # reset the input value, in case there was a match before
        #   updateRadioButtons(session, inputId = paste0(AllCodesInput$Column[i],  "_mysep_", AllCodesInput$Value[i]), selected = "")
        #
        #       }
      }
    }


  })

  observeEvent(input$SeeCodeDefs, {

    shinyjs::show("CodeTranslationFinal")
    shinyjs::show("ApplyCodeTranslation")

    req(CodeTranslationFinal$dt$InputValue)

    dt <- CodeTranslationFinal$dt

    dt$OutputValue <- sapply(paste(dt$InputColumn, dt$InputValue, sep = "_mysep_"), function(x) input[[x]])
    dt$OutputColumn <- profileOutput()$AllCodes$Column[match(dt$OutputValue, paste(profileOutput()$AllCodes$Column, profileOutput()$AllCodes$Value, sep = "_mysep_"))]
    dt$OutputDefinition <- profileOutput()$AllCodes$Definition[match(dt$OutputValue, paste(profileOutput()$AllCodes$Column, profileOutput()$AllCodes$Value, sep = "_mysep_"))]
    dt$OutputValue <-  lapply(dt$OutputValue, function(x) if(!is.null(x) && x != "") rev(strsplit(x,"_mysep_")[[1]])[[1]] else NULL) # remove the column part in the value
    CodeTranslationFinal$output <- dt
  })


  # observe({
  #
  # }, priority = 1)

  output$CodeTranslationFinal <- DT::renderDT({
    req(CodeTranslationFinal$output)
    datatable(CodeTranslationFinal$output[c("InputColumn", "InputValue", "OutputColumn", "OutputValue", "InputDefinition", "OutputDefinition")],
              options = list( paging = FALSE, scrollX=TRUE),
              container = htmltools::withTags(table(
                # class = 'display',
                thead(
                  tr(
                    th('Input', colspan = 2),
                    th('Output', colspan = 2),
                    th('defintions', colspan = 2)
                  ),
                  tr(
                    th("Column"),
                    th("Value"),
                    th("Column"),
                    th("Value"),
                    th("Input"),
                    th("Output")
                  )
                )
              )), rownames = F)
  })


  observeEvent(input$RevertCodeTranslation, {
    shinyjs::show("ApplyCodeTranslation")
    shinyjs::hide("RevertCodeTranslation")

    if(input$predefinedProfileOutput == "App") {
      DataOutput(DataDone())

    } else {
      DataOutput(ReversedRequiredFormat(DataDone(), profileOutput(), x))
    }
  })

  observeEvent(input$ApplyCodeTranslation, {

    shinyjs::show("RevertCodeTranslation")
    shinyjs::hide("ApplyCodeTranslation")


    DataOutput <- DataOutput()

    idx <- which(names(DataOutput) %in% paste0("Original_", input$TreeCodes))

    CodesInput <- DataOutput[,..idx]
    # names(CodesInput) <- gsub("Original_", "", names(CodesInput))

    for(j in names(CodesInput)) {

      CodeTranslation <- CodeTranslationFinal$output[CodeTranslationFinal$output$InputColumn %in% gsub("Original_", "", j), ]
      CodeTranslation <- CodeTranslation[!is.na(CodeTranslation$OutputColumn),]

      CodesInput[,OriginalTranslated := CodesInput[,j, with = F]]

      for(i in seq_len(nrow(CodeTranslation))) {

        CodesInput[,OriginalTranslated := gsub(paste0("\\<", CodeTranslation$InputValue[i], "\\>"), paste(CodeTranslation$OutputColumn[i], CodeTranslation$OutputValue[i], sep = "_"), OriginalTranslated)]

      }

      CodesInput[,paste0(j, "_Translated") := OriginalTranslated]
      CodesInput[, OriginalTranslated:=NULL]

    }

    CodesInput[, Translation:=do.call(paste, c(.SD, sep = ";")), .SDcols=-seq_along(idx)]
    CodesInput[, grep("_Translated", colnames(CodesInput)):=NULL]

    CodesInput[, Translation:=gsub("\\<[a-zA-Z0-9]*\\>", "", Translation)] # remove codes that don't have an equivalence
    CodesInput[, Translation:=gsub("\\b(\\w+)\\b\\s*\\W\\s*(?=.*\\1)", "", Translation, perl = T)] # remove duplicated -  this deals with n-1 relationship (if different input refer to the same output code )


    OutCols <- unique(na.omit(CodeTranslationFinal$output$OutputColumn))

    for(j in OutCols) {
      CodesInput[,Final:=gsub(paste0(j, "_|\\<(\\w*?_\\w*?)\\>"), "", Translation)]
      CodesInput[, Final:=gsub("^[[:punct:]]*|[[:punct:]]*$","", Final)] # remove leading and trailing punctuation
      CodesInput[, Final:= gsub("([[:punct:]]){2,}","\\1", Final)] # remove repeated punctuation

      if(profileOutput()$TreeCodesSepMan %in% "")  CodesInput[, Final:=gsub(";", "", Final)] # replace ";" by "" if profileOutput()$TreeCodesSepMan is ""

      colnames(CodesInput) <- gsub("Final", j, colnames(CodesInput))
      CodesInput[,Final := NULL]
    }



    DataOutput(cbind(DataOutput, CodesInput[, ..OutCols]))
  })


  observeEvent(input$DontUseProfileOutput, {
    shinyjs::hide("DontUseProfileOutput")
    shinyjs::hide("CodeTranslationsDiv")

    # if(input$predefinedProfileOutput != "No") shinyjs::show("UseProfileOutput")
    updateRadioButtons(session, 'predefinedProfileOutput', selected = "No")


    # reset profile and code translation inputs and table (some of these are probably not necessary... it took me a while to make this work but I can't tell what combination of this and other changes need to happen)
    profileOutput(NULL)
    reset('profileOutput')


    lapply(paste(CodeTranslationFinal$dt$InputColumn, CodeTranslationFinal$dt$InputValue, sep = "_"), function(i) updateRadioButtons(session, inputId = i, choices=character(0), selected=character(0)))


    CodeTranslationFinal$dt <- NULL
    CodeTranslationFinal$output <- NULL

    output$CodeTranslationTable <- NULL

    # revert DataOutput
    DataOutput(NULL)

  }, priority = 1000)

  observeEvent(input$UseProfileOutput, {
    shinyjs::show("GoToDownload")

  }, ignoreInit = T)

  observeEvent(input$DontUseProfileOutput, {
    shinyjs::hide("GoToDownload")

  }, ignoreInit = T)

  # Visualize output
  output$DataOutput <- DT::renderDT(DataOutput()[,lapply(.SD, function(x) {if(all(is.na(x))) {NULL} else {x}} )], rownames = FALSE,
                                options = list(pageLength = 8, scrollX=TRUE),
                                container = FooterWithHeader(DataOutput()[,lapply(.SD, function(x) {if(all(is.na(x))) {NULL} else {x}} )]),
                                selection = "none")

  output$DataOutputSummary <- renderPrint(summary(DataOutput()[,lapply(.SD, function(x) {if(all(is.na(x))) {NULL} else {x}} )]))




  # download tab ####

  # Save all output as zip file

  output$dbZIP <- downloadHandler(

    filename = function() {
      paste(gsub(".csv", "", input$file1$name), '.zip', sep = '')
    },

    content = function(file) {

      withProgress(message = 'Compiling your files',
                   detail = 'This may take a while...', value = 0,
                   {

                     dir.create("original", showWarnings = F)
                     dir.create("processed", showWarnings = F)


                     # list all files that are currently in wd (so we can compare with the ones that are created in this step and save only those)
                     before_files <- list.files(recursive = T)

                     incProgress(1/15)

                     # Profile ##

                     inputs_to_save <- c(names(input)[names(input) %in% x$ItemID], "MeasLevel", "Tidy", "VariableName", "SelectedColumns", grep("Variablecolumns|TickedMelt|ValueName", names(input), value = T)) #, names(input)[names(input) %in% c(unique(xCorr$Function), xCorr$ItemID)]) # names(input)

                     Profile <- list()
                     for(input.i in inputs_to_save){
                       Profile[[input.i]] <-  input[[input.i]]
                     }

                     Profile[["AllCodes"]] <- AllCodes()
                     Profile[["CodeTranslationFinal"]] <- CodeTranslationFinal$output

                     Profile$ProfileName <- ifelse(input$predefinedProfile %in% "No", "Custom", input$predefinedProfile)


                     saveRDS(Profile, file = "original/input_profile.rds")

                     if("original/input_profile.rds" %in% list.files(recursive = T)) cat("original/input_profile.rds was saved\n")

                     incProgress(1/15)

                     # Output profile ####
                     profileOutput <- profileOutput()
                     profileOutput$ProfileName <- ifelse(input$predefinedProfileOutput %in% "No", "Custom", input$predefinedProfileOutput)


                     saveRDS(profileOutput(), file = "processed/output_profile.rds")

                     if("processed/output_profile.rds" %in% list.files(recursive = T)) cat("processed/output_profile.rds was saved\n")

                     incProgress(1/15)

                     # Correction info ##
                     corrInfo <- reactiveValuesToList(input)[xCorr$ItemID][reactiveValuesToList(input)[xCorr$Function] %in% "Yes"]

                     if(length(corrInfo) >0) {

                       corrInfo[sapply(corrInfo, is.null)] <- NULL # to avoid having fewer elements when unlisting

                       corrInfoDT<- data.table(xCorr[match(names(corrInfo), xCorr$ItemID), c("Function", "ItemID")])

                       corrInfoDT[, Parameter := gsub(Function, "", ItemID), by=seq_len(nrow(corrInfoDT))]

                       corrInfoDT[, value := unlist(corrInfo)]
                       corrInfoDT[, ItemID := NULL]

                       write.csv(corrInfoDT, "processed/CorrectionParametersApplied.csv", row.names = F)

                       if("CorrectionParametersApplied.csv" %in% list.files(recursive = T)) cat("CorrectionParametersApplied.csv was saved\n")
                     }

                     # Metadata ##

                     OurStandardColumn <- colnames(DataDone())

                     idxOriginal <- grep("Original$", OurStandardColumn)
                     idxTreeCodes <- grep("^Original_", OurStandardColumn)

                     CTwasApplied <- ifelse(is.null(input$ApplyCodeTranslation), FALSE, input$ApplyCodeTranslation > input$RevertCodeTranslation) # this is to know if we need to deal with code translation at all

                     if(!is.null(profileOutput()$TreeCodes) & CTwasApplied){

                       idxTreeCodesOut <- grep(paste(paste0("^", profileOutput()$TreeCodes, "$"), collapse = "|"), colnames(DataOutput()))
                       OurStandardColumn[idxTreeCodesOut] <- NA
                     }


                     YourInputColumn <- reactiveValuesToList(input)[xall$ItemID[match(OurStandardColumn, xall$ItemID)]]
                     YourInputColumn[idxTreeCodes] <- gsub("Original_", "", OurStandardColumn[idxTreeCodes])
                     YourInputColumn[idxOriginal] <- reactiveValuesToList(input)[gsub("Original", "", OurStandardColumn[idxOriginal])]
                     if(!is.null(profileOutput()$TreeCodes) & CTwasApplied) YourInputColumn[idxTreeCodesOut] <- paste(YourInputColumn[idxTreeCodes], collapse = " and/or ")


                     m <- match(OurStandardColumn, xall$ItemID)

                     # if(!is.null(profileOutput())) {
                     OutputColumn <-  profileOutput()[xall$ItemID[m]]
                     # } else {
                     #   OutputColumn <- OurStandardColumn
                     # }

                     m[idxOriginal] <- which(xall$ItemID %in% "XXXOriginal")
                     m[idxTreeCodes] <- which(xall$ItemID %in% "Original_XXX")
                     if(!is.null(profileOutput()$TreeCodes) & CTwasApplied) m[idxTreeCodesOut] <- which(xall$ItemID %in% "TreeCodesOutput")

                     OutputColumn[idxOriginal] <- OurStandardColumn[idxOriginal] # xall$ItemID[m[which(is.na(names(OutputColumn)))]]
                     OutputColumn[idxTreeCodes] <- OurStandardColumn[idxTreeCodes] # xall$ItemID[m[which(is.na(names(OutputColumn)))]]
                     if(!is.null(profileOutput()$TreeCodes) & CTwasApplied) OutputColumn[idxTreeCodesOut] <- colnames(DataOutput())[idxTreeCodesOut] # xall$ItemID[m[which(is.na(names(OutputColumn)))]]

                     incProgress(1/15)

                     # if(length(profileOutput()) > 0) {
                     Description = paste0(xall$Description[m], ifelse(xall$EvalUnit[m], paste(" in", profileOutput()[paste0(gsub("^X|^Y", "", xall$ItemID[m]),"UnitMan")]), ""))

                     # } else {
                     #   Description = paste0(xall$Description[m], ifelse(!xall$EvalUnit[m], paste(" in", xall$Unit[m]), ""))
                     # }


                     incProgress(1/15)

                     YourInputColumn[sapply(YourInputColumn, is.null) | YourInputColumn%in%"none"] <- NA
                     names(YourInputColumn)[is.na(names(YourInputColumn))] <- "NA"
                     OutputColumn[sapply(OutputColumn, is.null) | OutputColumn%in%"none"] <- NA


                     incProgress(1/15)
                     Metadata <- data.frame(YourInputColumn = unlist(YourInputColumn),
                                            OurStandardColumn,
                                            OutputColumn = unlist(OutputColumn),
                                            Description = Description)

                     incProgress(1/15)

                     Metadata <- Metadata[!(is.na(Metadata$YourInputColumn) & is.na(Metadata$OutputColumn)),] #remove lines with for columns that are missing in input and output


                     incProgress(1/15)

                     # if(length(profileOutput()) > 0) {
                     Metadata <- Metadata[!profileOutput()[Metadata$OurStandardColumn] %in% "none", ]  # remove lines that don't even exist in output ptofile
                     # }

                     incProgress(1/15)

                     Metadata <- Metadata[apply(setDT(DataOutput())[, Metadata$OutputColumn, with = F], 2, function(x) !all(is.na(x))), ] # keep only the columns that are not all NAs


                     incProgress(1/15)

                     Metadata <- unique(Metadata) # keep only unique rowa



                     write.csv(Metadata, file = "processed/metadata.csv", row.names = F)
                     if("processed/metadata.csv" %in% list.files(recursive = T)) cat("processed/metadata.csv was saved\n")

                     incProgress(1/15)


                     # Intput data ####

                     lapply(names(Data()), function(i) write.csv(Data()[[i]], paste0("original/", i, ".csv"), row.names = F))
                     # Formatted data ###
                     DataToSave <- DataOutput()
                     # setDF(DataToSave)

                     write.csv(DataToSave[,Metadata$OutputColumn, with = F], file = "processed/output_data.csv", row.names = FALSE)
                     if("processed/output_data.csv" %in% list.files(recursive = T)) cat("processed/output_data.csv was saved\n")

                     incProgress(1/15)

                     # Tree Codes definitions ##
                     if(length(input$TreeCodes) > 0) write.csv(AllCodes()[, c("Column", "Value", "Definition")
                     ], "original/tree_codes_metadata.csv", row.names = FALSE)

                     incProgress(1/15)

                     # Tree Codes Translation ##

                     if(!is.null(input$ApplyCodeTranslation) & length(input$ApplyCodeTranslation ) > 0 & length(CodeTranslationFinal$output) > 0) {

                       CodeTranslationFinal$output$OutputValue <- sapply(CodeTranslationFinal$output$OutputValue, function(x) ifelse(is.null(x), NA, x)) # this is to avoid having a list

                       write.csv(CodeTranslationFinal$output[c("InputColumn", "InputValue", "OutputColumn", "OutputValue", "InputDefinition", "OutputDefinition")], "processed/tree_codes_translation.csv", row.names = FALSE)

                       if("processed/tree_codes_translation.csv" %in% list.files(recursive = T)) cat("processed/tree_codes_translation.csv was saved\n")

                     }

                     incProgress(1/15)

                     # save ZIP

                     FilesToZip <-  setdiff(list.files(recursive = T), before_files)

                     zip(zipfile=file, files=c("log.txt",FilesToZip))


                     # remove files and folders from the app
                     file.remove(FilesToZip)
                     unlink("original", recursive = T)
                     unlink("processed", recursive = T)


                   })
    },
    contentType = "application/zip"

  )

  # Save profile .Rdata

  output$dbProfile <- output$dbProfile1 <- output$dbProfile2 <-downloadHandler(
    filename = function() {
      paste(gsub(".csv", "", input$file1$name), '_Profile.rds', sep = '')
    },
    content = function(file) {
      inputs_to_save <- c(names(input)[names(input) %in% x$ItemID], "MeasLevel", "Tidy", "VariableName", "SelectedColumns", grep("Variablecolumns|TickedMelt|ValueName", names(input), value = T)) # names(input)
      Profile <- list()
      for(input.i in inputs_to_save){
        Profile[[input.i]] <-  input[[input.i]]
      }
      Profile[["AllCodes"]] <- AllCodes()
      Profile[["CodeTranslationFinal"]] <- CodeTranslationFinal$output
      Profile[["ProfileName"]] <- "Custom"
      saveRDS( Profile, file = file)
    }
  )


  # Save script .R

  output$dbCode <- downloadHandler(
    filename = function() {
      paste(gsub(".csv", "",input$file1$name), '_Code.R', sep = '')
    },
    content = function(file) {
      text_upload <- glue::glue(
        "
      # install DataHarmonization package
      githubinstall::githubinstall('Alliance-for-Tropical-Forest-Science/DataHarmonization')
      library(DataHarmonization)

      # upload the data
       Data <- data.table::fread('{input$file1$name}', header = {input$header}, sep = '{input$cbSeparator}', check.names = T, encoding = 'UTF-8')

      # upload your profile (saved via shiny app)
      Profile <- readRDS(paste0(gsub('.csv', '', '{input$file1$name}'), '_Profile.rds'))

      # format your data
      DataFormated <- RequiredFormat( Data, input = Profile)
      ")
      writeLines(text_upload, file)
    }
  )

  # # Save metadata .csv
  #
  # output$dbMetadata <- downloadHandler(
  #   filename = function() {
  #     paste(gsub(".csv", "", input$file1$name), '_Metadata.csv', sep = '')
  #   },
  #   content = function(file) {
  #
  #     YourInputColumn <- reactiveValuesToList(input)[xall$ItemID[match(colnames(DataDone()), xall$ItemID)]]
  #     OurStandardColumn <- colnames(DataDone())
  #
  #     if(!is.null(profileOutput())) {
  #       m <- match(OurStandardColumn, xall$ItemID)
  #       OutputColumn <-  profileOutput()[xall$ItemID[m]]
  #       OutputColumn[which(is.na(names(OutputColumn)))] <- xall$ItemID[m[which(is.na(names(OutputColumn)))]]
  #       Description = paste0(xall$Description[m], ifelse(!xall$Unit[m] %in% c("-", "year"), paste(" in", profileOutput()[paste0(gsub("^X|^Y", "", xall$ItemID[m]),"UnitMan")]), ""))
  #
  #     } else {
  #       OutputColumn <- OurStandardColumn
  #       Description = paste0(xall$Description[match(OurStandardColumn, xall$ItemID)], ifelse(!xall$Unit[match(OurStandardColumn, xall$ItemID)] %in% c("-", "year"), paste(" in", xall$Unit[match(OurStandardColumn, xall$ItemID)]), ""))
  #     }
  #
  #     YourInputColumn[is.na(names(YourInputColumn))|YourInputColumn%in%"none"] <- NA
  #     names(YourInputColumn)[is.na(names(YourInputColumn))] <- "NA"
  #     # OutputColumn[is.na(names(OutputColumn))|OutputColumn%in%"none"] <- NA
  #     OutputColumn[OutputColumn%in%"none"] <- NA
  #
  #
  #     Metadata <- data.frame(YourInputColumn = unlist(YourInputColumn),
  #                            OurStandardColumn,
  #                            OutputColumn = unlist(OutputColumn),
  #                            Description = Description)
  #
  #     #remove lines with for columns that are missing in input and output
  #     Metadata <- Metadata[!(is.na(Metadata$YourInputColumn) & is.na(Metadata$OutputColumn)),]
  #
  #     # remove lines that don't even exist in output ptofile
  #
  #     Metadata <- Metadata[!profileOutput()[Metadata$OurStandardColumn] %in% "none", ]
  #
  #     # save
  #     write.csv(Metadata, file = file, row.names = F)
  #   }
  # )

  # Help tab ####
  output$AppGeneralWorkflow <- renderImage(
    list(src = "www/AppGeneralWorkflow.png",
         contentType = "image/png",
         alt = "test",
         width = "100%",
         align = "center"),
    deleteFile = F)


}
