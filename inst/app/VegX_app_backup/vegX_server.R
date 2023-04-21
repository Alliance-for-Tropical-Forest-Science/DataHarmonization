
# source script to get VegX_tree
VegXtree <- readRDS("data/VegXtree.rds")
VegXtree$Do(function(x) x$inputId <-gsub("/", "_", x$pathString))

tree <- cbind(ToDataFrameTypeCol(VegXtree), ToDataFrameTable(VegXtree, "name", "inputId", "annotation"))

tree$subtext <- paste("<strong>", tree$name, "</strong>", stringr::str_replace_all( stringr::str_wrap(tree$annotation, width = 50), "\\n", "<br>"))
tree <- split(tree, factor(tree$level_2, levels = unique(tree$level_2)))

choices <- sapply(tree, function(x) setNames(as.list(gsub("VegX_", "", x$inputId)), x$name))
subtext <- unlist(sapply(tree, "[[", "subtext"), recursive = T)

# level3 <-  sapply(tree, "[[", "level_3")
# choices <- mapply(FUN = function(x, y) split(x, factor(y, levels = unique(y))), x = choices, y = level3)
# subtext <- mapply(FUN = function(x, y) split(x, factor(y, levels = unique(y))), x = subtext, y = level3)

# test to show we can only have 1 2 levels of nestedness
# choices = list(A = c("A", B = c("b", "B")), C = c("C", "D"))
# subtext = unlist(choices)

server <- function(input, output, session) {

  observeEvent(input$nTable,
               {
                 output$ui_uploadTables <- renderUI({
                   lapply(1:input$nTable, function(i) {

                     column(width = 3,
                            # load button for main data file (csv format)
                            box(title = paste("Table", i),
                                width = NULL,
                                fileInput(inputId = paste0("file", i), "Choose CSV File", accept = ".csv"),
                                # does the dataframe have a header?
                                checkboxInput( paste0("header", i), "Header", TRUE),
                                # choose separator
                                selectInput(inputId = paste0("cbSeparator", i),
                                            label = "Separator",
                                            choices = c("auto", ",", "\t",  "|", ";", ":"), # pb with tab
                                            selected = "auto"
                                ),
                                textInput(inputId = paste0("TableName", i),
                                          label = "Give an explicit UNIQUE and SHORT name to this table. No space, no special character, no accent.",
                                          value = paste0("Table", i)
                                ),
                                # textInput(inputId = paste0("TableDescription", i),
                                #           label = "Give an explicit description of your table.",
                                #           value = paste0("My Table", i)
                                # ),
                                tags$textarea(id=paste0("TableDescription", i),
                                              label = "Give an explicit description of your table...",
                                              rows=5,
                                              cols = 6,
                                              placeholder = paste0("My Table", i))
                            )
                     )


                   })


                 })
               })

  # read file and create data table
  Data <- reactive({
    req(input$file1)
    sapply(c(reactiveValuesToList(input)[paste0("TableName", 1:input$nTable)]), function(n) {

      i = which(c(reactiveValuesToList(input)[paste0("TableName", 1:input$nTable)]) %in% n)
      file <- input[[paste0("file", i)]]
      ext <- tools::file_ext(file$datapath)

      req(file)
      validate(need(ext == "csv", "Please upload a csv file"))

      data.table::fread(file$datapath,
                        header = input[[paste0("header", i)]],
                        sep = input[[paste0("cbSeparator", i)]])
    }, simplify = F)

  })


  observeEvent(input$profile, {
    file <- input$profile
    ext <- tools::file_ext(file$datapath)

    req(file)
    validate(need(ext == "rds", "Please upload a csv file"))

    profile <- readRDS(file$datapath)

    for(i in which(x$ItemID %in% names(profile))) {
      eval(parse(text = paste0("updateTextInput(session, '", x$ItemID[i], "', value = profile$", x$ItemID[i], ")")))
      # updateTextInput(session, "Site", value = profile$Site)
    }




  })


  # render data table
  # output$tabData <- renderDT({
  #   if (!is.null(input$file0$name))
  #     Data()
  # }, rownames = FALSE,
  # options = list(pageLength = 8, scrollX=TRUE))

  # Avoid seeing errors
  text_reactive <- reactiveValues(
    NoData = "No data has been submitted yet."
  )

  observeEvent(input$submit, {
  output$uiheader <- renderUI({

    lapply(1:isolate(input$nTable), function(i) {
      X <- isolate(colnames(Data()[[i]]))
      title <- isolate(reactiveValuesToList(input)[paste0("TableName", i)])
      box(
        title = title,
        lapply(X, function(x) pickerInput(inputId = paste(title, x, sep = "_"), label = x, choices = choices, multiple = T, options = list(`live-search` =T, width = F), choicesOpt = list(content = subtext), width = '75%'))

      )
      })


  })

  updateTabItems(session, "tabs", "headers")


  })




  # Visualize output
 observeEvent(input$update, {

  X <- isolate(unlist(sapply(1:input$nTable, function(i) paste(reactiveValuesToList(input)[paste0("TableName", i)], names(Data()[[i]]), sep = "_"))))
  value_entered <- sapply(reactiveValuesToList(input)[X], paste, collapse = ";")
  inputValues <- data.frame(input = names(value_entered), value_entered )
  inputValues <- inputValues[inputValues$value_entered != "",]
  output$visualiseInput <- renderTable(inputValues)

  })


  # save profile Rdata file

  output$dbProfile <- downloadHandler(
    filename = function() {
      paste("My_Profile_", Sys.Date(), ".rds", sep = '')
    },
    content = function(file) {
      inputs_to_save <- names(input)#[names(input) %in% x$ItemID]
      Profile <- list()
      for(input.i in inputs_to_save){
        Profile[[input.i]] <-  input[[input.i]]
      }
      saveRDS( Profile, file = file)
    }
  )


}
