
# Fichier pour gérer les interactions de l'application Shiny


# source the REquiredFormat function to get the list of arguments
# source(paste0(dirname(dirname(getwd())), "/R/RequiredFormat.R")) # ***make this better!!**
# x <- as.list(formals(RequiredFormat)[-1])

# read in csv file that has all we want to show in app
x <- read.csv("data/interactive_items.csv")
x <- x[x$Activate, ]
for(i in unique(x$UI)) assign(paste0("x", i), x[x$UI %in% i,])

# install DataHarmonization package
# devtools::install_github("Alliance-for-Tropical-Forest-Science/DataHarmonization")
library(DataHarmonization)

server <- function(input, output, session) {

  # read file and create data table
  Data <- reactive({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)

    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))

    data.table::fread(file$datapath,
                      header = input$header,
                      sep = input$cbSeparator)
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
  output$tabData <- renderDT({
    if (!is.null(input$file1$name))
      Data()
  }, rownames = FALSE,
  options = list(pageLength = 8, scrollX=TRUE))

  # Avoid seeing errors
  text_reactive <- reactiveValues(
    NoData = "No data has been submitted yet."
  )

  output$ui1 <- renderUI({
    p(text_reactive$NoData)
  })
  output$ui2 <- renderUI({
    p(text_reactive$NoData)
  })
  output$ui3 <- renderUI({
    p(text_reactive$NoData)
  })
  output$ui4 <- renderUI({
    p(text_reactive$NoData)
  })

  # create options to choose from:

  ColumnOptions <- eventReactive(Data(), { c("none", colnames(Data())) })

  UnitOptions <- eventReactive(Data(),
                                {c("mm", "cm", "dm", "m")
                                })

  AreaUnitOptions <- eventReactive(Data(),
                                   {c("m2", "ha", "km2")
                                   })


  LifeStatusOptions <- eventReactive(input$LifeStatus, {
    sort(unique(Data()[[input$LifeStatus]]))})

  CommercialOptions <- eventReactive(input$CommercialSp, {
    sort(unique(Data()[[input$CommercialSp]]))})

  OtherOptions <- eventReactive(Data(), {""})

  # enter column names for each element of the RequiredFormat function

  observeEvent(input$file1,
               {
                 output$ui1 <- renderUI({
                   lapply(1:nrow(x1), function(i) {

                     eval(parse(text = paste(x1$ItemType[i], "(inputId = x1$ItemID[i], label = ifelse(x1$helpText[i] %in% '', x1$Label[i], paste0(x1$Label[i], ' (', x1$helpText[i], ')')),", x1$argument[i],"= get(x1$argValue[i])()", ifelse(x1$Options[i] != FALSE, paste0(", options = ", x1$Options[i]), ""), ifelse(x1$Multiple[i] %in% TRUE, ", multiple = TRUE)", ")"))))

                   })

                 })

                 output$ui2 <- renderUI({

                   lapply(c(1:nrow(x2)), function(i) {
                     if(input[[x2$if_X1_is_none[i]]] %in% "none")
                       eval(parse(text = paste(x2$ItemType[i], "(inputId = x2$ItemID[i], label = ifelse(x2$helpText[i] %in% '', x2$Label[i], paste0(x2$Label[i], ' (', x2$helpText[i], ')')),", x2$argument[i], "= get(x2$argValue[i])()", ifelse(x2$Options[i] != FALSE, paste0(", options = ", x2$Options[i]), ""), ifelse(x2$Multiple[i] %in% TRUE, ", multiple = TRUE)", ")"))))

                   })
                 })

                 output$ui3 <- renderUI({

                   lapply(c(1:nrow(x3)), function(i) {
                     if(input[[x3$if_X1_is_none[i]]] %in% "none" & !input[[x3$if_X2_isnot_none[i]]] %in% "none" )

                       eval(parse(text = paste(x3$ItemType[i], "(inputId = x3$ItemID[i], label = ifelse(x3$helpText[i] %in% '', x3$Label[i], paste0(x3$Label[i], ' (', x3$helpText[i], ')')),", x3$argument[i], "= get(x3$argValue[i])()", ifelse(x3$Options[i] != FALSE, paste0(", options = ", x3$Options[i]), ""), ifelse(x3$Multiple[i] %in% TRUE, ", multiple = TRUE)", ")"))))

                   })
                 })

                 output$ui4 <- renderUI({

                   lapply(c(1:nrow(x4)), function(i) {
                     if(!input[[x4$if_X2_isnot_none[i]]] %in% "none" )
                       eval(parse(text = paste(x4$ItemType[i], "(inputId = x4$ItemID[i], label = ifelse(x4$helpText[i] %in% '', x4$Label[i], paste0(x4$Label[i], ' (', x4$helpText[i], ')')),", x4$argument[i], "= get(x4$argValue[i])()", ifelse(x4$Options[i] != FALSE, paste0(", options = ", x4$Options[i]), ""), ifelse(x4$Multiple[i] %in% TRUE, ", multiple = TRUE)", ")"))))

                   })
                 })

                 output$ui5 <- renderUI({

                   lapply(c(1:nrow(x5)), function(i) {
                     if(input[[x5$if_X1_is_none[i]]] %in% "none" & input[[x5$if_X2_is_none[i]]] %in% "none" )

                       eval(parse(text = paste(x5$ItemType[i], "(inputId = x5$ItemID[i], label = ifelse(x5$helpText[i] %in% '', x5$Label[i], paste0(x5$Label[i], ' (', x5$helpText[i], ')')),", x5$argument[i], "= get(x5$argValue[i])()", ifelse(x5$Options[i] != FALSE, paste0(", options = ", x5$Options[i]), ""), ifelse(x5$Multiple[i] %in% TRUE, ", multiple = TRUE)", ")"))))

                   })
                 })

               })


  # format data usin the input

  DataFormated <- eventReactive(input$LaunchFormating | input$UpdateTable, {

    tryCatch({
      RequiredFormat(Data = Data(), isolate(reactiveValuesToList(input)), x, ThisIsShinyApp = T)
    },
    warning = function(warn){
      showNotification(gsub("in RequiredFormat\\(Data = Data\\(\\), isolate\\(reactiveValuesToList\\(input\\)\\),", "", warn), type = 'warning', duration = NULL)
    },
    error = function(err){
      showNotification(gsub("in RequiredFormat\\(Data = Data\\(\\), isolate\\(reactiveValuesToList\\(input\\)\\),", "", err), type = 'err', duration = NULL)
    })
  })

  # Visualize output
  output$tabDataFormated <- renderDT({
    # validate(
    #   need(req(DataFormated()), "AA")
    # )
      DataFormated()
  }, rownames = FALSE,
  options = list(pageLength = 8, scrollX=TRUE))

  # save final data table

  output$dbFile <- downloadHandler(
    filename = function() {
      paste(gsub(".csv", "", input$file1$name), '_formated.csv', sep = '')
    },
    content = function(file) {
      write.csv(DataFormated(), file, row.names = FALSE)
    }
  )

  # save profile Rdata file

  output$dbProfile <- downloadHandler(
    filename = function() {
      paste(gsub(".csv", "", input$file1$name), '_Profile.rds', sep = '')
    },
    content = function(file) {
      inputs_to_save <- names(input)[names(input) %in% x$ItemID]
      Profile <- list()
      for(input.i in inputs_to_save){
        Profile[[input.i]] <-  input[[input.i]]
      }
      saveRDS( Profile, file = file)
    }
  )

  # save code needed to produce the table

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
       Data <- data.table::fread('{input$file1$name}', header = {input$header}, sep = '{input$cbSeparator}')

      # upload your profile (saved via shiny app)
      Profile <- readRDS(paste0(gsub('.csv', '', '{input$file1$name}'), '_Profile.rds'))

      # format your data
      DataFormated <- ParacouSubsetFormated <- RequiredFormat( Data, input = Profile)
      ")
      writeLines(text_upload, file)
    }
  )

  # Save metadata

  output$dbMetadata <- downloadHandler(
    filename = function() {
      paste(gsub(".csv", "", input$file1$name), '_Metadata.csv', sep = '')
    },
    content = function(file) {
      columns_to_save <- colnames(DataFormated())
      Metadata <- data.frame(Field = columns_to_save,
                             Description = x$Description[match(columns_to_save, x$ItemID)])

      write.csv(Metadata, file = file, row.names = F)
    }
  )

}
