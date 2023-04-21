# to split CodeTRanslationTable into tabs but it is not working because then the non active tabs are making a mess and the input[[]] don't work

output$uiCodeTranslations <-  renderUI({
  div(
    DTOutput("CodeTranslationTable"),
    # uiOutput("uiCodeTranslationTable"),
    ...

})

output$uiCodeTranslationTable <- renderUI({
  req(CodeTranslationTable)
  do.call(tabsetPanel, c(id='ct', type = "tabs", lapply(unique(AllCodesOutput$Column), function(i) {
    tabPanel(
      title=i,
      DTOutput(outputId = paste0("ct_", i))
    )
  })))


})

observe({
  req(CodeTranslationTable)

  cnames = lapply(unique(AllCodesOutput$Column), colnames)

  lapply(unique(AllCodesOutput$Column), function(i) {


    # sketch if separate codeTRanslationTable into tabs
    sketch = HTML(paste0("<table><thead><tr><th colspan = 2></th>", paste(paste0("<th colspan =", table(AllCodesOutput$Column)[i], " style='text-align:left'>",i, "</th>"), collapse = ""), "</tr><tr><th></th><th></th>",paste(paste0("<th style= font-weight:400 title= '",AllCodesOutput$Definition[AllCodesOutput$Column %in% i], "'>", colnames(CodeTranslationTable[,AllCodesOutput$Column %in% i]), "</th>"), collapse = ""), "</tr></thead><tfoot><tr><th></th><th></th>",paste(paste0("<th style= font-weight:400>", colnames(CodeTranslationTable[,AllCodesOutput$Column %in% i]), "</th>"), collapse = ""), "</tr></tfoot></table>")) # title is for tooltips


    output[[paste0("ct_", i)]] <- renderDT( {datatable(
      data = cbind(AllCodesInput$Column, rownames(CodeTranslationTable), CodeTranslationTable[,AllCodesOutput$Column %in% i]) ,
      rownames = F,
      selection = 'none',
      escape = FALSE,
      extensions = c('RowGroup', 'FixedColumns'),
      options = list(dom = 't', paging = FALSE, ordering = FALSE, scrollX=TRUE,
                     rowGroup = list(dataSrc=c(0)),
                     # columnDefs = list(list(visible=FALSE, targets=c(1))),
                     fixedColumns = list(leftColumns = 2),
                     initComplete =JS('

                         // This to give count of code in the grey line that can collapse the code



    function(settings, json) {

    var olds = $("tr.dtrg-group").children( "td" )

            $("tr.dtrg-group").each(function(i) {

                  var pattern = /( \\([0-9]*\\))/g;
                  var old = $(this).children( "td" ).html()
                  var older = old.replace(pattern, "")

                  var count = Object.keys($(this).nextUntil(".dtrg-group")).length -2;

                  $(this).children( "td" ).html(older + " (" + count+ ") ");
              })

    }
')),
      container = sketch,
      callback = JS("


              // Add radiobuttons

          table.rows().every(function(i, tab, row) {
          var $this = $(this.node());
          $this.attr('id', this.data()[0]+'_'+this.data()[1]);
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
    )}, # this is generating the radio buttons in the body of the table
    server = FALSE)

  }
  )
})
