#' Gadget to test and ajust regular expressions in a dictionary.
#' @param dictionary Tibble. Dictionary with at least a column "pattern".
#' @param corpus     List. List of texts.
#' @return A new dictionary
#' @importFrom miniUI miniPage
#' @importFrom miniUI gadgetTitleBar
#' @importFrom miniUI miniTabstripPanel
#' @importFrom miniUI miniTabPanel
#' @importFrom miniUI miniContentPanel
#' @importFrom shiny fillCol
#' @importFrom shiny fillRow
#' @importFrom shiny icon
#' @importFrom shiny fileInput
#' @importFrom shiny textInput
#' @importFrom shiny dateInput
#' @importFrom shiny numericInput
#' @importFrom shiny textAreaInput
#' @importFrom shiny selectInput
#' @importFrom shiny checkboxInput
#' @importFrom shiny downloadButton
#' @importFrom shiny downloadHandler
#' @importFrom shiny stopApp
#' @importFrom shiny runGadget
#' @importFrom shiny conditionalPanel
#' @importFrom shiny tags
#' @importFrom shiny dataTableOutput
#' @importFrom shiny tableOutput
#' @importFrom shiny uiOutput
#' @importFrom shiny plotOutput
#' @importFrom shiny actionButton
#' @importFrom shiny renderDataTable
#' @importFrom shiny renderUI
#' @importFrom shiny renderPlot
#' @importFrom shiny renderText
#' @importFrom shiny renderTable
#' @importFrom shiny reactive
#' @importFrom shiny reactiveValues
#' @importFrom shiny observe
#' @importFrom shiny HTML
#' @importFrom shiny htmlOutput
#' @importFrom shinythemes shinytheme
#' @importFrom shiny observeEvent
#' @importFrom shiny withProgress
#' @importFrom shiny incProgress
#' @importFrom shiny includeHTML
#' @importFrom shiny withMathJax
#' @importFrom rhandsontable rhandsontable
#' @importFrom rhandsontable renderRHandsontable
#' @importFrom rhandsontable hot_to_r
#' @importFrom shiny isolate 
#' @importFrom shiny paneViewer
#' @importFrom tibble column_to_rownames
#' @importFrom tibble tibble
#' @importFrom tibble as_tibble
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize_all
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom dplyr case_when
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr everything
#' @importFrom tidyr spread
#' @importFrom stringr str_view
#' @export


text_test_regex <- function(dictionary, corpus) {
  
  ui <- miniPage(
    theme = shinytheme("spacelab"),
    
    gadgetTitleBar("Regular Expressions"),
    miniTabstripPanel(
      miniTabPanel("Text regex",
        icon = icon("eye"),
        miniContentPanel(
          fillCol(
            flex = c(1,1,10),
            fillRow(
              flex = c(1, 1, 1),
              uiOutput("pattern"),
              textInput("try", "Or try:"),
              uiOutput("text")
            ),
            
            tags$hr(),
            
            fillRow(
              htmlOutput("view")
            )
          )
        )
      ),
      miniTabPanel("Correct patterns",
                   icon = icon("edit"),
                   miniContentPanel(
                     fillRow(
                       flex = c(10,2),
                       rhandsontable::rHandsontableOutput("edit"),
                       shiny::actionButton("apply", "Apply change")
                     )
                   )
      )
    )
  )
  

  server <- function(input, output, session) {

    # Bind variables
    
    
    # Create reactive values
    values <- shiny::reactiveValues()
    values$edit <- dictionary
    values$corpus <- unlist(corpus)
    
    # Create selection lists
    output$pattern <- shiny::renderUI({
      choice <- values$edit$pattern
      shiny::selectInput(
        "pattern",
        "Pattern:",
        choices = choice,
        multiple = FALSE
      )
    })
    
    output$text <- shiny::renderUI({
      choice <- 1:length(values$corpus)
      shiny::selectInput(
        "text",
        "Text:",
        choices = choice,
        multiple = FALSE
      )
    })
    
    selectext <- reactive({
      if (!is.null(input$text)) text <- values$corpus[as.numeric(input$text)] else text <- values$corpus[1]
      text
    })
    
    output$view <- renderUI({
      if (!is.null(selectext()) & !is.null(input$pattern)){
        text <- selectext()
        if (!is.null(input$try) & input$try != "") pattern <- input$try else pattern <- input$pattern
        output <- stringr::str_view_all(text, pattern)
        output <- gsub("<span class='match'>", '<font size="4" color="red"><b>', output$x$html)
        output <- gsub("</span>", "</b></font>", output)
      } else output <- ""
      HTML(output)
    })
    
    # Editable table
    output$edit <- renderRHandsontable({
      edit <- values$edit %>%
        rhandsontable(stretchH = "all", width = '100%', height = 400, rowHeaders = FALSE)
    })
    
    # Add the references manually selected
    observeEvent(input$apply, {
      values$edit <- isolate(input$edit) %>%
        hot_to_r()
    })
    
    
    # List of action to do when exiting
    observeEvent(input$done, {
      values$edit
      stopApp()
    })
  }
  runGadget(ui, server, viewer = paneViewer(minHeight = "maximize"))
}
