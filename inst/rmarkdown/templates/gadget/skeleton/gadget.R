#' Template for a gadget.
#' @return What the coder decides to return.
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
#' @importFrom shiny observeEvent
#' @importFrom shiny withProgress
#' @importFrom shiny incProgress
#' @importFrom shiny includeHTML
#' @importFrom shiny withMathJax
#' @importFrom colourpicker colourInput
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
#' @importFrom exams exams2nops
#' @importFrom exams exams2pdf
#' @importFrom exams exams2moodle
#' @importFrom exams exams2blackboard
#' @importFrom exams exams2html
#' @importFrom tth tth
#' @importFrom tibble tibble
#' @importFrom purrr map
#' @importFrom readr read_csv
#' @importFrom webshot webshot
#' @importFrom stats na.omit
#' @importFrom stringr str_split
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace_all
#' @importFrom utils data
#' @importFrom utils read.csv
#' @importFrom writR stat_totals
#' @export


gadget <- function() {
  
  ui <- miniPage(
    theme = shinytheme("spacelab"),
    
    gadgetTitleBar("Name of your gadget"),
    miniTabstripPanel(
      miniTabPanel("Examples of input",
        icon = icon("sliders"),
        miniContentPanel(
          fillCol(
            flex = c(1, 1, 2, 1, 1, 1, 1, 1, 3),
            
            fillRow(
              flex = c(1, 1, 1),
              textInput(
                inputId = "textinput",
                label   = "Text input",
                value   = "manacc"
              ),
              numericInput(
                inputId = "numericinput",
                label   = "Numeric input",
                value   = 5,
                min     = 0,
                max     = 10,
                step    = 1
              ),
              actionButton(
                inputId = "action",
                label   = "Action button",
                width   = "100%",
                icon("filter"),
                style   = "margin-top: 25px; color: #fff; background-color: #337ab7; border-color: #2e6da4"
              )
            ),
            
            tags$hr(),
            
            fillRow(
              flex = c(1, 1, 1),
              checkboxInput(
                inputId = "checkboxinput",
                label   = "Checkbox input",
                value   = FALSE
              ),
              checkboxGroupInput(
                inputId  = "checkboxgroupinput",
                label    = "Checkbox group input",
                choices  = c("Choice 1","Choice 2","Choice 3","Choicee 4"),
                selected = "Choice 1"
              ),
              radioButtons(
                inputId  = "radiobuttoninput",
                label    = "Radio button input",
                choices  = c("Choice 1","Choice 2","Choice 3","Choicee 4"),
                selected = "Choice 1"
              )
            ),
            
            tags$hr(),
            
            fillRow(
              flex = c(1, 1),
              sliderInput(
                inputId = "sliderinput",
                label   = "Slider input",
                value   = 5,
                min     = 0,
                max     = 10,
                step    = 1
              ),
              sliderInput(
                inputId = "sliderrangeinput",
                label   = "Slider range input",
                value   = c(3,7),
                min     = 0,
                max     = 10,
                step    = 1
              )
            ),
            
            tags$hr(),
            
            fillRow(
              flex = c(1, 1),
              dateInput(
                inputId = "dateinput",
                label   = "Date input"
              ),
              dateRangeInput(
                inputId   = "daterangeinput",
                label     = "Date range input",
                start     = "2020-01-01",
                end       = "2020-12-31",
                min       = "2019-01-01",
                max       = "2022-12-21",
                format    = "dd/mm/yy",
                separator = " - "
              )
            ),
            
            tags$hr(),
            
            fillRow(
              flex = c(1, 1, 1),
              selectInput(
                inputId  = "selectinput",
                label    = "Select input",
                choices  = c("File", "Color"),
                selected = "File"
              ),
              conditionalPanel(
                'input.selectinput === "File"',
                fileInput(
                  inputId  = "fileinput",
                  label    = "File input",
                  accept   = c(".csv"),
                  multiple = FALSE
                )
              ),
              conditionalPanel(
                'input.selectinput === "Color"',
                colourInput(
                  inputId = "colorinput",
                  label   = "Color input"
                )
              )
            )
            
          )
        )
      ),
      
      miniTabPanel("Look",
                   icon = icon("eye"),
                   miniContentPanel(
                     uiOutput("htmlpage")
                   )
      ),
      
      miniTabPanel("Sort",
        icon = icon("list-ol"),
        miniContentPanel(
          fillCol(
            flex = c(1,1,1,10),
            uiOutput("filtcut"),
            uiOutput("filtcolor"),
            tags$hr(),
            dataTableOutput("filtered")
          )
        )
      )
    )
  )
  

  server <- function(input, output, session) {

    # Bind variables
    some_variable <- NULL
    
    # Create a list where reactive values can be stored to be later updated through observers
    values <- reactiveValues()
    values$data <- as.data.frame(mutate(ggplot2::diamonds, cut = as.character(cut), color = as.character(color)))
    
    # Use reactive values to create objects updated after other objects (input, reactive values) were changed
    reactive_value <- reactive({})
    
    # Use observers to implement side-effects of changes in reactive values
    observe({ })
    
    # Use renderUI to make under interfaces which are reactive to user input. The following example is a nice application to filter data.
    
    
    
    # Create a first filter
    output$filtcut <- renderUI({
      choices <- c("", sort(unique(values$data$cut), decreasing = FALSE))
      selectInput("slctcut", "Chapter:", choices = choices, selected = "", multiple = FALSE, width = '100%')
    })
    
    afterfiltcut <- reactive({
      filter <- input$slctcut
      if (is.null(filter)){
        values$data
      } else if (filter == "") {
        values$data
      } else {
        dplyr::filter(values$data, str_detect(values$data$cut, filter))
      }
    })
    
    
    output$filtcolor <- renderUI({
      choices <- c("", sort(unique(afterfiltcut()$color), decreasing = FALSE))
      selectInput("slctcolor", "Chapter:", choices = choices, selected = "", multiple = FALSE, width = '100%')
    })
    
    afterfiltcolor <- reactive({
      filter <- input$slctcolor
      if (is.null(filter)){
        afterfiltcut()
      } else if (filter == "") {
        afterfiltcut()
      } else {
        dplyr::filter(afterfiltcut(), str_detect(afterfiltcut()$color, filter))
      }
    })
    
    # The use an output to render the filtered table
    output$filtered <- renderDataTable({ afterfiltcolor() })
    
    
    # use a user interface to display html pages (whether online or in the package itself
    output$htmlpage <- renderUI({
      #file <- paste0(input$slctquest, ".html")
      #address <- system.file("doc", file, package=input$package_name)
      address <- "https://github.com/NicolasJBM"
      withMathJax(includeHTML(address))
    })
    
    # Use observeEvent to make a set of actions dependent on pressing an action button.
    # You can even use progress bars to help the user wait.
    observeEvent(input$action, {
      shiny::withProgress(message = 'Calculation in progress',
                          detail = 'This may take a while...', value = 0, {
                            for (i in 1:15) {
                              incProgress(1/15)
                              Sys.sleep(0.25)
                            }
                          })
    })
    
    
    # List of action to do when exiting
    observeEvent(input$done, {
      
      
      stopApp()
    })
  }
  runGadget(ui, server, viewer = paneViewer(minHeight = "maximize"))
}
