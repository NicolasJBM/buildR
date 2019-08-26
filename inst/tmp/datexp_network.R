#' Gadget for the interpretation of topic models.
#' @param x Tibble. Term, topic, and beta as return by topmod_extract.
#' @param y Tibble. Document, topic, and gamma as return by topmod_extract.
#' @import shiny
#' @import dplyr
#' @import miniUI
#' @import stringr
#' @export


topmod_interpret <- function(x,y) {
  
  ui <- miniPage(
    gadgetTitleBar("Topics similarity"),
    miniContentPanel(
      miniTabstripPanel(
        miniTabPanel("Word-based", icon = incon(""),
                     fillRow(
                       flex = c(3,1)
                     )
        ),
        miniTabPanel("Document-based", icon = incon(""),
                     fillRow(
                       flex = c(3,1)
                       
                     )
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    
    
    #################
    # On exit
    
    observeEvent(input$done, {
      stopApp()
    })
  }
  runGadget(ui, server)
}
