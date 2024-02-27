#' invent_brick UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_invent_brick_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Fetch some matches"),
    p(),
    verbatimTextOutput(outputId = ns("regex")),
    p(),
    actionButton(
      inputId = "fetch_match",
      label = "give me more !",
      icon = icon("circle-plus")
    )
  )
}

#' invent_brick Server Functions
#'
#' @noRd
mod_invent_brick_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(r$regex_combined, {
      # make full regex
      combined_regex <- map_chr(r$regex_list[r$regex_combined], "brick")
      combined_regex <- paste(combined_regex, collapse = "")
      output$regex <- renderText(combined_regex)
    })
  })
}

## To be copied in the UI
# mod_invent_brick_ui("invent_brick_1")

## To be copied in the server
# mod_invent_brick_server("invent_brick_1")
