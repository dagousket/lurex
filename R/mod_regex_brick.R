#' regex_brick UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets pickerInput
#' @importFrom bslib tooltip
#' @importFrom bsicons bs_icon
mod_regex_brick_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Construct a regex brick"),
    p(),
    selectInput(
      inputId = "motif_type",
      label = span(
        "type of text",
        tooltip(
          bs_icon("info-circle"),
          "some info here",
          placement = "bottom"
        )
      ),
      choices = c("unordered", "ordered")
    ),
    p(),
    selectInput(
      inputId = "content_type",
      label = span(
        "type of content",
        tooltip(
          bs_icon("info-circle"),
          "some info here",
          placement = "bottom"
        )
      ),
      choices = c("wildcard", "character", "digits", "punctuations", "custom")
    ),
    p(),
    textInput(
      inputId = "custom_text",
      label = span(
        "custom motif",
        tooltip(
          bs_icon("info-circle"),
          "some info here",
          placement = "bottom"
        )
      ),
      value = "write your motif here"
    ),
    p(),
    selectInput(
      inputId = "occurrence",
      label = span(
        "occurence",
        tooltip(
          bs_icon("info-circle"),
          "some info here",
          placement = "bottom"
        )
      ),
      choices = c("once", "at least once", "anytime", "custom range")
    ),
    p(),
    sliderInput(
      inputId = "occurence_slider",
      label = "custom range",
      min = 0,
      max = 10,
      value = c(1, 2),
      ticks = FALSE,
      step = 1,
      round = TRUE
    ),
    p(),
    actionButton(
      inputId = "make_brick",
      label = "create",
      icon = icon("circle-plus")
    )
  )
}

#' regex_brick Server Functions
#'
#' @noRd
mod_regex_brick_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_regex_brick_ui("regex_brick_1")

## To be copied in the server
# mod_regex_brick_server("regex_brick_1")
