#' regex_brick UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets pickerInput prettySwitch
#' @importFrom bslib tooltip
#' @importFrom bsicons bs_icon
mod_regex_brick_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Construct a regex brick"),
    p(),
    selectInput(
      inputId = ns("motif_type"),
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
      inputId = ns("content_type"),
      label = span(
        "type of content",
        tooltip(
          bs_icon("info-circle"),
          "some info here",
          placement = "bottom"
        )
      ),
      choices = c(
        "wildcard",
        "lowercase letters",
        "uppercase letters",
        "digits",
        "punctuation",
        "space and tab",
        "custom"
      ),
      multiple = TRUE
    ),
    p(),
    textInput(
      inputId = ns("custom_motif"),
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
      inputId = ns("occurrence"),
      label = span(
        "occurence",
        tooltip(
          bs_icon("info-circle"),
          "some info here",
          placement = "bottom"
        )
      ),
      choices = c("once", "at least once", "anytime", "custom")
    ),
    p(),
    sliderInput(
      inputId = ns("occurence_slider"),
      label = "custom range",
      min = 0,
      max = 10,
      value = c(1, 2),
      ticks = FALSE,
      step = 1,
      round = TRUE
    ),
    p(),
    prettySwitch(
      inputId = "escape_special",
      label = span(
        "Escape special character",
        tooltip(
          bs_icon("info-circle"),
          "some info here",
          placement = "bottom"
        )
      ),
      status = "info",
      value = TRUE,
      fill = TRUE
    ),
    p(),
    actionButton(
      inputId = ns("make_brick"),
      label = "create",
      icon = icon("circle-plus")
    )
  )
}

#' regex_brick Server Functions
#'
#' @noRd
mod_regex_brick_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    golem::invoke_js("hideid", ns("custom_motif"))

    # hiding / showing custom entries
    observeEvent(
      input$content_type,
      {
        if ("custom" %in% input$content_type) {
          golem::invoke_js("hideid", ns("custom_motif"))
        } else {
          golem::invoke_js("showid", ns("custom_motif"))
        }
      }
    )

    # making a brick
    observeEvent(
      input$make_brick,
      {
        new_brick <- build_a_regex_brick(
          type = input$motif_type,
          content = input$content_type,
          occurence = input$occurrence,
          custom_motif = input$custom_motif,
          custom_occurrence = input$occurence_slider
        )
        r$regex_list <- append(r$regex_list, new_brick)
      }
    )
  })
}

## To be copied in the UI
# mod_regex_brick_ui("regex_brick_1")

## To be copied in the server
# mod_regex_brick_server("regex_brick_1")
