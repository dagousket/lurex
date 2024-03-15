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
    strong(uiOutput(outputId = ns("regex"))),
    p(),
    uiOutput(outputId = ns("match")),
    p(),
    actionButton(
      inputId = ns("fetch_match"),
      label = "give me more !",
      icon = icon("circle-plus")
    ),
    p(),
    prettySwitch(
      inputId = ns("search_scrabble"),
      label = span(
        "Search word in scrabble",
        tooltip(
          bs_icon("info-circle"),
          "some info here",
          placement = "bottom"
        )
      ),
      status = "info",
      value = TRUE,
      fill = TRUE
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
      helpers <- c("or" = "|", "start" = "^", "end" = "$")

      combined_regex <- map_chr(
        .x = r$regex_combined,
        .f = \(x){
          if (x %in% names(r$regex_list)) {
            r$regex_list[[x]][["brick"]]
          } else {
            helpers[x]
          }
        }
      )

      combined_regex <- color_style_regex(
        combined_regex,
        is_regex = TRUE
      )
      output$regex <- renderUI(combined_regex)

      # clean up invented list
      r$regex_match <- NULL
    })


    observeEvent(
      input$fetch_match,
      {
        if (!is.null(r$is_regex_valid) && r$is_regex_valid) {
          r$regex_match <- tagList(
            generate_from_regex(
              brick_list = r$regex_combined,
              brick_info = r$regex_list,
              use_scrabble = input$search_scrabble
            )
          )
        }

        output$match <- renderUI({
          validate(
            need(r$is_regex_valid, "Your regex is not valid yet")
          )
          r$regex_match
        })
      },
      ignoreInit = TRUE
    )
  })
}

## To be copied in the UI
# mod_invent_brick_ui("invent_brick_1")

## To be copied in the server
# mod_invent_brick_server("invent_brick_1")
