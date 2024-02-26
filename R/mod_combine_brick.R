#' combine_brick UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom sortable bucket_list add_rank_list
#' @importFrom htmltools tags
#' @importFrom bslib tooltip
#' @importFrom bsicons bs_icon
mod_combine_brick_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Order your motifs"),
    bucket_list(
      header = "plop",
      group_name = "brick_bucket",
      orientation = "vertical",
      add_rank_list(
        input_id = "ranked_bricks",
        text = "",
        labels = c("[mot1]", "[mot2]", "[mot3]", "[mot4]"),
        orientation = "horizontal"
      )
    ),
    fluidRow(
      column(
        4,
        bucket_list(
          header = "bin",
          group_name = "brick_bucket",
          add_rank_list(
            input_id = "dumped_bricks",
            text = ""
          )
        )
      ),
      column(
        8,
        bucket_list(
          header = "helpers",
          group_name = "brick_bucket",
          add_rank_list(
            input_id = "helper_bricks",
            text = "",
            labels = list(
              "or" = tags$div(
                "|", tooltip(bs_icon("info-circle"), "or : match the motif on the left or the motif on the right", placement = "bottom")
              ),
              "start" = tags$div(
                "^", tooltip(bs_icon("info-circle"), "start : no text before this", placement = "bottom")
              ),
              "end" = tags$div(
                "$", tooltip(bs_icon("info-circle"), "stop : no text after this", placement = "bottom")
              )
            ),
            orientation = "horizontal"
          )
        )
      )
    ),
    actionButton(
      inputId = "clear_bin",
      label = "empty bin",
      icon = icon("trash")
    )
  )
}

#' combine_brick Server Functions
#'
#' @noRd
mod_combine_brick_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_combine_brick_ui("combine_brick_1")

## To be copied in the server
# mod_combine_brick_server("combine_brick_1")
