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
    uiOutput(ns("sortable_1")),
    fluidRow(
      column(
        4,
        uiOutput(ns("sortable_2"))
      ),
      column(
        8,
        uiOutput(ns("sortable_3"))
      )
    ),
    actionButton(
      inputId = ns("clear_bin"),
      label = "empty bin",
      icon = icon("trash")
    )
  )
}

#' combine_brick Server Functions
#'
#' @noRd
mod_combine_brick_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # the main bucket
    observeEvent(r$regex_list, {
      output$sortable_1 <- renderUI({
        bucket_list(
          header = "plop",
          group_name = "brick_bucket",
          orientation = "vertical",
          add_rank_list(
            input_id = ns("ranked_bricks"),
            text = "",
            labels = r$regex_list,
            orientation = "horizontal"
          )
        )
      })
    })

    observeEvent(input$ranked_bricks, {
      # check validity of the regex
      r$is_regex_valid <- validate_regex(input$ranked_bricks)
      # save regex
      r$regex_combined <- input$ranked_bricks
    })

    # the bin bucket
    observeEvent(input$clear_bin,
      {
        output$sortable_2 <- renderUI({
          bucket_list(
            header = "bin",
            group_name = "brick_bucket",
            add_rank_list(
              input_id = ns("dumped_bricks"),
              text = ""
            )
          )
        })
      },
      ignoreNULL = FALSE
    )

    # the helper bucket
    observeEvent(input$helper_bricks,
      {
        # no more start/end if already in use
        bricks_list <- list(
          "or" = tags$div(
            "|", tooltip(bs_icon("info-circle"), "or : match the motif on the left or the motif on the right", placement = "bottom")
          ),
          "start" = tags$div(
            "^", tooltip(bs_icon("info-circle"), "start : no text before this", placement = "bottom")
          ),
          "end" = tags$div(
            "$", tooltip(bs_icon("info-circle"), "stop : no text after this", placement = "bottom")
          )
        )

        unused <- c("start", "end")[!c("start", "end") %in% input$ranked_bricks]
        bricks_list <- bricks_list[c(unused, "or")]

        output$sortable_3 <- renderUI({
          bucket_list(
            header = "helpers",
            group_name = "brick_bucket",
            add_rank_list(
              input_id = ns("helper_bricks"),
              text = "",
              labels = bricks_list,
              orientation = "horizontal"
            )
          )
        })
      },
      ignoreNULL = FALSE
    )
  })
}

## To be copied in the UI
# mod_combine_brick_ui("combine_brick_1")

## To be copied in the server
# mod_combine_brick_server("combine_brick_1")
