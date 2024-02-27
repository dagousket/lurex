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
        p("bin"),
        uiOutput(ns("sortable_2")),
        actionButton(
          inputId = ns("clear_bin"),
          label = "empty bin",
          icon = icon("trash")
        )
      ),
      column(
        8,
        span("helpers", tooltip(
          bs_icon("info-circle"),
          "hover on tile to see definition",
          placement = "bottom"
        )),
        uiOutput(ns("sortable_3"))
      )
    )
  )
}

#' combine_brick Server Functions
#'
#' @importFrom purrr map_chr
#'
#' @noRd
mod_combine_brick_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # the main bucket
    observeEvent(r$regex_list, {
      # extract only string from list
      brick_labels <- setNames(
        as.list(map_chr(r$regex_list, "brick")),
        names(r$regex_list)
      )

      output$sortable_1 <- renderUI({
        bucket_list(
          header = NULL,
          group_name = "brick_bucket",
          orientation = "vertical",
          add_rank_list(
            input_id = ns("ranked_bricks"),
            text = NULL,
            labels = brick_labels,
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
    observeEvent(
      input$clear_bin,
      {
        # drop items from list
        dropped_items <- names(r$regex_list) %in% input$dumped_bricks
        r$regex_list <- r$regex_list[!dropped_items]

        # reset bin values
        output$sortable_2 <- renderUI({
          bucket_list(
            header = NULL,
            group_name = "brick_bucket",
            add_rank_list(
              input_id = ns("dumped_bricks"),
              text = NULL
            )
          )
        })
      },
      ignoreNULL = FALSE
    )

    # the helper bucket
    observeEvent(
      c(input$helper_bricks, input$clear_bin),
      {
        # no more start/end if already in use
        bricks_list <- list(
          "or" = tags$div(
            tooltip(
              "|",
              "or : match the motif on the left or the motif on the right",
              placement = "bottom"
            )
          ),
          "start" = tags$div(
            tooltip(
              "^",
              "start : no text before this",
              placement = "bottom"
            )
          ),
          "end" = tags$div(
            tooltip(
              "$",
              "stop : no text after this",
              placement = "bottom"
            )
          )
        )

        unused <- c("start", "end")[!c("start", "end") %in% input$ranked_bricks]
        bricks_list <- bricks_list[c(unused, "or")]

        output$sortable_3 <- renderUI({
          bucket_list(
            header = NULL,
            group_name = "brick_bucket",
            add_rank_list(
              input_id = ns("helper_bricks"),
              text = NULL,
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
