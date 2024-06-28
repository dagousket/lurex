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
      col_4(
        p("bin"),
        uiOutput(ns("sortable_2")),
        actionButton(
          inputId = ns("clear_bin"),
          label = "empty bin",
          icon = icon("trash")
        )
      ),
      col_4(
        span(
          "helpers",
          tooltip(
            bs_icon("info-circle"),
            "hover on tile to see definition",
            placement = "bottom"
          )
        ),
        uiOutput(ns("sortable_3"))
      ),
      col_4(
        p("status"),
        uiOutput(outputId = ns("regex_status")),
        textOutput(ns("regex_reason"))
      )
    )
  )
}

#' combine_brick Server Functions
#'
#' @importFrom purrr map_chr
#' @importFrom stats setNames
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

      # re-insert helpers if present
      if (any(c("or", "start", "end") %in% input$ranked_bricks)) {
        helper_brick <- c("or" = "|", "start" = "^", "end" = "$")
        current_list <- input$ranked_bricks
        current_bricks <- c(brick_labels, helper_brick)[current_list]
        new_bricks <- brick_labels[!names(brick_labels) %in% current_list]
        brick_labels <- c(current_bricks, new_bricks)
      }

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
      regex_eval <- validate_regex(input$ranked_bricks)
      r$is_regex_valid <- regex_eval[[1]]
      # save regex
      r$regex_combined <- input$ranked_bricks
      # clean up invented list
      r$regex_match <- NULL

      if (isTRUE(regex_eval[[1]])) {
        eval_status <- "valid"
        eval_icon <- "check2-circle"
        eval_style <- "color:green"
      } else {
        eval_status <- "invalid"
        eval_icon <- "bug"
        eval_style <- "color:red"
      }

      output$regex_status <- renderUI({
        span(
          eval_status,
          bs_icon(eval_icon),
          style = eval_style
        )
      })

      output$regex_reason <- renderText(regex_eval[[2]])
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
