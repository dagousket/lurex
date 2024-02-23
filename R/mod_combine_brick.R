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
			)
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
