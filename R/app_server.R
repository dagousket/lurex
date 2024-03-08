#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
	# Your application server logic

	# the flying regex list
	r <- reactiveValues(
		regex_list = list(),
		is_regex_valid = NULL,
		regex_combined = NULL,
		regex_match = NULL
	)

	mod_regex_brick_server("regex_brick_1", r = r)
	mod_combine_brick_server("combine_brick_1", r = r)
	mod_invent_brick_server("invent_brick_1", r = r)
}
