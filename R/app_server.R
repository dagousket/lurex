#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
	# Your application server logic
	mod_regex_brick_server("regex_brick_1")
	mod_combine_brick_server("combine_brick_1")
	mod_invent_brick_server("invent_brick_1")
}
