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
mod_regex_brick_ui <- function(id) {
	ns <- NS(id)
	tagList(
		h3("Construct a regex brick"),
		pickerInput(
			inputId = "motif_type",
			label = "Type of motif",
			choices = c("unordered", "ordered")
		),
		selectInput(
			inputId = "content_type",
			label = "Type of content",
			choices = c("character", "digits", "punctuations")
		),
		textInput(
			inputId = "custom_text",
			label = "custom motif",
			value = "write your motif here"
		),
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
