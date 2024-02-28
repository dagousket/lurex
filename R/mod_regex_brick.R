#' regex_brick UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets pickerInput prettySwitch sliderTextInput
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
				"occurrence",
				tooltip(
					bs_icon("info-circle"),
					"some info here",
					placement = "bottom"
				)
			),
			choices = c("once", "at least once", "anytime", "custom")
		),
		p(),
		sliderTextInput(
			inputId = ns("occurrence_slider"),
			label = "custom range",
			choices = c(as.character(0:10), "no max"),
			selected = c("1", "2")
		),
		p(),
		prettySwitch(
			inputId = ns("escape_special"),
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
#' @importFrom shinyWidgets show_toast
#' @noRd
mod_regex_brick_server <- function(id, r) {
	moduleServer(id, function(input, output, session) {
		ns <- session$ns

		# hide custom options at start
		golem::invoke_js("hideid", ns("custom_motif"))
		golem::invoke_js("hideid", ns("occurrence_slider"))

		# hiding / showing custom motif entries
		observeEvent(
			input$content_type,
			{
				if ("custom" %in% input$content_type) {
					golem::invoke_js("showid", ns("custom_motif"))
				} else {
					golem::invoke_js("hideid", ns("custom_motif"))
				}
			}
		)

		# hiding / showing custom occurrence entries
		observeEvent(
			input$occurrence,
			{
				if ("custom" %in% input$occurrence) {
					golem::invoke_js("showid", ns("occurrence_slider"))
				} else {
					golem::invoke_js("hideid", ns("occurrence_slider"))
				}
			}
		)


		# making a brick
		observeEvent(
			input$make_brick,
			{
				brick_number <- as.character(input$make_brick)
				new_brick <- build_a_regex_brick(
					type = input$motif_type,
					content = input$content_type,
					occurrence = input$occurrence,
					custom_motif = input$custom_motif,
					custom_occurrence = input$occurrence_slider,
					escape = input$escape_special
				)
				r$regex_list[brick_number] <- list(new_brick)
				show_toast(
					title = "new regex",
					text = new_brick$brick,
					type = "success",
					timer = 3000,
					width = "400px"
				)
			}
		)
	})
}

## To be copied in the UI
# mod_regex_brick_ui("regex_brick_1")

## To be copied in the server
# mod_regex_brick_server("regex_brick_1")
