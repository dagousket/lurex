#' regex_brick UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList selectInput textInput actionButton
#' @importFrom shinyWidgets prettySwitch sliderTextInput
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
					"does the order of the characters matter ?",
					placement = "bottom"
				)
			),
			choices = c("unordered", "ordered")
		),
		p(),
		span(
			selectInput(
				inputId = ns("content_type"),
				label = span(
					"type of content",
					tooltip(
						bs_icon("info-circle"),
						"which type of character are you looking for ?",
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
			id = ns("content_span")
		),
		p(),
		span(
			textInput(
				inputId = ns("custom_motif"),
				label = span(
					"custom motif",
					tooltip(
						bs_icon("info-circle"),
						"include a specific set of word/character to search for",
						placement = "bottom"
					)
				),
				placeholder = "write your motif here"
			),
			id = ns("custommotif_span")
		),
		p(),
		selectInput(
			inputId = ns("occurrence"),
			label = span(
				"occurrence",
				tooltip(
					bs_icon("info-circle"),
					"how many times should this motif be detected ?",
					placement = "bottom"
				)
			),
			choices = c("once", "at least once", "anytime", "custom")
		),
		p(),
		span(
			sliderTextInput(
				inputId = ns("occurrence_slider"),
				label = span(
					"custom range",
					tooltip(
						bs_icon("info-circle"),
						"what are the min/max occurrence boundaries ?",
						placement = "bottom"
					)
				),
				choices = c(as.character(0:10), "no max"),
				selected = c("1", "2")
			),
			id = ns("customrange_span")
		),
		p(),
		span(
			prettySwitch(
				inputId = ns("escape_special"),
				label = span(
					"Escape special character",
					tooltip(
						bs_icon("info-circle"),
						"treat custom motif as raw text without regex interpreatation",
						placement = "bottom"
					)
				),
				status = "info",
				value = TRUE,
				fill = TRUE
			),
			id = ns("escape_span")
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
		golem::invoke_js("hideid", ns("custommotif_span"))
		golem::invoke_js("hideid", ns("customrange_span"))
		golem::invoke_js("hideid", ns("content_span"))
		golem::invoke_js("hideid", ns("escape_span"))

		# hiding / showing content type entries
		observeEvent(
			input$motif_type,
			{
				if (input$motif_type == "unordered") {
					golem::invoke_js("showid", ns("content_span"))
				} else if (input$motif_type == "ordered") {
					golem::invoke_js("hideid", ns("content_span"))
					golem::invoke_js("showid", ns("custommotif_span"))
					golem::invoke_js("showid", ns("escape_span"))
				}
			}
		)

		# hiding / showing custom motif entries
		observeEvent(
			input$content_type,
			{
				if ("custom" %in% input$content_type) {
					golem::invoke_js("showid", ns("custommotif_span"))
					golem::invoke_js("showid", ns("escape_span"))
				} else {
					golem::invoke_js("hideid", ns("custommotif_span"))
					golem::invoke_js("hideid", ns("escape_span"))
				}
			}
		)

		# hiding / showing custom occurrence entries
		observeEvent(
			input$occurrence,
			{
				if ("custom" %in% input$occurrence) {
					golem::invoke_js("showid", ns("customrange_span"))
				} else {
					golem::invoke_js("hideid", ns("customrange_span"))
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
				Sys.sleep(1)
			}
		)
	})
}

## To be copied in the UI
# mod_regex_brick_ui("regex_brick_1")

## To be copied in the server
# mod_regex_brick_server("regex_brick_1")
