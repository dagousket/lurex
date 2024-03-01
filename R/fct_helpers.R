#' @noRd
one_nav <- function(
	id,
	text
) {
	sprintf(
		'<li class="nav-item"><a class="nav-link js-scroll-trigger" href="#%s">%s</a></li>',
		id,
		text
	) |> HTML()
}

one_section <- function(
	id,
	...
) {
	tagList(
		tags$section(
			class = "resume-section",
			id = id,
			tags$div(
				class = "resume-section-content",
				...
			)
		),
		tags$hr(class = "m-0")
	)
}
