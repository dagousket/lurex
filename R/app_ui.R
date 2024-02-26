#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    htmlTemplate(
      app_sys("app/www/index.html"),
      navs = list(
        one_nav(
          id = "make_brick",
          text = "create"
        ),
        one_nav(
          id = "order_bricks",
          text = "combine"
        ),
        one_nav(
          id = "invent_bricks",
          text = "invent"
        )
      ),
      sections = list(
        one_section(
          id = "make_brick",
          mod_regex_brick_ui("regex_brick_1")
        ),
        one_section(
          id = "order_bricks",
          mod_combine_brick_ui("combine_brick_1")
        ),
        one_section(
          id = "invent_bricks",
          mod_invent_brick_ui("invent_brick_1")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "lurex"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
