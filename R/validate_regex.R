#' Validate a regex combination
#'
#' @param bricks character. The ordered list of regex bricks
#' @param use_toast logical. Should a UI toast be shown for incorrect regex.
#'
#' @return list. logical indicating validity, character indicating reasons
#' @export
#'
#' @examples
#' validate_regex(
#'   bricks = list("1", "end", "or", "2", "end", "or", "3", "4", "end")
#' )
validate_regex <- function(
  bricks,
  use_toast = TRUE
) {
  # get idx
  or_idx <- which(bricks == "or")
  start_idx <- which(bricks == "start")
  end_idx <- which(bricks == "end")

  valid <- TRUE
  text <- ""

  if (all(bricks %in% c("start", "end", "stop"))) {
    valid <- FALSE
    text <- "no brick in regex"
    use_toast <- FALSE
  }

  if ("start" %in% bricks) {
    # start can be found as 1st element or after a "or"
    if (!all(start_idx %in% c(1, or_idx + 1))) {
      valid <- FALSE
      text <- "start `^` should be the first element"
    }
  }

  if ("end" %in% bricks) {
    if (!all(end_idx %in% c(length(bricks), or_idx - 1))) {
      valid <- FALSE
      text <- "end `$` should be the last element"
    }
  }

  if ("or" %in% bricks) {
    is_first_or_last <- any(c(1, length(bricks)) %in% or_idx)

    if (is_first_or_last) {
      # cannot be first or last
      valid <- FALSE
      text <- "or `|` should not be 1st or last element"
    }

    right_idx <- or_idx + 1[or_idx != length(bricks)]
    left_idx <- or_idx - 1[or_idx != 1]

    is_next_to_or <- any(
      c("or") %in% bricks[c(left_idx, right_idx)]
    )
    is_right_to_start <- any(
      "start" %in% bricks[left_idx]
    )
    is_left_to_end <- any(
      "end" %in% bricks[right_idx]
    )

    if (is_next_to_or) {
      text <- "or `|` should not be next to another `|`"
      valid <- FALSE
    }

    if (is_right_to_start) {
      text <- "or `|` should not be directly after start `^`"
      valid <- FALSE
    }

    if (is_left_to_end) {
      text <- "or `|` should not be directly before end `$`"
      valid <- FALSE
    }
  }
  if (isFALSE(valid)) {
    make_a_toast(
      text = text,
      use_toast = use_toast
    )
  }

  return(list(valid, text))
}

#' make a toast
#'
#' @param text character. The text to sho
#' @inheritParams validate_regex use_toast
#'
#' @importFrom shinyWidgets show_toast
#'
#' @noRd
make_a_toast <- function(text, use_toast) {
  if (isTRUE(use_toast)) {
    show_toast(
      title = "woups",
      text = text,
      type = "warning",
      timer = 3000,
      width = "400px",
      position = "top-end"
    )
  }
}
