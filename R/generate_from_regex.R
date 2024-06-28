
#' List possible matches for a regex composed of multiple bricks
#'
#' @param brick_list list. A list of bricks.
#' @param n integer. The number of match to return.
#' @param n_bound integer. The number of character to add around match.
#' This will be aded if no start/end helper is detected.
#' @param use_scrabble logical. Should we first look for match in scrabble.
#'
#' @importFrom shiny tagList
#'
#' @return matchlist. A character string matching the regex.
#'
#' @noRd
#' @examples
#' brick_list <- c("start", "1", "end")
#'
#' brick_info <- list(
#'   `1` = build_a_regex_brick(
#'     type = "unordered",
#'     content = c("digits", "punctuation"),
#'     occurrence = "custom",
#'     custom_motif = "",
#'     custom_occurrence = c("6", "no max")
#'   )
#' )
#'
#' generate_from_regex(
#'   brick_list = brick_list,
#'   brick_info = brick_info
#' )
generate_from_regex <- function(
  brick_list,
  brick_info,
  n = 10,
  n_bound = 5,
  use_scrabble = TRUE
) {
  # verify presence of `or` groups
  if ("or" %in% brick_list) {
    # extract groups
    or_idx <- which(brick_list == "or")
    regex_groups <- findInterval(
      seq_along(brick_list),
      or_idx
    )
    # clean `or` from lists
    brick_list <- brick_list[-or_idx]
    regex_groups <- regex_groups[-or_idx]
  } else {
    regex_groups <- rep(1, length(brick_list))
  }

  # create by group
  groups_id <- unique(regex_groups)
  n_in_group <- ceiling(n / length(groups_id))

  list_of_n <- c()
  for (id in groups_id) {
    # verify presence of `start`/`end` helpers
    all_characters <- unlist(strsplit(rawToChar(as.raw(32:126)), ""))
    group_bricks <- brick_list[regex_groups == id]

    if (group_bricks[1] == "start") {
      char_before <- rep("", n_in_group)
      group_bricks <- group_bricks[-1]
    } else {
      char_before <- replicate(
        n_in_group,
        paste0(sample(all_characters, n_bound), collapse = "")
      )
    }

    if (group_bricks[length(group_bricks)] == "end") {
      group_bricks <- group_bricks[-length(group_bricks)]
      char_after <- rep("", n_in_group)
    } else {
      char_after <- replicate(
        n_in_group,
        paste0(sample(all_characters, n_bound), collapse = "")
      )
    }

    # generate each regex brick with maxword = n / (number of group)
    char_inside <- map(
      brick_info[group_bricks],
      \(x) {
        generate_from_brick(
          x,
          maxword = n_in_group,
          use_scrabble = use_scrabble
        )
      }
    )

    # col idx ?
    # how much brick before, except and/or/start
    col_idx <- min(which(regex_groups == id))
    if (col_idx > 1) {
      helper_before_idx <- sum(
        brick_list[1:col_idx - 1] %in% c("start", "end", "or")
      )
      col_idx <- col_idx - helper_before_idx
    }


    # bind them into final list of matches
    list_of_n_in_group <- color_style_regex(
      char_inside = char_inside,
      char_before = char_before,
      char_after = char_after,
      col_idx = col_idx
    )

    list_of_n <- c(list_of_n, list_of_n_in_group)
  }
  return(sample(list_of_n))
}

#' Style regex with colors
#'
#' @param char_inside character. A vector of regex match
#' if it contains helpers, helpers will be color in black
#' @param char_before character. A vector of random character before regex
#' @param char_after character. A vector of random character after regex
#' @param col_idx integer. A index to start the color vector from
#' @param is_regex logical. Take into account regex characters as black
#'
#' @importFrom purrr map_dbl map map_vec
#' @importFrom shiny tagList span
#'
#' @noRd
color_style_regex <- function(
  char_inside,
  char_before = NULL,
  char_after = NULL,
  col_idx = 1,
  is_regex = FALSE
) {
  # check if regex is empty
  if (length(char_inside) == 0) {
    return(span(""))
  }

  # check size of vector
  if (is_regex) {
    vec_size <- 1
  } else {
    vec_size <- unique(map_dbl(char_inside, length))
  }

  # initialise palette
  color_pal_size <- length(char_inside)
  col_pal <- rep(lurex::color_pal, length.out = col_idx + color_pal_size)
  col_pal <- col_pal[col_idx:(col_idx + color_pal_size)]

  # style inner character (regex) with pkg color palette
  char_inside_with_style <- map(1:vec_size, .f = \(x) {
    # parse content (in multiple piece if match)
    if (!is_regex) {
      content <- map_vec(char_inside, x)
    } else {
      content <- char_inside
    }

    # apply color to each element, skip helper with black color
    nb_helper <- 0
    content_style <- list()
    for (idx in seq_along(content)) {
      if (is_regex && content[idx] %in% c("^", "|", "$")) {
        col_style <- "color:black"
        nb_helper <- nb_helper + 1
      } else {
        col_style <- paste0("color:", col_pal[idx - nb_helper])
      }
      content_style[[idx]] <- span(
        content[idx],
        style = col_style,
        .noWS = "outside"
      )
    }
    return(content_style)
  })

  # add grey boundary if provided
  if (!is_regex) {
    vec_boundary_size <- unique(length(char_before), length(char_after))

    if (length(vec_size) != 1 || vec_size != vec_boundary_size) {
      stop("The provided character vectors do not have the same length")
    }

    # bind them into final list of matches with grey extra boundaries
    list_of_n_in_group <- map(
      .x = 1:vec_size,
      .f = \(x) {
        p(
          span(char_before[x], style = "color:lightgrey", .noWS = "outside"),
          char_inside_with_style[[x]],
          span(char_after[x], style = "color:lightgrey", .noWS = "outside")
        )
      }
    )
    return(list_of_n_in_group)
  } else {
    return(tagList(char_inside_with_style[[1]]))
  }
}
