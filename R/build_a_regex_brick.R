#' Build a regex brick
#'
#' @param type character. The type of regex group.
#' Can be "ordered" or "unordered".
#' @param content character. The content of the regex group.
#' Can be "wildcard", "lowercase letters", "uppercase letters", "digits",
#' "punctuation", "space and tab", or "custom".
#' @param occurrence character. The occurrence of the regex group.
#' Can be "once", "at least once", "anytime", or "custom".
#' @param custom_motif character. The custom motif to be used in a regex group.
#' Only applicable if "custom" is selected for the content parameter.
#' @param custom_occurrence integer. The occurrence to be used in a regex group.
#' Only applicable if "custom" is selected for the occurrence parameter.
#' @param escape logical. Should special characters in custom motif be escaped.
#'
#' @importFrom stringr str_escape
#'
#' @return A character string representing the built regex.
#' @export
#'
#' @examples
#' build_a_regex_brick(
#'   type = "ordered",
#'   occurrence = "custom",
#'   custom_motif = "hello$",
#'   custom_occurrence = c(1, 5),
#'   escape = FALSE
#' )
#'
#' build_a_regex_brick(
#'   type = "unordered",
#'   occurrence = "anytime",
#'   custom_motif = "hello!",
#'   content = c("punctuation", "custom")
#' )
build_a_regex_brick <- function(
  type = c("ordered", "unordered"),
  content = c(
    "wildcard",
    "lowercase letters",
    "uppercase letters",
    "digits",
    "punctuation",
    "space and tab",
    "custom"
  ),
  occurrence = c("once", "at least once", "anytime", "custom"),
  custom_motif = NULL,
  custom_occurrence = NULL,
  escape = TRUE
) {
  # check input
  type <- match.arg(type)
  content <- match.arg(content, several.ok = TRUE)
  occurrence <- match.arg(occurrence)

  # check cusom
  if ("custom" %in% occurrence && is.null(custom_occurrence)) {
    stop("custom occurrence selected but no value given")
  }
  if ("custom" %in% content && is.null(custom_motif)) {
    stop("custom motif selected but no value given")
  }

  # escape special character
  unescaped_motif <- custom_motif
  if (!is.null(custom_motif) && isTRUE(escape)) {
    # escape special
    custom_motif <- str_escape(custom_motif)
    # avoid creating range in unordered input with `-`
    if (type == "unordered" && grepl("-", custom_motif)) {
      custom_motif <- paste0("-", gsub("-", "", custom_motif))
    }
  }

  # set occurrence
  if (occurrence != "custom") {
    occurrence_equivalence <- list(
      "once" = "",
      "at least once" = "+",
      "anytime" = "*"
    )
    custom_occurrence <- NULL
    freq <- occurrence_equivalence[occurrence]
  } else {
    freq <- sprintf(
      "{%s,%s}",
      custom_occurrence[1],
      sub("no max", "", custom_occurrence[2])
    )
  }

  # set group
  if (type == "ordered") {
    group_bracket <- c("(", ")")

    # we only allow custom motif for ordered group for now
    text <- sprintf(
      "%s%s%s%s",
      group_bracket[1],
      custom_motif,
      group_bracket[2],
      freq
    )
    # remove unused parameters
    content <- NULL
  } else if (type == "unordered") {
    group_bracket <- c("[", "]")

    # set content
    regex_equivalance <- c(
      "wildcard" = ".",
      "lowercase letters" = "[:lower:]",
      "uppercase letters" = "[:upper:]",
      "digits" = "[:digit:]",
      "punctuation" = "[:punct:]",
      "space and tab" = "[:blank:]",
      "custom" = custom_motif
    )

    text <- sprintf(
      "%s%s%s%s",
      group_bracket[1],
      paste(regex_equivalance[content], collapse = ""),
      group_bracket[2],
      freq
    )
  }

  new_brick <- list(
    brick = text,
    type = type,
    content = content,
    occurrence = occurrence,
    custom_motif = unescaped_motif,
    custom_occurrence = custom_occurrence
  )

  return(new_brick)
}
