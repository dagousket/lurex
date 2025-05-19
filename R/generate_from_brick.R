#' Generate a word from a regex brick
#'
#' @param brick list. A list of the regex specifics
#' @param use_scrabble logical. Should the regex try to match a real word
#' @param maxword integer. Maximum number of random word to return
#' @param maxfreq integer. Boundary when no maximum occurrence is specified
#'
#' @importFrom purrr map_chr
#'
#' @return wordlist. A character string matching the regex.
#' @export
#'
#' @examples
#' brick <- build_a_regex_brick(
#'   type = "ordered",
#'   occurrence = "custom",
#'   custom_motif = "heyho",
#'   content = NULL,
#'   custom_occurrence = c(5, 10)
#' )
#'
#' generate_from_brick(brick)
#'
#' brick <- build_a_regex_brick(
#'   type = "unordered",
#'   occurrence = "at least once",
#'   custom_motif = "write your motif here",
#'   content = c("digit")
#' )
#'
#' generate_from_brick(brick)
#'
#' brick <- build_a_regex_brick(
#'   type = "unordered",
#'   occurrence = "anytime",
#'   custom_motif = "WonDERFul",
#'   content = c("custom")
#' )
#'
#' generate_from_brick(brick)
generate_from_brick <- function(
  brick,
  use_scrabble = TRUE,
  maxword = 10,
  maxfreq = 10
) {
  # verify brick list
  brick_items <- c(
    "brick",
    "type",
    "content",
    "occurrence",
    "custom_motif",
    "custom_occurrence"
  )

  if (!all(brick_items %in% names(brick))) {
    stop("brick input list does not have all required items")
  }

  # convert no max if custom range is used
  if (brick$occurrence == "custom") {
    up_bound <- sub(
      "no max",
      maxfreq,
      brick$custom_occurrence[2]
    )
    brick$custom_occurrence <- c(
      as.numeric(brick$custom_occurrence[1]),
      as.numeric(up_bound)
    )
  } else {
    brick$custom_occurrence <- c(0, 0)
  }

  if (brick$type == "ordered") {
    # repeat motif accoring to occurrence
    occurrence_equivalence <- list(
      "once" = 1,
      "at least once" = 1:maxfreq,
      "anytime" = 0:maxfreq,
      "custom" = brick$custom_occurrence[1]:brick$custom_occurrence[2]
    )
    motif_reps <- occurrence_equivalence[[brick$occurrence]]

    invented_match <- map_chr(motif_reps, \(x) {
      paste(rep(brick$custom_motif, x), collapse = "")
    })

    use_duplicates <- length(invented_match) < maxword
    invented_match <- sample(
      invented_match,
      size = maxword,
      replace = use_duplicates
    )

    return(invented_match)
  } else if (brick$type == "unordered") {
    some_letters_in_regex <- any(
      c(
        "lowercase letters",
        "uppercase letters",
        "custom"
      ) %in%
        brick$content
    )
    if (use_scrabble && some_letters_in_regex) {
      scrabble_match <- search_in_scrabble_mem(brick)
    } else {
      scrabble_match <- c()
    }

    # generate from sampling
    extra_word_to_generate <- max(0, maxword - length(scrabble_match))
    if (extra_word_to_generate == 0) {
      return(sample(scrabble_match, maxword))
    } else {
      all_characters <- unlist(strsplit(rawToChar(as.raw(32:126)), ""))
      char_bucket <- list(
        "lowercase letters" = letters,
        "uppercase letters" = LETTERS,
        "digits" = 0:9,
        "punctuation" = grep("[[:punct:]]", all_characters, value = TRUE),
        "space and tab" = grep("[[:blank:]]", all_characters, value = TRUE),
        "wildcard" = grep(".", all_characters, value = TRUE),
        "custom" = strsplit(brick$custom_motif, "")
      )
      char_bucket <- unique(unlist(char_bucket[brick$content]))

      # complete scrabble list up to max_word
      if (brick$occurrence == "once") {
        repeats <- rep(1, extra_word_to_generate)
      } else if (brick$occurrence != "custom") {
        start_bound <- ifelse(
          brick$occurrence == "at least once",
          1,
          0
        )
        repeats <- sample(
          start_bound:maxfreq,
          size = extra_word_to_generate,
          replace = TRUE
        )
      } else if (brick$occurrence == "custom") {
        sample_range <- seq.int(
          from = as.numeric(brick$custom_occurrence[1]),
          to = min(c(maxfreq, as.numeric(brick$custom_occurrence[2])))
        )
        if (length(sample_range) == 1) {
          repeats <- rep(
            sample_range,
            extra_word_to_generate
          )
        } else {
          repeats <- sample(
            sample_range,
            size = extra_word_to_generate,
            replace = TRUE
          )
        }
      }
      invented_match <- map_chr(
        .x = repeats,
        .f = \(x) {
          paste0(
            sample(
              x = char_bucket,
              size = x,
              replace = TRUE
            ),
            collapse = ""
          )
        }
      )
      return(c(scrabble_match, invented_match))
    }
  }
}
