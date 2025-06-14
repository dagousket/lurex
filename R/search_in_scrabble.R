#' Search a word from Scrabble with regex
#'
#' @param brick list. A list of the regex specifics, works for unordered regex.
#'
#' @importFrom purrr map map_vec
#' @importFrom stringr str_extract_all
#'
#' @return wordlist. A character string matching the regex.
#'
#' @noRd
#' @examples
#' search_in_scrabble(
#'   list(
#'     brick = "[zulu]{4,4}",
#'     type = "unordered"
#'   )
#' )
#'
#' search_in_scrabble_mem(
#'   list(
#'     brick = "[zulu]{4,4}",
#'     type = "unordered"
#'   )
#' )
search_in_scrabble <- function(brick) {
  if (brick$type != "unordered") {
    stop("scrabble search is only available for unordered motifs")
  }
  wordlist <- lurex::wordlist

  # match full upper or full lower case
  match_all <- grep(
    pattern = paste0("^(", brick$brick, ")$"),
    x = c(wordlist, toupper(wordlist))
  )
  all_matches <- c(wordlist, toupper(wordlist))[match_all]

  universal_case <- all(
    c(
      "lowercase letters",
      "uppercase letters"
    ) %in%
      brick$content
  )

  if ("custom" %in% brick$content || universal_case) {
    # list which letter is found as upper or lower case
    if (universal_case) {
      letters_bucket <- map(.x = letters, .f = \(x) {
        c(x, toupper(x))
      })
    } else {
      letters_bucket <- map_vec(.x = letters, .f = \(x) {
        unique(
          str_extract_all(
            string = brick$custom_motif,
            pattern = paste0(c(x, toupper(x)), collapse = "|")
          )
        )
      })
    }
    names(letters_bucket) <- letters

    # scan wordlist match and pick correct case
    mixed_case_match <- map_chr(.x = all_matches, .f = \(word) {
      paste(
        map(.x = strsplit(word, split = "")[[1]], \(word_letter) {
          sample(letters_bucket[[tolower(word_letter)]], 1)
        }),
        collapse = ""
      )
    })

    all_matches <- c(all_matches, mixed_case_match)
  }

  return(unique(all_matches))
}

#' Search a word from Scrabble with regex with cache
#'
#' @inheritParams search_in_scrabble
#'
#' @importFrom memoise memoise
#'
#' @noRd
search_in_scrabble_mem <- memoise(search_in_scrabble)
