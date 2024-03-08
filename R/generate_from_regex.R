# WARNING - Generated by {fusen} from dev/flat_word_generator.Rmd: do not edit by hand

#' List possible matches for a regex composed of multiple bricks
#'
#' @param brick_list list. A list of bricks.
#' @param n integer. The number of match to return.
#' @param n_bound integer. The number of character to add around match.
#' This will be aded if no start/end helper is detected.
#' @param use_scrabble logical. Should we first look for match in scrabble.
#'
#' @return matchlist. A character string matching the regex.
#'
#' @noRd
#' @examples
#' brick_list <- c("start", "1", "end")
#'
#' brick_info <- list(
#' 	`1` = build_a_regex_brick(
#' 		type = "unordered",
#' 		content = c("digits", "punctuation"),
#' 		occurrence = "custom",
#' 		custom_motif = "",
#' 		custom_occurrence = c("6", "no max")
#' 	)
#' )
#'
#' generate_from_regex(
#' 	brick_list = brick_list,
#' 	brick_info = brick_info
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

		# bind them into final list of matches
		list_of_n_in_group <- map_chr(
			.x = 1:n_in_group,
			.f = \(x){
				paste0(
					c(
						char_before[x],
						map_vec(char_inside, x),
						char_after[x]
					),
					collapse = ""
				)
			}
		)

		list_of_n <- c(list_of_n, list_of_n_in_group)
	}
	return(sample(list_of_n))
}