test_that("function returns regex with `or` helper", {
  brick_list <- c("start", "1", "end", "or", "start", "2", "end")

  brick_info <- list(
    `1` = list(
      brick = "[[:punct:]hello!]*",
      type = "unordered",
      content = c("punctuation", "custom"),
      occurrence = "anytime",
      custom_motif = "hello!",
      custom_occurrence = NULL
    ),
    `2` = list(
      brick = "(what's up \\?)",
      type = "ordered",
      content = NULL,
      occurrence = "once",
      custom_motif = "what's up ?",
      custom_occurrence = NULL
    )
  )

  n_to_test <- 12
  res <- generate_from_regex(
    brick_list = brick_list,
    brick_info = brick_info,
    n = n_to_test
  )

  expect_type(object = res, type = "list")
  expect_s3_class(object = res[[1]], class = "shiny.tag")
})

test_that("function returns regex without `or` helper", {
  brick_list <- c("start", "1", "2", "end")

  brick_info <- list(
    `1` = list(
      brick = "[[:punct:]hello!]*",
      type = "unordered",
      content = c("punctuation", "custom"),
      occurrence = "anytime",
      custom_motif = "hello!",
      custom_occurrence = NULL
    ),
    `2` = list(
      brick = "(what's up \\?)",
      type = "ordered",
      content = NULL,
      occurrence = "once",
      custom_motif = "what's up ?",
      custom_occurrence = NULL
    )
  )

  n_to_test <- 12
  res <- generate_from_regex(
    brick_list = brick_list,
    brick_info = brick_info,
    n = n_to_test
  )

  expect_type(object = res, type = "list")
  expect_s3_class(object = res[[1]], class = "shiny.tag")
})
