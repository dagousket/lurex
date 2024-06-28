test_that("function returns error for unexpected input", {
  expect_error(
    search_in_scrabble(list("type" = "incorrect")),
    regexp = "scrabble search is only available for unordered motifs"
  )
})

test_that("function returns all matches for correct input", {
  res <- search_in_scrabble(
    list(
      brick = "[zulu]{4,4}",
      type = "unordered"
    )
  )

  expect_setequal(object = res, expected = c("lull", "lulu", "zulu"))
})
