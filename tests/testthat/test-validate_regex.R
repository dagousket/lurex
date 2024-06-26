test_that("function returns TRUE for correct regex", {
  expect_true({
    validate_regex(bricks = list("yes", "or", "no"))[[1]]
  })
  expect_true({
    validate_regex(
      bricks = list("1", "end", "or", "2", "end", "or", "3", "4", "end")
    )[[1]]
  })
})

test_that("function returns FALSE for incorrect regex", {
  expect_false({
    validate_regex(
      bricks = list("or", "yes", "no"),
      use_toast = FALSE
    )[[1]]
  })
  expect_false({
    validate_regex(
      bricks = list("start", "or", "1", "2", "end"),
      use_toast = FALSE
    )[[1]]
  })
})
