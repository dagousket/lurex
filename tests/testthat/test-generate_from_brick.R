test_that("function returns error for unexpected input", {
  expect_error(
    {
      generate_from_brick(brick = list())
    },
    regexp = "brick input list does not have all required items"
  )
})

test_that("function returns true match for ordered input", {
  brick <- list(
    brick = "(heyho){5,10}",
    type = "ordered",
    content = NULL,
    occurrence = "custom",
    custom_motif = "heyho",
    custom_occurrence = c(5, 10)
  )
  res <- generate_from_brick(brick)
  expect_true(all(grepl(pattern = brick$brick, x = res)))
})

test_that("function returns true match for unordered input", {
  brick <- list(
    brick = "[[:digit:]]+",
    type = "unordered",
    content = "digits",
    occurrence = "at least once",
    custom_motif = "write your motif here",
    custom_occurrence = NULL
  )

  res <- generate_from_brick(brick)
  expect_true(all(grepl(pattern = brick$brick, x = res)))
})

test_that("function returns true scarbble match for unordered input", {
  brick <- list(
    brick = "[WonDERFul]*",
    type = "unordered",
    content = "custom",
    occurrence = "custom",
    custom_motif = "WonDERFul",
    custom_occurrence = c(5, 10)
  )

  res <- generate_from_brick(brick)
  expect_true(all(grepl(pattern = brick$brick, x = res)))

  brick <- list(
    brick = "[zul]{4,4}",
    type = "unordered",
    content = "custom",
    occurrence = "custom",
    custom_motif = "zul",
    custom_occurrence = c(4, 4)
  )

  res <- generate_from_brick(brick)
  expect_true(all(grepl(pattern = brick$brick, x = res)))
})
