
test_that("function returns error for unexpected input", {
  expect_error({
    build_a_regex_brick(type = "unknown")
  })
  expect_error({
    build_a_regex_brick(content = "unknown")
  })
  expect_error({
    build_a_regex_brick(occurrence = "unknown")
  })
  expect_error(
    {
      build_a_regex_brick(occurrence = "custom")
    },
    regexp = "custom occurrence selected but no value given"
  )
  expect_error(
    {
      build_a_regex_brick(content = "custom")
    },
    regexp = "custom motif selected but no value given"
  )
})

test_that("function gives expected regex for ordered type with escape", {
  regex_output <- build_a_regex_brick(
    type = "ordered",
    occurrence = "custom",
    custom_motif = "hello?",
    custom_occurrence = c("1", "no max")
  )
  expect_equal(object = regex_output$brick, expected = "(hello\\?){1,}")
})

test_that("function gives expected regex with no escape", {
  regex_output <- build_a_regex_brick(
    type = "ordered",
    occurrence = "anytime",
    custom_motif = "hello?",
    escape = FALSE
  )
  expect_equal(object = regex_output$brick, expected = "(hello?)*")
})

test_that("function gives expected regex for unordered type", {
  regex_output <- build_a_regex_brick(
    type = "unordered",
    occurrence = "anytime",
    custom_motif = "hello",
    content = c("lowercase letters", "uppercase letters", "custom")
  )
  expect_equal(
    object = regex_output$brick,
    expected = "[[:lower:][:upper:]hello]*"
  )
})
