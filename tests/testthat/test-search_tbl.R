test_that("it works", {
  test_tbl <-
    tibble::tibble(
      x = c("a", "b", "c"),
      y = c("d", "e", "a"),
      z = c("f", "g", "h")
    )

  expect_equal(
    test_tbl |>
      search_tbl("a"),
    tibble::tibble(
      x = c("a", "c"),
      y = c("d", "a")
    )
  )


})
