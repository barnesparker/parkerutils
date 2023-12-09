test_that("can handle no group variables", {
  expect_equal(
    mtcars |>
      count_distinct(distinct_by = mpg),
    mtcars |>
      dplyr::summarise(n = dplyr::n_distinct(mpg))
  )
})

test_that("can handle one group variable", {
  expect_equal(
    mtcars |>
      count_distinct(cyl, distinct_by = mpg),
    mtcars |>
      dplyr::group_by(cyl) |>
      dplyr::summarise(n = dplyr::n_distinct(mpg))
  )
})

test_that("can handle multiple group variables", {
  expect_equal(
    mtcars |>
      count_distinct(cyl, gear, distinct_by = mpg),
    mtcars |>
      dplyr::group_by(cyl, gear) |>
      dplyr::summarise(n = dplyr::n_distinct(mpg))
  )
})

test_that("sorting works", {
  expect_equal(
    mtcars |>
      count_distinct(cyl, distinct_by = mpg, sort = TRUE),
    mtcars |>
      dplyr::group_by(cyl) |>
      dplyr::summarise(n = dplyr::n_distinct(mpg)) |>
      dplyr::arrange(desc(n))
  )
})

test_that("naming works", {
  expect_equal(
    mtcars |>
      count_distinct(distinct_by = mpg, name = "n_cyl") |>
      colnames(),
    "n_cyl"
  )

})
