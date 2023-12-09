
#' Count distinct values
#' @param x A data frame or tibble.
#'
#' @param ... Variables to group by.
#' @param distinct_by The name of the column to count distinct values of.
#' @param sort If TRUE, will show the largest groups at the top.
#' @param name The name of the new column in the output.
#' @param na.rm If TRUE, exclude missing observations from the count.
#' If there are multiple vectors in ..., an observation will be excluded if any
#' of the values are missing.
#'
#' @importFrom rlang :=
#' @export
count_distinct <- function(x, ..., distinct_by, sort = FALSE, name = "n", na.rm = FALSE) {

  assertthat::assert_that(
    !missing(distinct_by),
    msg = "Must provide `distinct_by` argument"
  )

  if (!missing(...)) {
    out <- dplyr::group_by(x, ..., .add = TRUE)
  } else {
    out <- x
  }


  out <-
    out |>
    dplyr::summarise(
      !!name := dplyr::n_distinct({{distinct_by}}, na.rm = na.rm)
    )

  if (sort) {
    out <- dplyr::arrange(out, dplyr::desc(!!rlang::sym(name)))
  }

  out
}
