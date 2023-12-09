#' Search a data frame for a value
#'
#' @param x A data frame or tibble.
#'
#' @param value The value to search for.
#'
#' @export
search_tbl <- function(x, value) {
  x |>
    dplyr::select(dplyr::where(\(col) any(stringr::str_detect(col, value), na.rm = TRUE))) |>
    dplyr::filter(dplyr::if_any(dplyr::everything(), \(col) stringr::str_detect(col, value)))
}
