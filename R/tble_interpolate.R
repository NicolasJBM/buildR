#' Replace missing values with interpolation (or extrapolation) over five years.
#' @param x      Tibble. One column "year", and one column "value".
#' @return A tibble with year and actual or interpolated values
#' @importFrom dplyr %>%
#' @importFrom dplyr case_when
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr lag
#' @export


tble_interpolate <- function(x){
  
  value <- NULL
  year <- NULL
  
  y <- x %>%
    dplyr::mutate(
      lag1 = dplyr::lag(value, 1),
      lag2 = dplyr::lag(value, 2),
      lag3 = dplyr::lag(value, 3),
      lag4 = dplyr::lag(value, 4),
      lag5 = dplyr::lag(value, 5),
      lead1 = dplyr::lead(value, 1),
      lead2 = dplyr::lead(value, 2),
      lead3 = dplyr::lead(value, 3),
      lead4 = dplyr::lead(value, 4),
      lead5 = dplyr::lead(value, 5)
    )
  
  y[,"altval"] <- rowMeans(y[, c("lag1","lag2","lag3","lag4","lag5","lead1","lead2","lead3","lead4","lead5")], na.rm = TRUE)
  
  y %>%
    dplyr::mutate(value = dplyr::case_when(is.na(value) ~ altval, TRUE ~ value)) %>%
    dplyr::select(year, value)
  
}
