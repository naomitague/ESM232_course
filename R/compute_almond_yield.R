#' Compute Almond Yield
#'
#' computes almond yield anomolies
#' Following Lobell et al. (2006, Table 2), yield anomaly depends on
#' February minimum temperature (the monthly average of daily minimum
#' temperatures, C) and total January precipitation (mm)
#'
#' @param  clim climate data frame
#' as daily (day, month, year, tmin_c, tmax_c, precip )
#' @param  Tmincoeff1 default=-0.015 (linear Feb min temperature term)
#' @param  Tmincoeff2 default=-0.0046 (quadratic Feb min temperature term)
#' @param  Pcoeff1 default=-0.07 (linear Jan precipitation term)
#' @param  Pcoeff2 default=0.0043 (quadratic Jan precipitation term)
#' @param  intercep default=0.28
#' @author Naomi Tague
#' @references D.B. Lobell et al. Agricultural and Forest Meteorology 141 (2006) 208–218.
#' @return
#' almond yield (anomoly from California mean in ton/acre, mean, maximum and minimum yields
#'
#'
compute_almond_yield <- function(clim, Tmincoeff1 = -0.015, Tmincoeff2 = -0.0046, Pcoeff1 = -0.07, Pcoeff2 = 0.0043, intercep = 0.28) {
  # extracted required climate variables

  tmp <- clim %>%
    group_by(month, year) %>%
    # monthly average of daily minimum temperature (Lobell et al. 2006)
    dplyr::summarize(tmin_c = mean(tmin_c), .groups = "drop")
  Feb_minT <- (tmp %>% subset(month == 2))$tmin_c

  tmp <- clim %>%
    group_by(month, year) %>%
    dplyr::summarize(precip = sum(precip), .groups = "drop")
  Jan_P <- (tmp %>% subset(month == 1))$precip


  # compute yield
  yield <-
    Tmincoeff1 * Feb_minT + Tmincoeff2 * Feb_minT**2 +
    Pcoeff1 * Jan_P + Pcoeff2 * Jan_P**2 + intercep

  return(list(maxyield = max(yield), minyield = min(yield), meanyield = mean(yield)))
}
