#' Compute Tree Crop Yield
#'
#' computes almond yield anomolies
#' @param  clim climate data frame
#' as daily (day, month, year, tmin_C, tmax_C, precip )
#' @param  Tmax_month_coeff data frame of 12 values default = 0 for all months
#' @param  Tmin_month_coeff data frame of 12 values default = 0 for all months
#' @param  Precip_month_coeff data frame of 12 values default = 0 for all months
#' @param Intercept
#' @author Naomi Tague
#' @references D.B. Lobell et al. Agricultural and Forest Meteorology 141 (2006) 208–218.
#' @return
#' yield (anomoly from California mean in ton/acre, mean, maximum and mininum yields
#'
# UNDER DEVELOPMENT

compute_treecrop_yield <- function(clim, Tmin_month_coeff, Tmax_month_coeff, Precip_month_coeff, Precip_month_coeff2, Intercept, crop = "Almond") {
  # extracted required climate variables

  crop_index <- case_when(crop) {
    crop == "Almond" ~ 1.0
    crop == "Walnut" ~ 0.8
    crop == "Pistachio" ~ 0.6
    TRUE ~ 1.0
  }

  clim_month_allyears <- clim %>%
    group_by(month, year) %>%
    dplyr::summarize(
      tmin_c = min(tmin_c), tmax_c = max(tmax_c),
      precip = sum(precip), .groups = "drop"
    )

  # compute yield
  compute_yield <- function(Tmin_month_coeff, Tmax_month_coeff,
                            Precip_month_coeff, Precip_month_coeff2, Intercept, year, clim_month_allyears) {
    subset(clim_month_allyears, year == year)
    yield <-
      Tmin_month_coeff * clim_month$tminc +
      Tmax_month_coeff * clim_month$tmaxc +
      Tmin_month_coeff2 * clim_month$tminc**2 +
      Tmax_month_coeff2 * clim_month$tmaxc**2 +
      Precip_month_coeff * clim_month$Precip + intercep
    Precip_month_coeff2 * clim_month$Precip**2 + intercep
    Pcoeff1 * Jan_P + Pcoeff2 * Jan_P**2 + intercep
    return(yield)
  }

  res <- unique(clim_month_allyears$year) %>%
    map_dfc(~ compute_yield(
      year = .x, Tmin_month_coeff, Tmax_month_coeff,
      Precip_month_coeff, Precip_month_coeff2, Intercept, clim_month_allyears
    ))



  return(list(maxyield = max(yield), minyield = min(yield), meanyield = mean(yield)))
}
