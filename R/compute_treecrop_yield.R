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

compute_treecrop_yield <- function(clim, crop = 2, override_defaults = TRUE,
                                   Tmin_month_coeff = NULL, Tmin_month_coeff2 = NULL,
                                   Tmax_month_coeff = NULL, Tmax_month_coeff2 = NULL,
                                   P_month_coeff = NULL, P_month_coeff2 = NULL,
                                   Intercept = NULL) {
  # initialize indices for different trees
  winegrapes=1
  almonds=2
  tablegrapes=3
  oranges=4
  walnuts=5
  avocados=6

  if (is.null(Tmin_month_coeff)) {
    Tmin_month_coeff<- matrix(0, nrow = 6, ncol = 12)
    # set default non-zero values
    Tmin_month_coeff[winegrapes, 4] <- 2.65
    Tmin_month_coeff[almonds, 2] <- -0.015
    Tmin_month_coeff[tablegrapes, 7] <- 6.93
    Tmin_month_coeff[oranges, 12] <- 1.08
    Tmin_month_coeff[walnuts, 11] <- 0.68
    Tmin_month_coeff[avocados, 8] <- 17.71
  }

  if (is.null(Tmin_month_coeff2)) {
    Tmin_month_coeff2<- matrix(0, nrow = 6, ncol = 12)
    # set default non-zero values
    Tmin_month_coeff2[winegrapes, 4] <- -0.17
    Tmin_month_coeff2[almonds, 2] <- -0.0046
    Tmin_month_coeff2[tablegrapes, 7] <- -0.19
    Tmin_month_coeff2[oranges, 12] <- --0.2
    Tmin_month_coeff2[walnuts, 11] <- -0.020
    Tmin_month_coeff2[avocados, 8] <- -0.29
  }

  if (is.null(Tmax_month_coeff)) {
    Tmax_month_coeff <- matrix(0, nrow = 6, ncol = 12)
      Tmax_month_coeff[walnuts, 11] <- 0.68
      Tmax_month_coeff[avocados, 8] <- 17.71
  }

  if (is.null(Tmax_month_coeff2)) {
    Tmax_month_coeff2 <- matrix(0, nrow = 6, ncol = 12)
       Tmax_month_coeff2[walnuts, 11] <- -0.020
      Tmax_month_coeff2[avocados, 8] <- -0.29
  }

  if (is.null(P_month_coeff)) {
    P_month_coeff <- matrix(0, nrow = 6, ncol = 12)
       P_month_coeff[winegrapes, 6] <- 4.78
    P_month_coeff[winegrapes, 9] <- -2.24
        P_month_coeff[almonds, 1] <- -0.07
        P_month_coeff[tablegrapes, 1]  <- 0.035
    P_month_coeff[tablegrapes, 10] <- 1.71
        P_month_coeff[oranges, 5] <- 4.99
      P_month_coeff[walnuts, 2] <- 0.038
    P_month_coeff[avocados, 10] <- 1.00
  }

  if (is.null(P_month_coeff2)) {
    P_month_coeff2 <- matrix(0, nrow = 6, ncol = 12)

    P_month_coeff2[winegrapes, 6] <- -4.93
    P_month_coeff2[winegrapes, 9] <- 1.54
    P_month_coeff2[almonds, 1] <- 0.0043
    P_month_coeff2[tablegrapes, 1]  <- 0.024
    P_month_coeff2[tablegrapes, 10] <- -0.673
    P_month_coeff2[oranges, 5] <- -1.97
    P_month_coeff2[walnuts, 2] <- -0.0051
    P_month_coeff2[avocados, 10] <- -0.31
  }

  if (is.null(Intercept)) {
    Intercept <- c(-10.5,0.28,-73.89,-2.47,-5.83,-288.09)
  }

# create a growing year to deal with negative months
  clim <- clim %>% mutate(
    gwy = ifelse(month >= 8, year + 1, year)
  )

  clim_month_allyears <- clim %>%
    group_by(month, gwy) %>%
    dplyr::summarize(
      tmin_c = min(tmin_c), tmax_c = max(tmax_c),
      precip = sum(precip), .groups = "drop"
    )

  # compute yield
  compute_yield <- function(Tmin_month_coeff, Tmin_month_coeff2, Tmax_month_coeff, Tmax_month_coeff2,
                            Precip_month_coeff, Precip_month_coeff2, Intercept, clim_month_allyears, tgwy) {
    clim_month = subset(clim_month_allyears, gwy==tgwy)
    if (nrow(clim_month) == 12) {
    yield <-
      sum(Tmin_month_coeff * clim_month$tmin_c) +
      sum(Tmax_month_coeff * clim_month$tmax_c) +
      sum(Tmin_month_coeff2 *clim_month$tmin_c)**2 +
      sum(Tmax_month_coeff2 * clim_month$tmax_c)**2 +
      sum(Precip_month_coeff * clim_month$precip) +
      sum(Precip_month_coeff2 * clim_month$precip**2) + Intercept
    }
    else
      yield=NULL

     return(yield)
  }

  res <- unique(clim_month_allyears$gwy) %>%
    map(~ compute_yield(
      Tmin_month_coeff[crop,], Tmin_month_coeff2[crop,],  Tmax_month_coeff[crop,], Tmax_month_coeff2[crop,],
      P_month_coeff[crop,], P_month_coeff2[crop,], Intercept[crop], clim_month_allyears, tgwy=.x
    ))

  res=unlist(res)
  return(list(maxyield = max(res), minyield = min(res), meanyield = mean(res)))
}
