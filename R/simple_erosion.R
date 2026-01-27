#' Simple Toy Erosion Model
#'
#' Computes amount of erosion based on rainfall intensity and soil properties
#' @param I rainfall intensity (mm/hr)
#' @param f infiltration capacity (mm/hr)
#' @param k exponent scaling runoff effect on erosion, default = 4

#' @author Naomi
#' @return Erosion value (kg sediment / hr)

simple_erosion <- function(I, f, k = 4) {
# first compute runoff
  q <- pmax(0, I - f)
# then erosion
  erosion <- q^k

  return(erosion)
}
