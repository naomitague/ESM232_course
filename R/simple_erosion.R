#' Simple Toy Erosion Model
#'
#' Computes amount of erosion based on rainfall intensity and soil properties
#' @param I rainfall intensity (mm/hr)
#' @param f infiltration capacity (mm/hr)
#' @param a exponent scaling runoff effect on erosion, default = 2
#' @param k exponent scaling infiltration effect on runoff default = 4

#' @author Naomi
#' @return Erosion value (kg sediment / hr)

simple_erosion <- function(I, f, k = 4, a = 2) {
# first compute runoff
  q <- pmax(0, I - f)^k
#  then erosion given runoff (a > 1)
  erosion = q^a
  return(erosion)
}
