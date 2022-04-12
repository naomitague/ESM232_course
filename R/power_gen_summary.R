#' Power Generation Summary
#'
#' This function computes instantaneous power generation
#’ from a reservoir given its height and flow rate into turbines
#' @param rho Density of water (kg/m3) Default is 1000
#' @param g Acceleration due to gravity (m/sec2) Default is 9.8
#' @param Kefficiency Turbine Efficiency (0-1) Default is 0.8
#' @param height height of water in reservoir (m) for n time steps
#' @param flow flow rate (m3/sec) for n time steps
#' @author Naomi
#' @examples power_gen(20, 1)
#' @return Min, Max and Mean Power generation (W/s)


power_gen_summary = function(height, flow, rho=1000, g=9.8, Keff=0.8) {
  
  # check to make sure we have the same number of height and flow values
  nheight  = length(height)
  nflow = length(flow)
  if (nheight != nflow) return("ERROR: Different number of height and flow values")
  
  # calculate power
  result = rho * height * (flow) * g * Keff
  
  # compute summary statistics
  power_mean = mean(result)
  power_max = max(result)
  power_min= min(result)
  
  return(list(max=power_max, min=power_min, mean = power_mean))
}
