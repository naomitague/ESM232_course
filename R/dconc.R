
#' Diffusion with update
#' @param time time
#' @param Conc initial concentration at each location
#' @param parms parameters list
#' @param D diffusion coefficient
#' @param k_upt uptake rate
#' @return derivative of population with time
#' @examples use with ode solver
#' ode(y=1,time=c(1;100),dexppop, parms=c(0.012))

dconc = function(time, Conc, parms) {

  with(parms, {
  # length of river
  nx = length(Conc)
  # we are going to change each location
  dCdt = numeric(length=nx)

  # first deal with locations not at boundary
  for (i in 2:(nx - 1)) {
    d2C_dx2 = (Conc[i + 1] - 2 * Conc[i] + Conc[i - 1]) / dx^2
    dCdt[i] = D * d2C_dx2 - k_upt * Conc[i]
  }

  # now deal with boundaries
  d2C_dx2_left = (Conc[2] - 2 * Conc[1] + Conc[2]) / dx^2
  d2C_dx2_right = (Conc[nx-1] - 2 * Conc[nx] + Conc[nx - 1]) / dx^2
  dCdt[1] = D * d2C_dx2_left - k_upt * Conc[1]
  dCdt[nx] = D * d2C_dx2_right - k_upt * Conc[nx]

  list(dCdt)
  })

  return(list(dCdt))
}
