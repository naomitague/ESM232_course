#' Compute Atmospheric Conductance
#'
#' THis function atmospheric conductance as a function of windspeed, and vegetation cahracteristics
#' @param       v windspeed (m/s)
#' @param      height vegetation height (m)
#' @param       zm measurement height of wind (m) (default 30)
#' @param      mult_zo scalar for roughness (default 0.1)
#' @param      mult_zd scalar for zero plane displacement (default 0.7)
#' @author Naomi
#' 
#' @return  Conductance (mm/s)

Catm = function(v, height, zm=30, mult_zo=0.1, mult_zd=0.7) {
    
  
    zd = mult_zd*height
    zo = mult_zo*height
    
    zd = ifelse(zd < 0, 0, zd)
    Ca = ifelse(zo > 0,
     v / (6.25*log((zm-zd)/zo)**2),0)
  
# convert to mm
    Ca = Ca*1000
   return(Ca)
}
