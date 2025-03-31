#' Lot. Voltera Model
#'
#' function computes the rate of change of populations in a predictor prey interaction
#' @param t  time (days)
#' @param pop datatype list initial conditions; list with two values prey=number of prey and pred=number of predictor
#' @param pars datatype list  coefficient in Lotka-Voltera pars$rprey, pars$alpha, pars$eff, par$pmort
#'  \emph{rprey} is growth rate of prey population;
#'  \emph{eff} is the rate of ingestion of prey by predators
#'  \emph{alpha} is a interaction coefficient (higher values greater interaction
# ’  \emph{pmort}  mortality rate of predictor population
#'  \emph{harv} harvest amount
#' @examples
#' lotvodKharvest(t = 1, pop = list(1, 2), pop = list(0.5, 0.3, 0.2, 0.2))
#'
#' pars <- data.frame(rprey = 0.95, alpha = 0.01, eff = 0.6, pmort = 0.4, K = 2000, harvest = 10)
#' currpop <- c(prey = 1, pred = 1)
#  days = seq(from=1,to=20)
#' res <- ode(func = lotvmodKharvest, y = currpop, times = days, parms = pars)
#'
#' @return  lotvmod returns a list containing the following components
#' \describe{
#' \item{dprey}{rate of change of prey populutation}
#' \item{dpred}{rate of change of preditor populutation}
#' }

lotvmodKharvest <- function(t, pop, pars) {
  with(as.list(c(pars, pop)), {
    dprey <- ifelse(prey > minharvest, rprey * (1 - prey / K) * prey - alpha * prey * pred - harvest,
      rprey * (1 - prey / K) * prey - alpha * prey * pred
    )
    dpred <- eff * alpha * prey * pred - pmort * pred
    return(list(c(dprey, dpred)))
  })
}
