#' Population Evolution using Leslie Matrix
#' Evolve a population
#' @param fertility fertility rates
#' @param survivability survivability rates
#' @param initialpop initial population
#' @param nstep number of time steps
#' @param K total population carrying capacity
#' @return population structure for each time step (OR error message if population cannot be defined)


evolve_popK = function(fertility, survivability, initialpop, nstep,K=0) {

nclasses = length(fertility)

# make sure inputs are in the right format
if ((nclasses!=length(survivability) ))
{ return("fertility doesn’t match survivability") }

if ((nclasses!=length(initialpop) ))
{ return("population initialization  doesn’t match fertility") }

#initialize the Leslie matrix
leslie_matrix = matrix(nrow=nclasses, ncol=nclasses)
leslie_matrix[,] = 0.0
leslie_matrix[1,] = fertility

for (i in 1:(nclasses-1)) {
leslie_matrix[i+1,i] = survivability[i]
}
leslie_matrix[nclasses,nclasses] = survivability[nclasses]

# create an matrix to store population structure
pop.structure = matrix(nrow=nclasses, ncol=nstep)

pop.structure[,1] = initialpop

for (i in 2:nstep) {

total.pop=sum(pop.structure[,i-1])

# if we are using a carrying capacity adjust fertitlity if we are getting close
if (K > 0) {
  ratio = max(0, 1.0-total.pop/K)
  leslie_matrix[1,] = fertility*ratio
}
pop.structure[,i] = leslie_matrix %*% pop.structure[,i-1]

}

return(pop.structure)
}
