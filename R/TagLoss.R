#'  Models tag-loss rate
#'
#'  This function takes input parameters and computes the likelihodd of the observations ocurring based on the input parameters. Returns the Log-likelihood of the observations given the input parameters.  Also contains a switch which allows the usert to force the function to return the estimated probabilities instead of the Log-likelihood.
#'
#'@param pin  The input parameters.  Take a vector of two numbers.
#'
#'@param bound Whether the parameters should be bounded betweeen 0 and 1.  A logical.
#'@param obs The observed recaptures of tagged animals.  A matrix with recapture state as rows and observations from different time periods as columns.
#'@param t Time periods. A numeric vector.
#'
#'@param flag Switches the model to return the Log-likelihood or a matrix of estimated probabilities
#'@return A list containing parameter estimates (back-transformed if bound=TRUE) and a summary of the optimisation process.
#'@examples TL_Wide(AA_Data,boot=0)
#'@examples TagLoss(c(0.5,0.1,0.5,0.1), obs=obs, t=t)
#'
#'@export
#'

TagLoss <- function(pin,bound=TRUE,obs=obs,t=t,flag='solve'){
  if(bound) pin <- cos(pin)^2
  trial <- ifelse(nrow(obs)==2,'AA','ab')
  if(trial=='AA'){
    P <- rbind(PAA(pin,t), PA(pin,t))
    P <- apply(P,2,function(x) x/sum(x))
    }
  if(trial=='ab'){
    P <- rbind(Pab(pin,t), Pa.(pin,t), Pb(pin,t))
    P <- apply(P,2,function(x) x/sum(x))
    }
  if(flag=='P') return(P)
  LL <- -sum(obs[P>0] * log(P[P>0]))
  if(flag=='solve') return (LL)
}
