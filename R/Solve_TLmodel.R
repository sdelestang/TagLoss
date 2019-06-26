#'  Uses nlminb optimisation to solve the TagLoss model
#'
#'  This function minimises the optimisation process prodced by the TagLoss model.
#'
#'@param pin  The input parameters.  Takes a vector of two or four numbers.
#'@param Data Observed data as a data.frame in wide format.  rows should represent unique periods at liberty identified by the first column (liberty).  Subsequent column record the numbers of observation of each tag recapture (e.g. column two can be those recaptured with two tags and column three those with only one tag).
#'@param trace The value of the objective function and the parameters is printed every iteration. Defaults to 0 which indicates no trace information is to be printed.
#'@param bound Whether the parameters should be bounded betweeen 0 and 1.  Logical.
#'@param boot Whether bootstrapping should occur (occurs if > 1) and how many iterations should be used in the bootstrapping proceedure.
#'@param probs The percentiles used in summarising the bootstrapping proceedure (defaluts to 2.5, 50 and 95th percentiles)
#'@return A list "Modelout" containing parameter estimates (back-transformed if bound=TRUE) and a summary of the optimisation process.  If boot >1 bootstrapping occurs and percentile estimates are generated for all parameters and the estimated numbers of tag recaptures in each group.
#'@examples Solve_TLmodel(c(0.5,0.1), AA_Data)
#'@export
#'

Solve_TLmodel <- function(pin,Data,trace=FALSE,bound=TRUE,boot=0,probs=c(0.025,0.5,0.975)){
  Data <- as.data.frame(Data)
  TL_Wide(Data,boot=0)
  trial <- ifelse(nrow(obs)==2,'AA','ab')
  nr <- ifelse(nrow(obs)==2,2,3)
  if(boot>1){
    mod_par_rep <- matrix(NA,nrow=boot,ncol=length(pin))
    mod_est_rep <- array(NA, dim=c(boot,nrow(Data),nr))
    Boot<-T  } else Boot <- F
  state <- ifelse(length(pin)==2 & trial=='AA', 1, ifelse(length(pin)==4 & trial=='ab',1,0))
  if(!Boot){
    if(state==0) {return('Pin is wrong length for model. Should be two numbers for AA and four for ab')
      } else {
      tout <- stats::nlminb(pin, TagLoss, obs=obs,t=t, control=list(trace=as.numeric(trace)))
      if(bound) tout$back_transformed_par <- cos(tout$par)^2 }  }
  if(Boot){
    if(state==0) {return('Pin is wrong length for model. Two parameters needed for AA and four for ab')
      } else {
      for(i in 1:boot){
        TL_Wide(Data,boot=boot)
        tout <- stats::nlminb(pin, TagLoss, obs=obs,t=t, control=list(trace=0))
        if(bound) tout$par <- cos(tout$par)^2
        mod_par_rep[i,] <- tout$par
        mod_est_rep[i,,][Data[,1]%in%t,] <- t(TagLoss(tout$par,bound=FALSE,obs=obs,t=t,flag='P'))
    }
      mod_par <- apply(mod_par_rep,2,stats::quantile, probs=probs)
      mod_est  <- apply(mod_est_rep,c(2,3),stats::quantile, probs=probs,na.rm=T)
      tout <- list( mod_est_bstraps=mod_est_rep, mod_est=mod_est,mod_par_bstraps=mod_par_rep, mod_par=mod_par)
      }}
    assign('Modelout',tout,envir=parent.frame())
    return(tout)}
