#'  Plots the output from the TagLoss model
#'
#'  This function plots the observed and estimated proportions of tags recaptured (and the residuals between them is requested).
#'@param Data The original input data used for the model.
#'@param boot The number of bootstraps used (if any).  This is used to detemine whether bootstrap results should be used.
#'@param resid A residual plot (observed - estimated  proportions).  Only used when boot is set to 0.
#'@param col The colours to use for each tag type
#'@return Bar and residual plots.
#'@examples   Solve_TLmodel(c(0.5,0.1), AA_Data)
#'@examples   PlotMod(AA_Data, col=c('green','purple', 'red'))
#'@export
#'


PlotMod <- function(Data, col=c(2:4), boot=0, resid=FALSE){
  Data <- as.data.frame(Data)
  if(is.null(Modelout$par)) pin <- Modelout$mod_par[2,]
  if(!is.null(Modelout$par)) {if(is.null(Modelout$back_transformed_par)) {pin <- Modelout$par }else {pin <- Modelout$back_transformed_par}}
  TL_Wide(Data,boot=0)
  if(boot==0){
    if(resid) graphics::par(mfrow=c(2,1),las=1)
    if(nrow(obs)==2) col=col[1:2]
    est <- TagLoss(pin,bound=F,flag='P',obs=obs,t=t)
    Pobs<- apply(obs,2, function(x) x/sum(x))
    mx <- ceiling(max(c(est,Pobs))*10)/10
    bp <- graphics::barplot(Pobs, names.arg = t,beside =T, ylim=c(0,mx),col=scales::alpha(col,0.2), xlab='Liberty', ylab='Proportion of tags')
    graphics::points(bp, est,col='white', bg=col,pch=21,cex=2)
    if(resid) {
      res <- Pobs-est
      mx <- max(abs(res))*2; mx <- ifelse(mx>1,1,mx)
      xs <- matrix(Data[,1], nrow=nrow(obs), ncol=nrow(Data), byrow = T)
      graphics::plot(xs,res,col=alpha(col,0.3),cex=1.5,ylim=c(-mx,mx),bty='l',xlab='Liberty',ylab='Residual', pch=16)
      graphics::abline(h=0,lty=3) } }
  if(boot>0){
    if(nrow(obs)==2) col=col[1:2]
    Pobs<- apply(obs,2, function(x) x/sum(x))
    graphics::par(las=1)
    bp <- graphics::barplot(Pobs, names.arg = t,beside =T, col=alpha(col,0.2), xlab='TimeStep', ylab='Proportion of tags')
    est <- t(Modelout$mod_est[2,,])
    graphics::points(bp, est,col='white', bg=col,pch=21,cex=2)
    options(warn = -1)
    graphics::arrows(bp,t(Modelout$mod_est[1,,]),y1=t(Modelout$mod_est[3,,]),col=col,code=3,angle=90,length=0.05)
    options(warn = 1)
    }
  graphics::par(mfrow=c(1,1),las=1)
}

