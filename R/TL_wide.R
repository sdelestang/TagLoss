#'Restructures double-tag-loss data in wide format
#'
#'Takes data on double tagged data in wide format and re-cofigures it into two input files for the tag-loss model DTL.
#'
#'@param Data  The input data. Takes a data.frame with oberservations as rows and liberties (first column) and tag categories (subsequent to first) as columns.
#'@param boot  A nuber value.  Numbers greater than 1 cause the data to be resampled for boot strapping.  Defaults to 0.
#'@return Two input files for use by the DLT function:
#'      A matrix with tag categories as rows and liberties as columns and a vector of liberties.
#'@examples TL_Wide(ab_Data)
#'@export
#'

TL_Wide <- function(Data,boot=0){
  Data <- as.data.frame(Data)
  options(warn = -1)
  if(exists('obs')){ rm(obs);rm(t)}
  options(warn = 1)
  if(ncol(Data)>2){
    #Data %<>% gather(tag, count, colnames(Data)[2:ncol(Data)]) %>% as.data.frame()
    Data <- tidyr::gather(Data, tag, count, colnames(Data)[2:ncol(Data)])
    Data <- as.data.frame(Data)
    tData <- tapply(Data[,3], list(Data[,2], Data[,1]), sum)  ## Store orig format cause of 0's
    tData <- matrix(rep(0, length(tData)), ncol=ncol(tData),nrow=nrow(tData), dimnames=dimnames(tData))
    tmp <- data.frame(time=rep(as.numeric(Data[,1]), Data[,3]), tag=rep(Data[,2], Data[,3]))
    if(boot>1){
      tmp <- tmp[base::sample(1:nrow(tmp),nrow(tmp),  replace = T),]
    }
    tmp <- tapply(tmp[,1], list(tmp[,2], tmp[,1]), length)
    tmp[is.na(tmp)] <- 0
    tData[rownames(tData)%in%rownames(tmp),colnames(tData)%in%colnames(tmp)] <- tmp
    tmp <- tData
    if(nrow(tmp)==2) tmp <- tmp[c('AA','A'),]
    if(nrow(tmp)==3) tmp <- tmp[c('ab','a','b'),]
    if(nrow(tmp)==5) tmp <- tmp[c('AA','A','ab','a','b'),]
    t <- as.numeric(colnames(tmp))
    assign('obs',tmp,envir=parent.frame())
    assign('t',t,envir=parent.frame()) }else {print('Dataframe is not in wide format')}
}
