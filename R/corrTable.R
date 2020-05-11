#' make correlation table
#'
#' @param xdata, dataset
#' @param method, pearson or
#' @keywords correlation table
#' @export
#' @examples
#'
#' corrTable()

corrTable <- function(xData, method=c("pearson")) {
  #--- Vinden van correlaties en significante waarden
  rCorr  <- rcorr(as.matrix(xData),type=method)
  rValue <- format(round(rCorr$r,digits=3),nsmall=3)
  rSig   <- rCorr$P
  # Asterisken toewijzen aan significante waarden
  rStars <- ifelse(rSig < 0.001,"***",ifelse(rSig < 0.01,"** ",
                                             ifelse(rSig < 0.05,"*  ","   ")))
  # Vinden van gemiddelden en standaarddeviaties
  xMean  <- format(round(sapply(xData,mean,na.rm=TRUE),3),nsmall=3)
  xStd   <- format(round(sapply(xData,sd,na.rm=TRUE),3),nsmall=3)
  # Combineren van correlaties met asterisken
  d <- matrix(paste(rValue,rStars,sep=""),ncol=ncol(rValue))
  colnames(d) <- capitalize(colnames(rValue))
  #  Verwijderen van alle waarden boven diagonaal
  d[upper.tri(d, diag=TRUE)] <- "   "
  #  Combineren van resultaten
  rTable <-data.frame(Mean=xMean, Std=xStd, d[,-ncol(d)],
                      stringsAsFactors=FALSE)
  rownames(rTable) <- capitalize(rownames(rTable))
  return(rTable)
}

corrTableP <- function(xData, method=c("pearson")) {
  rCorr  <- rcorr(as.matrix(xData),type=method)
  rValue <- format(round(rCorr$r,digits=3),nsmall=3)
  rSig   <- format(round(rCorr$P,digits=3),nsmall=3)
  rStars <- paste0("(",rSig,")")
  xMean  <- format(round(sapply(xData,mean,na.rm=TRUE),3),nsmall=3)
  xStd   <- format(round(sapply(xData,sd,na.rm=TRUE),3),nsmall=3)
  d <- matrix(paste(rValue,rStars,sep=""),ncol=ncol(rValue))
  colnames(d) <- capitalize(colnames(rValue))
  d[upper.tri(d, diag=TRUE)] <- "   "
  rTable <-data.frame(Mean=xMean, Std=xStd, d[,-ncol(d)],
                      stringsAsFactors=FALSE)
  rownames(rTable) <- capitalize(rownames(rTable))
  return(rTable)
}
