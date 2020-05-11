#' resdiualAna
#'
#' Function to automate residual analysis.
#' It outputs several partial regression plots
#'
#' @param model, The formula object to analyse
#' @keywords Residual-analysis
#' @export
#' @examples
#' resdiualAna()

resdiualAna <- function(model){
  prb <- lm(model, data=dsCase)
  yhat <- fitted(prb)
  ehat <- residuals(prb)
  #sighat <- summary.glm(prb)$sighat

  hlev <- influence(prb)[1]
  hlev <- unlist(hlev)

  #sresid <- ehat / (sighat*sqrt(1-hlev))
  zresid <- ehat / sd(ehat)
  sresid <- rstandard(prb)
  sdresid <- rstudent(prb)
  regByProducts <-
    cbind("Predicted values" = yhat,
          "Std Predicted value" = (yhat-mean(yhat))/sd(yhat),
          "Residual" = ehat,
          "Std Residual (zresid)" = zresid,
          "Stud Residual (sresid)" = sresid,
          "Stud Deleted Residual" = sdresid,
          "Cook's Distance" = cooks.distance(p1),
          "Centered Leverage value"= hlev
    )
  df <- psych::describe(regByProducts)
  class(df) <- class(df)[c(3,1,2)]
  df <-round(df[c(2,8,9,3,4)], 3)

  ggplot(as.data.frame(sdresid), aes(sample = sdresid)) +
    stat_qq(colour="red") + stat_qq_line(colour="green",
                                         size=1.5)
  ggsave(paste0(dirRslt,"sdresid.pdf"))

  varl <- all.vars(model)
  for (i in 2:length(varl)){
    modelsmaker(model,varl[i])
    print(all.vars(newmodelDV))
    print(str(newmodelDV))
    print(head(tmp))
    (ehatDV = residuals(lm(formula = newmodelDV, data = tmp)))
    print("test")
    tmp <- data.frame(
      ehatDV = residuals(lm(formula = newmodelDV, data = tmp)),
      ehatIV = residuals(lm(formula = newmodelIV, data = tmp)))  #family = ... verwijderd vanwege non-probit
    ggplot(tmp, aes(x=ehatIV, y=ehatDV))+
      geom_point(colour="blue")+
      geom_smooth(method = "glm", se=FALSE, colour="red")
    ggsave(paste0(dirRslt, "partial_",varl[i],".pdf"))
  }}
