#' residualAna
#'
#' Function to automate residual analysis.
#' It outputs several partial regression plots
#' @param model, The formula object to analyse
#' @keywords Residual-analysis
#' @export
#' @examples
#' residualAna()

residualAna <- function(model, data=dsCase){

    lregr <- lm(model, data=data)
  yhat <- fitted(lregr)
  ehat <- residuals(lregr)
  hlev <- influence(lregr)[1]
  hlev <- unlist(hlev)
  zresid <- ehat / sd(ehat)
  sresid <- rstandard(lregr)
  sdresid <- rstudent(lregr)
  regByProducts <-
    cbind("Predicted values" = yhat,
          "Std Predicted value" = (yhat-mean(yhat))/sd(yhat),
          "Residual" = ehat,
          "Std Residual (zresid)" = zresid,
          "Stud Residual (sresid)" = sresid,
          "Stud Deleted Residual" = sdresid,
          "Cook's Distance" = cooks.distance(lregr),
          "Centered Leverage value"= hlev)

  print("Check for multicollinearity:")
  print(car::vif(lregr))
  cat("\n\n")
  df <- psych::describe(regByProducts)
  class(df) <- class(df)[c(3,1,2)]
  (df <-round(df[c(2,8,9,3,4)], 3))

  ggplot(as.data.frame(sdresid), aes(sample = sdresid)) +
    stat_qq(colour="red") + stat_qq_line(colour="green",
                                         size=1.5)
  print("Saving QQ-plot")
  ggsave(paste0(dirRslt,"sdresid.pdf"))
  cat("\n")
  varl <- all.vars(model)
  tmp <- data[complete.cases(data[c(varl)]),]
  for (i in 2:length(varl)){
    modelsmaker(model,varl[i], data = data)
    df <- data.frame(
      ehatIV = residuals(lm(formula = newmodelIV, data = tmp)),
      ehatDV = residuals(lm(formula = newmodelDV, data = tmp)))

    ggplot(df, aes(x=ehatIV, y=ehatDV))+
      geom_point(colour="blue")+
      geom_smooth(method = "glm", se=FALSE, colour="red")
    print(paste0("Saving partial regression plot ", i -1, " out of: ", length(varl)-1))
    suppressMessages(ggsave(paste0(dirRslt, "partial_",varl[i],".pdf")))
  }
  }
