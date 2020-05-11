#' regressionator
#'
#' Create formula object from list of variables and outputs in stargazer
#' @param vars List of variables to include in model
#' @param method OLS, probit or tobit
#' @param dep Dependent variable, defaults to first item in vars
#' @param name A name for stargazer output. .doc extension is added
#' @param quadr non-linear variables to add as quadratic
#' @param dsCase Dataset of your case, defaults to, possibly non-existent, dsCase
#' @keywords regressionator
#' @export
#' @examples regressionator(vars = c(k, EXTERNAL, BATH:SMOOTH))
#' regressionator()

regressionator <- function(vars, method="OLS", dep = vars[1], name = "", quadr ="", dsCase = dsCase){
  if (quadr != ""){
    quadr <- c(quadr)
    ls <- c()
    for (i in quadr){
      j <- paste0("I(", i, "^2)")
      vars <<- c(vars, j)
    }}

  newformula <- paste0(dep, " ~ ")
  newformula <-  formula(paste0(newformula,paste0(vars[2:length(vars)], collapse = " + ")))
  print(newformula)
    if (method == "OLS"){
    newmodel <- lm(formula = newformula, data = dsCase)
  }else if (method == "probit"){
    newmodel <- glm(newformula, data=dsCase, family = binomial(link = "probit"))
  }else if (method == "tobit"){}

  if(name == ""){
    name == paste0(method, "_model")
  }
  sink("NUL")
  stargazer(newmodel, out = paste0(name,".doc"), summary = FALSE, type = "html")
  sink()
  return(newmodel)
  }
