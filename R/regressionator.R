#' regressionator
#'
#' Create formula object from list of variables and outputs in stargazer
#' @param vars List of variables to make formula, or formula object
#' @param method OLS, probit or tobit
#' @param dep Dependent variable, defaults to first item in vars
#' @param name A name for stargazer output. .doc extension is added
#' @param quadr non-linear variables to add as quadratic
#' @param dsCase Dataset of your case, defaults to, possibly non-existent, dsCase
#' @param residualAna Wether to do a residualanalysis
#' @keywords regressionator
#' @export
#' @examples regressionator(vars = c(k, EXTERNAL, BATH:SMOOTH))
#' regressionator()

regressionator <- function(vars, method="OLS", dep = vars[1], name = "", quadr ="",
                           data = dsCase, residualAna = FALSE){
  if (is_formula(vars)) vars <- all.vars(vars)
  if (quadr != ""){
    quadr <- c(quadr)
    ls <- c()
    for (i in quadr){
      j <- paste0("I(", i, "^2)")
      vars <- c(vars, j)
    }}

  if (dep == vars[1]) vars <- vars[2:length(vars)]
  newformula <- paste0(dep, " ~ ")
  newformula <-  formula(paste0(newformula,paste0(vars, collapse = " + ")))

  if (method == "OLS"){
    newmodel <- lm(formula = newformula, data = data)
  }else if (method == "probit"){
    newmodel <- glm(newformula, data=data, family = binomial(link = "probit"))
  }else if (method == "tobit"){}

  if(name == ""){
    name == paste0(force(method), "_model")
  }
  print("Opslaan regressiemodel")
  sink("NUL")
  stargazer(newmodel, out = paste0(dirRslt,name,".doc"), summary = FALSE, type = "html")
  sink()
  cat("Model opgeslagen \n\n")
  if (residualAna == TRUE){
  residualAna(newformula, data=data)}
  return(newmodel)
  }
