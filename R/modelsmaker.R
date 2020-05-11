#' modelsmaker
#'
#' Takes a formula object as input and outputs two new formulae.
#' It's use is to create the models required in residual analysis.
#' newmodelIV will be a formula with the original independent variable.
#' newmodelDV will be a formula with excl as independent variable
#' @param model The formula object representing your raw model
#' @param excl A variable to exclude in the new model
#' @keywords modelmaker
#' @export
#' @examples
#' modelsmaker()

modelsmaker <- function(model, excl){
  varl <- all.vars(model)
  assign(x = "tmp",value = dsCase[complete.cases(dsCase[c(varl)]),c(varl)],
         pos= 1)
  print(head(tmp))
  varl <- varl[-match(excl, varl)]
  newmodelDV <- paste0(varl[1], " ~ ")
  assign(x="newmodelDV", value = formula(paste0(newmodelDV,
                                                paste0(varl[2:length(varl)], collapse = " + "))),
         pos = 1)
  newmodelIV <- paste0(excl, " ~ ")
  newmodelIV <<-  formula(paste0(newmodelIV,
                                 paste0(varl[2:length(varl)], collapse = " + ")))
}
