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

modelsmaker <- function(model, excl, data=dsCase){
  varl <- all.vars(model)
  varl <- varl[-match(excl, varl)]
  newmodelDV <- paste0(varl[1], " ~ ")
  newmodelDV <<- formula(paste0(newmodelDV,
                            paste0(varl[2:length(varl)], collapse = " + ")))
  newmodelIV <- paste0(excl, " ~ ")
  newmodelIV <<-  formula(paste0(newmodelIV,
                                 paste0(varl[2:length(varl)], collapse = " + ")))
  }
