#' impyrtable
#'
#' Function to ouput sample table by industry in a given year,
#'   or to output average impairment table by industry in a given year.
#' @param year, year to filter the data on
#' @param gem, wether to plot and save a table with mean impairment
#' (default=TRUE) sample table or to plot and save sample table
#' @keywords impyrtable
#' @export
#' @examples
#' impyrtable()


impyrtable <- function(year, gem=TRUE){
  if (gem == TRUE){
    table <- dsCase %>% group_by(INDUSTRY) %>%
      filter(fyear == year) %>%
      summarize('Write-off'= mean(GDWLIMP, na.rm = TRUE)) %>%
      round(digits = 3)
  }else{ table <- dsCase %>% group_by(INDUSTRY) %>%
    filter(fyear == year) %>%
    summarize('Write-off'= sum(GDWLDEC == 1))
  }
  stargazer(table,
            out = paste0("impyrtable_",year,".doc"), type = "html", summary = FALSE,
            no.space = TRUE, intercept.bottom = FALSE)
}
