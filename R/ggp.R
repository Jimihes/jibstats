#' ggp
#'
#' Function to plot y against x and use ggsave
#' @param y, y variable
#' @param x, x variable
#' @keywords scatterplot
#' @export
#' @examples ggp(GDWLIMP,SMOOTH)
#' ggp()

ggp <- function(y,x){
  ggplot(data=dsCase, aes_string(y=y, x=x)) +
    geom_point(col="blue") +
    xlab(x) +
    ylab(y) +
    geom_smooth(method = lm, col = "black", lwd = 1.0,
                se = FALSE) +
    ggsave(paste0(dirRslt, "plot_",x,".pdf"))
}
