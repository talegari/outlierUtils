###############################################################################
# outlier_which
###############################################################################
#
# function returns positions of the outliers
# NA's are not considered as outliers
# the logic is similar to `boxplot.stats`
#
###############################################################################
#
# author  : Srikanth KS (talegari)
# license : GNU AGPLv3 (http://choosealicense.com/licenses/agpl-3.0/)
#
###############################################################################v

outlier_which <- function(x, coef = 1.5){

  stopifnot(class(x) %in% c("numeric", "integer"))
  stopifnot(coef >= 0)
  stats <- stats::fivenum(x, na.rm = TRUE)
  iqr   <- diff(stats[c(2L, 4L)])
  out   <- x < (stats[2L] - (coef * iqr)) | x > (stats[4L] + (coef * iqr))
  if(sum(out) != 0){
    return(which(out))
  } else {
    return(integer())
  }
}
