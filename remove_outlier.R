###############################################################################
# remove_outliers
###############################################################################
#
# function returns a vector after removing outliers
# NA's are not considered as outliers
# the logic is similar to `boxplot.stats`
#
###############################################################################
#
# author  : Srikanth KS (talegari)
# license : GNU AGPLv3 (http://choosealicense.com/licenses/agpl-3.0/)
#
###############################################################################

remove_outlier <- function(x, coef = 1.5){

  stopifnot(class(x) %in% c("numeric", "integer"))
  stopifnot(coef >= 0)
  stats <- stats::fivenum(x, na.rm = TRUE)
  iqr   <- diff(stats[c(2L, 4L)])
  inn   <- x >= (stats[2L] - (coef * iqr)) & x <= (stats[4L] + (coef * iqr))
  inn[is.na(inn)] <- TRUE
  return(x[inn])
}
