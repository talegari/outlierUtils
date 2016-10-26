###############################################################################
# is_outlier
###############################################################################
#
# function returns a logical vector indicating whether the value is an outlier
# NA's are not considered as outliers
# the logic is similar to `boxplot.stats`
#
###############################################################################
#
# author  : Srikanth KS (talegari)
# license : GNU AGPLv3 (http://choosealicense.com/licenses/agpl-3.0/)
#
###############################################################################

is_outlier <- function(x, coef = 1.5){

  stopifnot(class(x) %in% c("numeric", "integer"))
  stopifnot(coef >= 0)
  stats <- stats::fivenum(x, na.rm = TRUE)
  iqr   <- diff(stats[c(2L, 4L)])
  out   <- x < (stats[2L] - (coef * iqr)) | x > (stats[4L] + (coef * iqr))
  out[is.na(out)] <- FALSE
  return(out)
}
