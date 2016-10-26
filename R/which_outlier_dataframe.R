###############################################################################
# which_outlier_dataframe
###############################################################################
#
# function returns an index indicating which rows is an outlier
#
# Procedure: Each column is normalized to contain values between -1 and 1.
# For each row, euclidean distance(normalized by dividing by length of the row)
# between the row and the col medians is computed and passed to `which_outlier`
#
###############################################################################
#
# author  : Srikanth KS (talegari)
# license : GNU AGPLv3 (http://choosealicense.com/licenses/agpl-3.0/)
#
###############################################################################

which_outlier_dataframe <- function(x, coef = 1.5){

  stopifnot(is.data.frame(x))
  stopifnot(unique(vapply(x, class, character(1)) %in% c("numeric", "integer")))

  euc_dist_normalized <- function(x, y){
    pos <- (!is.na(x)) & (!is.na(y))
    return( sqrt(sum((x[pos] - y[pos])^2))/sum(pos) )
  }

  normalize1 <- function(mat){
    apply(x, 2, function(acol){ acol/max(abs(acol)) })
  }

  y <-  normalize1(x)

  colMedian <- apply(y, 2, function(acol2){median(acol2, na.rm = TRUE)})

  pointEst  <- apply(x, 1, function(arow){euc_dist_normalized(arow, colMedian)})
  return( which_outlier(pointEst, coef = coef) )
}
