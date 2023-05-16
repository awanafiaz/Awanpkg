#' This is a computational function

#' @author Awan Afiaz

#' @param x A vector of numeric values

#' @return Returns the logarithm of the sum of exponents of the given vector.
#' All NA values are automatically removed from computation.

#' @examples
#' data = c(1:200)
#' log_summed_exps(x = data)
#'
#' @importFrom stats na.omit

#' @export
log_summed_exps <- function(x) {
  ## Checks and removes all missing data
  x = ifelse(test = is.na(x), yes = na.omit(x), no = x)

  ## first order the values in ascending order and remove the first and last obs
  x_ordered_trunc = sort(x)[2:(length(x)-1)]

  ## take exponent of the ordered differences from the max of x
  exp_diff = exp(x_ordered_trunc - max(x) )

  ## Calculate final expression
  result = max(x) + log(1 + sum(exp_diff))

  return(result)
}

