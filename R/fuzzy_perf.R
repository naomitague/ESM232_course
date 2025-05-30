#' fuzzy_perf
#'
#' Compute a performance measure (0-1) between observation and model
#' when obervations are uncertain
#' @param x vector of values
#' @param a1 first parameter below which performance is not acceptable
#' @param s2 second parameter lower threshold of good performance
#' @param a3 third parameter upper threshold of good performance
#' @param a4 fourth parameter above which performance is not acceptable
#' @return  combined 0-1 performance measure



fuzzy_perf = function(x, a1,a2,a3,a4) {

  if (length(x) == 0) {
    return(NA)
  }

  if (x <= a1) {
    return(0)
  } else if (x < a2) {
    return((x - a1) / (a2 - a1))
  } else if (x < a3) {
    return(1)
  } else if (x < a4) {
    return((a4 - x) / (a4 - a3))
  } else {
    return(0)
  }
}

