#' lowflowmetrics
#'
#' Compute percent error between observation and model
#' @param  m  model estimates
#' @param  o  observations
#' @param  month month
#' @param  day day
#' @param  year year
#' @return annual_min_err, annual_min_corr, low_month_cor, low_month_err


compute_lowflowmetrics = function(m,o, month, day, year,wy) {

  flow = cbind.data.frame(m,o, month, day, year,wy)
  # first lets get minimum yearly values
  
  tmp = flow %>% group_by(wy) %>% summarize(mino=min(o), minm=min(m))

  annual_min_err = mean(tmp$minm-tmp$mino)
  
  annual_min_corr = cor(tmp$minm, tmp$mino)
  
  # now lets get monthly values
  tmp = flow %>% group_by(month, year) %>% summarize(model=sum(m), obs=sum(o))
  # now extract august
  aug = subset(tmp, month==8)
  low_month_err = mean(aug$model-aug$obs)
  low_month_cor=cor(aug$model, aug$obs)
  return(list(annual_min_err=annual_min_err, annual_min_corr=annual_min_corr, low_month_err=low_month_err,
              low_month_cor=low_month_cor))
}
