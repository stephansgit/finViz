#' Calculating a performance table
#'
#' This function calculates and displays a table of period performance for financial instruments. A cumulated performance per period is calculated for the different instruments provided. For this purpose, the return data should be log-returns, otherwise the aggregation gives meaningless results.
#' @param R A zoo-object with returns
#' @keywords financial performance
#' @return Returns a data.frame object showing the instrument and their respective cumulated performance.
#' @details Currently only an aggregation by year is implemented.
#' @export
#' @examples
#' library(PerformanceAnalytics)
#' data(edhec)
#' calcPeriodPerformance(R=edhec[,1:5])

calcPeriodPerformance <- function(R) {
  returns.df <- data.frame(Date=index(R), coredata(R))
  returns.norm <- melt(returns.df, id.vars = "Date", variable.name = "Instrument", value.name="Return")
  perf.tbl <- returns.norm %>% 
    group_by(Instrument, Year=year(Date)) %>% 
    summarise(Return=sum(Return))
  perf.tbl <- dcast(perf.tbl, Instrument~Year, value.var="Return")
  return(perf.tbl)
}