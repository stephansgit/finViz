#' Plotting a performance matrix
#'
#' This function plots the performances of financial instruments in the form of a four-field-matrix
#' @param r A zoo-object with returns
#' @param short Lookback period for the shortest time window
#' @param mid Lookback period for the middle time window
#' @param long Lookback period for the longest time window
#' @keywords financial performance
#' @export
#' @examples
#' test <- zoo(matrix(rnorm(n=5*300,mean = 0, sd=0.01),300), as.Date("2013-01-01")+0:299)
#' colnames(test) <- c("Stock A", "Stock B", "Stock C", "Stock D", "Stock E")
#' plotPerfMatrix(test, short=10, mid=50, long=100)

plotPerfMatrix <- function(r, short=5, mid=20, long=250) {
  localenv <- environment() # I'm not sure this is a good idea...
  mid.perf <- as.data.frame(coredata(tail(cumsum(tail(r,mid)),1)))
  short.perf <- as.data.frame(coredata(tail(cumsum(tail(r,short)),1)))
  long.perf <- as.data.frame(coredata(tail(cumsum(tail(r,long)),1)))
  per.mat.df <- data.frame(cbind(t(short.perf), t(mid.perf), t(long.perf)))
  colnames(per.mat.df) <- c("short", "mid", "long")
  chart_title <- paste0("Performance Matrix\n(last as of ",end(r),")")
  grenzen <- max(abs(per.mat.df))+.05
  p <- ggplot(per.mat.df, aes(long, mid, label=rownames(per.mat.df)), environment = localenv) + 
    geom_vline(xintercept=0, color="darkgrey", linetype="dashed") + 
    geom_hline(yintercept=0, color="darkgrey", linetype="dashed") +
    geom_point(aes(color=short), size=4) +
    geom_point(shape=1, size=4, colour="darkgrey") +
    geom_text(hjust=0, vjust=-.5,size=4) + 
    scale_colour_gradient2(low="red", high="green", paste0(short,"-Period-\nPerformance"), labels=percent) +
    scale_y_continuous(paste0("Medium Term: ", mid, " periods"), labels=percent, limits=c(-grenzen, +grenzen)) +
    scale_x_continuous(paste0("Long Term: ", long, " periods"), labels=percent, limits=c(-grenzen, +grenzen)) +
    ggtitle(chart_title) + theme_classic() + theme(axis.line=element_blank())
  p
}


