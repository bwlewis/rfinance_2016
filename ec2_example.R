# A custome reducer/combiner function that plots the rate at which
# results are received.
plt <- function()
{
  n <- 0          # number of results so far
  t <- Sys.time() # time of last result
  i <- 0
  val <- c()
  times <- c()
  function(x, y)
  {
    i <<- i + 1
    dt <- as.numeric(difftime(Sys.time(), t, units="secs"))
    if(dt > 5)               # Every 5 seconds or so...
    {
      t <<- Sys.time()       # update time
      n <<- n + i            # update number of results
      rate <- 12 * i / dt    # per minute result rate 
      val <<- c(val, rate)
      times <<- c(times, dt)
      i <<- 0
      x <- cumsum(times) / 60
      plot(x, val, col=4, ylab="", xlab="minutes", main="Results per minute")
      lines(lowess(x, val), col="red")
      Sys.sleep(0.1)         # plot refresh
    }
    c(x, y)                  # return a value to foreach
  }
}














library(doRedis)
registerDoRedis("RJOBS", host="172.31.10.138")

setProgress(TRUE)
ans <- foreach(j=1:5000, .combine=plt(), .inorder=FALSE) %dopar%
{
  x <- matrix(rnorm(1e6), 1000)
  sum(svd(x)$d)
}









