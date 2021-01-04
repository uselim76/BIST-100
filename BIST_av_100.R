library(ggplot2)
library(readr)
library(quantmod)
library(rugarch)
library(lattice)
library(timeSeries)

bist <- as.data.frame(read_csv("/home/selim/Documents/BIST/Result.csv"))
bist <-bist[,c(1,5)]
bist <-na.omit(bist)
bist<-xts(x=bist[,2], order.by = bist[,1])


bistReturns = diff(log(bist))
bistReturns[as.character(head(index(bist),1))] = 0


# Create the forecasts vector to store the predictions
windowLength =500
foreLength = length(bistReturns) - windowLength
forecasts <- vector(mode="character", length=foreLength)



for (d in 0:foreLength) {
  # Obtain the BIST100 rolling window for this day
  bistReturnsOffset = bistReturns[(1+d):(windowLength+d)]
  
  # Fit the ARIMA model
  final.aic <- Inf
  final.order <- c(0,0,0)
  for (p in 0:5) for (q in 0:5) {
    if ( p == 0 && q == 0) {
      next
    }
    
    arimaFit = tryCatch( arima(bistReturnsOffset, order=c(p, 0, q)),
                         error=function( err ) FALSE,
                         warning=function( err ) FALSE )
    
    if( !is.logical( arimaFit ) ) {
      current.aic <- AIC(arimaFit)
      if (current.aic < final.aic) {
        final.aic <- current.aic
        final.order <- c(p, 0, q)
        final.arima <- arima(bistReturnsOffset, order=final.order)
      }
    } else {
      next
    }
  }
  # Specify and fit the GARCH model
  spec = ugarchspec(
    variance.model=list(garchOrder=c(1,1)),
    mean.model=list(armaOrder=c(final.order[1], final.order[3]), include.mean=T),
    distribution.model="sged"
  )
  fit = tryCatch(
    ugarchfit(
      spec, bistReturnsOffset, solver = 'hybrid'
    ), error=function(e) e, warning=function(w) w
  )
  
  # If the GARCH model does not converge, set the direction to "long" else
  # choose the correct forecast direction based on the returns prediction
  # Output the results to the screen and the forecasts vector
  if(is(fit, "warning")) {
    forecasts[d+1] = paste(index(bistReturnsOffset[windowLength]), 1, sep=",")
    print(paste(index(bistReturnsOffset[windowLength]), 1, sep=","))
  } else {
    fore = ugarchforecast(fit, n.ahead=1)
    ind = fore@forecast$seriesFor
    forecasts[d+1] = paste(colnames(ind), ifelse(ind[1] < 0, -1, 1), sep=",")
    print(paste(colnames(ind), ifelse(ind[1] < 0, -1, 1), sep=",")) 
  }
}
  
write.csv(forecasts, file="forecasts_test500_I.csv", row.names=FALSE)  

bistArimaGarch = as.xts( 
  read.zoo(
      file="/home/selim/Documents/BIST/forecasts_new500_I.csv", format="%Y-%m-%d", header=F, sep=","
     )
  )

# Data from May 2013
# bistArimaGarch=bistArimaGarch[4026:5937]
bistIntersect = merge( bistArimaGarch[,1], bistReturns, all=F )
bistArimaGarchReturns = bistIntersect[,1] * bistIntersect[,2]

bistArimaGarchCurve = log( cumprod( 1 + bistArimaGarchReturns ) )
bistBuyHoldCurve =log( cumprod( 1 + bistIntersect[,2] ) )
bistCombinedCurve = merge( bistArimaGarchCurve, bistBuyHoldCurve, all=F )

xyplot( 
  bistCombinedCurve,
     superpose=T,
     col=c("darkred", "darkblue"),
     lwd=2,
     key=list( 
         text=list(
             c("ARIMA+GARCH", "Buy & Hold")
           ),
        lines=list(
             lwd=2, col=c("darkred", "darkblue")
           )
       )
   )
  
