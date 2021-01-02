library(readr)

# Load data
bist <- as.data.frame(read_csv("/home/selim/Documents/BIST/Result.csv"))
bist <-bist[,c(1,5)]
bist <-na.omit(bist)
bist<-xts(x=bist[,2], order.by = bist[,1])



# Create the forecasts vector to store the predictions
# Put forecast length as 25 working days, approximately a month
windowLength=25
foreLength = length(bist) - windowLength
forecasts <- vector(mode="numeric", length=foreLength)

returns <- vector(mode="numeric", length=foreLength)

for (d in 0:foreLength-1) {
  # Obtain the BIST100 rolling window for this day
  bistMovingAverage = mean(bist[(1+d):(windowLength+d)])
  # If moving average of the previous period is lower than last day, be long else short
  if(bist[windowLength+d]  > bistMovingAverage )
    forecasts[d+1]=1
  else
    forecasts[d+1]=-1
}


#Prepare returns on logarithmic scale
bistReturns = diff(log(bist))
bistReturns[as.character(head(index(bist),1))] = 0
bistForecasts=bist[26:6436]

#Merge returns and forecasts
bistIntersect = merge( bistForecasts[,1], bistReturns, all=F )
bistForecastReturns = bistIntersect[,2] * forecasts

#Prepare combined data with original returns and forecasted returns
bistForecastCurve = log( cumprod( 1 + bistForecastReturns ) )
bistBuyHoldCurve =log( cumprod( 1 + bistIntersect[,2] ) )
bistCombinedCurve = merge( bistForecastCurve, bistBuyHoldCurve, all=F )

xyplot( 
  bistCombinedCurve,
  superpose=T,
  col=c("darkred", "darkblue"),
  lwd=2,
  key=list( 
    text=list(
      c("Moving Average", "Buy & Hold")
    ),
    lines=list(
      lwd=2, col=c("darkred", "darkblue")
    )
  )
)
