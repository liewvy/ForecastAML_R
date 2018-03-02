# Map 1-based optional input ports to variables
cadairydata <- maml.mapInputPort(1) # class: data.frame

# Function to de-trend and standardize a time series
ts.detrend <- function(ts, Time, min.length = 3){
  
  # Error messages if they are NULL  
  messages <- c('ERROR1: ts.detrend requires arguments ts and Time to have the same length',
                'ERROR2: ts.detrend requires argument ts to be of type numeric',
                paste('ERROR3: ts.detrend has encountered a time series with length less than', as.character(min.length)),
                'ERROR4: ts.detrend has encountered a Time argument not of class POSIXct',
                'ERROR5: Detrend regression has failed in ts.detrend',
                'ERROR6: Exception occurred in ts.detrend while standardizing time series in function ts.detrend'
  )
  # Create a vector of zeros to return as a default in some cases
  zerovec  <- rep(length(ts), 0.0)
  
  # The input arguments are not of the same length, return ts and quit
  if(length(Time) != length(ts)) {warning(messages[1]); return(ts)}
  
  # If the ts is not numeric, just return a zero vector and quit
  if(!is.numeric(ts)) {warning(messages[2]); return(zerovec)}
  
  # If the ts is too short, just return it and quit
  if((ts.length <- length(ts)) < min.length) {warning(messages[3]); return(ts)}
  
  # Check that the Time variable is of class POSIXct
  if(class(cadairydata$Time)[[1]] != "POSIXct") {warning(messages[4]); return(ts)}
  
  # De-trend the time series by using a linear model
  ts.frame  <- data.frame(ts = ts, Time = Time)
  tryCatch({ts <- ts - fitted(lm(ts ~ Time, data = ts.frame))},
           error = function(e){warning(messages[5]); zerovec})
  
  tryCatch( {stdev <- sqrt(sum((ts - mean(ts))^2))/(ts.length - 1)
  ts <- ts/stdev},
  error = function(e){warning(messages[6]); zerovec})
  
  ts
}  
# Apply the detrend.ts function to the variables of interest
df.detrend <- data.frame(lapply(cadairydata[, 4:7], ts.detrend, cadairydata$Time))

# Plot the results to look at the relationships
plot(cadairydata[,c(4:7)], main = "Scatterplots before detrend")
plot(df.detrend[,c(1:4)], main = "Scatterplots after detrend", col = "blue");

# Select data.frame to be sent to the output Dataset port
maml.mapOutputPort("df.detrend");