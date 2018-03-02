# Map 1-based optional input ports to variables
cadairydata <- maml.mapInputPort(1) # class: data.frame

# Create function for transformation, to compute log
# of the input value times a multiplier (default as 1)
log.transform <- function(invec, multiplier = 1) 
{
  # Warning messages for error in the conversion
  warningmessages <- c("Error1: Non-numeric data",
                       "Error2: Data less than 0",
                       "Error3: Multiplier is not a scalar",
                       "Error4: Invalid time series value"
  )
  
  # Warning Error1 if any input arguments is non numeric
  if(!is.numeric(invec) | !is.numeric(multiplier)) 
  {warning(warningmessages[1]); return(NA)} 
  
  # Warning Error2 if value < 0   
  if(any(invec < 0.0) | any(multiplier < 0.0)) 
  {warning(warningmessages[2]); return(NA)}
  
  # Warning Error3 if multiplier is not scalar  
  if(length(multiplier) != 1) 
  {warning(warningmessages[3]); return(NA)}
  
  # Use tryCatch for any exception in execution of 
  # multiplication and log conversion
  tryCatch(log(multiplier * invec), error = function(e)
  {warning(warningmessages[4]); NA})
}

# Multipliers for the corresponding columns of production data
multipliers  <- list(1.0, 6.5, 1000.0, 1000.0)

# Transform 4 columns of production data with corresponding multipliers
cadairydata[, 4:7] <- Map(log.transform, cadairydata[, 4:7], multipliers)

# Remove rows converted to NA values
cadairydata <- na.omit(cadairydata)  

# Create new column Time as POSIXct object
Sys.setenv(TZ = "PST8PDT")
cadairydata$Time <- as.POSIXct(strptime(paste(
  as.character(cadairydata$Year), "-", 
  as.character(cadairydata$Month.Number), 
  "-01 00:00:00", sep = ""), 
  "%Y-%m-%d %H:%M:%S"))

# Select data.frame to be sent to the output Dataset port
maml.mapOutputPort("cadairydata");