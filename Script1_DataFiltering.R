# Map 1-based optional input ports to variables
cadairydata <- maml.mapInputPort(1) # class: data.frame

# Convert Month to factor, and get substring of first 3 letters of a month, 
# to correct the number of levels in Month column (14 levels in raw data)
cadairydata$Month <- as.factor(substr(cadairydata$Month,1,3))

# Remove unnecessary first 2 columns (Column 0: row index, Year.Month)
cadairydata <- cadairydata[,c(-1,-2)]

# Create function to count number of months from initial month of data series
num.month <- function(Year,Month)
{
  yearMin <- min(Year)  # Find beginning year in data series
  12*(Year - yearMin) + Month - 1   # Count number of month from the beginning
}
# Add column count by month from beginning of data series using function num.month
cadairydata$Month.Count <- num.month(cadairydata$Year,cadairydata$Month.Number)

# Select data.frame to be sent to the output Dataset port
maml.mapOutputPort("cadairydata");