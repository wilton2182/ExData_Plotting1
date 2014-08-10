## plot1.R - for Exploratory Data Analysis Project 1
##         - written by Don Wilton
##
## When plot1.R is loaded by source() it automatically generates plot1.png
## in the current working directory. Downloaded files are not removed.
## Once loaded, plot1display() can be invoked to display the histogram in 
## the default graphics display
##
## plot1.R - reproduce a plot with the following characteristics:
## - A histogram of the frequency of Global_active_power values.
## - The data will be for the time period 2007-02-01 and 2007-02-02.
## - The dataset to use is available on the course web site:
##   https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
## - The plot will be constructed using the base plotting system.
## - The plot graphics will have the following characteristics:
##   - Title = Global Active Power.
##   - x-axis title - Global Active Power (kilowatts).
##   - y-axis title - Frequency.  
##   - The bars of the histogram will be red.
##   - The y-axis scale will have range 0-1200 with tic marks every 200.
##   - The x-axis scale will have range 0-6 with tic marks every 2.
##   - The histogram should have bars that span a range of 0.5 kilowatts.
## - The plot will be saved in plot1.png in the working directory.
## - The PNG plot dimensions will have a width and height of 480 pixels.

## Additional requirements on plot1.R:
## - include code for reading the data so the plot can be fully reproduced.
## - also include the code that creates the PNG file.

##  plot1.R will perform the following steps
## - download the data set
## - identify the name of the file to be extracted from the zip file.
## - extract the file from the zip file.
## - load the data into a data frame and do some preprocessing.
##   - convert "?" to NA
##   - convert date and time variables
##   - keep only the data from the dates 2007-02-01 and 2007-02-02
## Plot the histogram and add annotation, etc.

## Get the zip file
getzip <- function(){
        ## Download the data file from the course web site to the working directory.
        ## The working directory is assumed to be where this script resides.
        fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        localfile <- "./household_power_consumption.zip"
        setInternet2(TRUE)
        download.file(fileUrl,destfile=localfile)
        localfile
}

## Unzip the specified file and return name of extracted file.
dounzip <- function(zipfn){
        ## identify the name of the file to be extracted from the zip file.
        zipfiles <- unzip(zipfn,list=TRUE)
        datafile <- zipfiles$Name[1]
        
        ## extract the file from the zip file.
        unzip(zipfn,exdir = ".")
        datafile        
}

## Load the specified file into a data frame and prepare it for use.
prepdata <- function(datfn){
        ## - load the data into a data frame
        ## - convert "?" to NA
        df <- read.table(datfn, header=TRUE, sep=";",na.strings="?",stringsAsFactors=FALSE)
        
        ## do some preprocessing...
        ## - convert date and time variables
        df$DateTime <- paste(df$Date, df$Time)
        df$Date <-  as.Date(df$Date, format="%d/%m/%Y") 
        
        ## - keep only the data from the dates 2007-02-01 and 2007-02-02
        df <- df[(df$Date >= as.Date("2007/02/01","%Y/%m/%d") & df$Date <= as.Date("2007/02/02","%Y/%m/%d")), ]
        df$DateTime <- strptime(df$DateTime, format="%d/%m/%Y %H:%M:%S")
        df
}

makehist <- function(df){
        ## Generate the histogram of the frequency of Global_active_power values.
        ## - The plot will be constructed using the base plotting system.
        ## - The plot graphics will have the following characteristics:
        ##   - These characteristics occur by default.
        ##     - y-axis title - Frequency. (default for histogram)
        ##     - The y-axis scale will have range 0-1200 with tic marks every 200.
        ##     - The x-axis scale will have range 0-6 with tic marks every 2.
        ##     - The histogram should have bars that span a range of 0.5 kilowatts.
        ##   - These characteristics need to be done explicitly.
        ##     - Title = Global Active Power.
        ##     - x-axis title - Global Active Power (kilowatts).
        ##     - The bars of the histogram will be red.
        
        hist(df$Global_active_power, 
             col = "red",                             # color bars red 
             main = "Global Active Power",            # main title
             xlab = "Global Active Power (kilowatts)" # x-axis title
        )
        
}

## Generate plot1.png in the working directory.
plot1png <- function(){
        print("Step 1 - Download zip file containing data")
        zipfile <- getzip()               # download the zip file
        print("Step 2 - Unzip the data file")
        datafile <- dounzip(zipfile)      # extract the data file 
        print("Step 3 - Load and prepare the data in a data frame")
        epc <- prepdata(datafile)         # load and prep data in data frame
        print("Step 4 - Set up PNG graphics device")
        ## The PNG plot dimensions will have a width and height of 480 pixels.
        png(filename = "plot1.png")       # set up graphics device
        print("Step 5 - Generate histogram")
        makehist(epc)                     # generate histogram
        print("Step 6 - Close PNG graphics device")
        dev.off()                         # close graphics device
        
}
## Invoke the PNG generation when plot1.R is loaded by source().
plot1png()

## Optional - Display histogram in default graphics display
plot1display <- function(){
        print("Step 1 - Download zip file containing data")
        zipfile <- getzip()               # download the zip file
        print("Step 2 - Unzip the data file")
        datafile <- dounzip(zipfile)      # extract the data file 
        print("Step 3 - Load and prepare the data in a data frame")
        epc <- prepdata(datafile)         # load and prep data in data frame
        print("Step 4 - Generate histogram")
        makehist(epc)                     # generate histogram
        
}

