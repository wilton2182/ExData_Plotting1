## plot4.R - for Exploratory Data Analysis Project 1
##           written by Don Wilton
##
## When plot4.R is loaded by source() it automatically generates plot1.png
## in the current working directory. Downloaded files are not removed.
## Once loaded, plot4display() can be invoked to display the histogram in 
## the default graphics display. 
##  
## plot4.R - reproduce 4 plots with the following characteristics:
## - 2 per row and 2 per column. 
## - Top left will be the same as plot2 except:
##   - the y label is "Global Active Power"
## - Bottom left will be the same as plot3 except:
##   - legend will not have a box around it.
## - Top right will be a line graph like plot2 except:
##   - y variable is Voltage and y axis label is default of "Voltage".
##     - y axis has labels at 234, 238, 242, and 246 and ticks every 2.
##   - x axis has label "datetime"
## - Bottom right will be a line graph like top right except:
##   - y variable is Global_reactive_power
##     - y axis has ticks and labels at 0.1, 0.2, 0.3, 0.4, and 0.5.

## Additional requirements on plot4.R:
## - include code for reading the data so the plot can be fully reproduced.
## - also include the code that creates the PNG file.

##  plot4.R will perform the following steps
## - download the data set
## - identify the name of the file to be extracted from the zip file.
## - extract the file from the zip file.
## - load the data into a data frame and do some preprocessing.
##   - convert "?" to NA
##   - convert date and time variables
##   - keep only the data from the dates 2007-02-01 and 2007-02-02
## Create the plots and add annotation, etc.

## Get the zip file 
## This function is unchanged from plot3.R.
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
## This function is unchanged from plot3.R.
dounzip <- function(zipfn){
        ## identify the name of the file to be extracted from the zip file.
        zipfiles <- unzip(zipfn,list=TRUE)
        datafile <- zipfiles$Name[1]
        
        ## extract the file from the zip file.
        unzip(zipfn,exdir = ".")
        datafile        
}

## Load the specified file into a data frame and prepare it for use.
## This function is unchanged from plot3.R.
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

## make4plots() replaces makelineplots() in plot3.R
make4plots <- function(df){
        ## Generate 4 plots with 2 per row and 2 per column.
        par(mfcol = c(2, 2))
        with(df,{                                          # use input dataset
                ## Same as plot2 except y label is "Global Active Power"
                plot(df$DateTime,df$Global_active_power,      
                     type = "l",                              # request a line plot
                     xlab = "",                               # x-axis title
                     ylab = "Global Active Power"             # y-axis title
                )
                ## - Bottom left will be the same as plot3 except:
                ##   - legend will not have a box around it.
                plot(df$DateTime,df$Sub_metering_1,      
                     type = "l",                              # request a line plot
                     xlab = "",                               # y-axis title
                     ylab = "Energy sub metering"             # y-axis title
                )
                lines(df$DateTime,df$Sub_metering_2,
                      type = "l",                             # request a line plot
                      col = "red"                             # request red line
                )
                lines(df$DateTime,df$Sub_metering_3,
                      type = "l",                             # request a line plot
                      col = "blue"                            # request blue line
                )
                legend("topright", 
                       lty = 1, 
                       bty = "n",                             # no box around legend.
                       col = c("black", "red","blue"), 
                       legend = c("Sub_metering_1",
                                  "Sub_metering_2",
                                  "Sub_metering_3")
                )
                ## - Top right will be a line graph like plot2 except:
                ##   - y variable is Voltage and y axis label is default of "Voltage".
                ##     - y axis has labels at 234, 238, 242, and 246 and ticks every 2.
                ##   - x axis has label "datetime"
                plot(df$DateTime,df$Voltage,      
                     type = "l",                              # request a line plot
                     lwd = 0.5,                               # seems to ignore lwd<1
                     xlab = "datetime",                       # x-axis title
                     ylab = "Voltage"                         # y-axis title
                )
                ## - Bottom right will be a line graph like top right except:
                ##   - y variable is Global_reactive_power
                ##     - y axis has ticks and labels at 0.1, 0.2, 0.3, 0.4, and 0.5.
                plot(df$DateTime,df$Global_reactive_power,      
                     type = "l",                              # request a line plot
                     xlab = "datetime",                       # x-axis title
                     ylab = "Global_reactive_power"           # y-axis title
                )
        }
        )
}

## Generate plot4.png in the working directory.
## Step 5 is changed from plot3.R plot3png()
plot4png <- function(){
        print("Step 1 - Download zip file containing data")
        zipfile <- getzip()               # download the zip file
        print("Step 2 - Unzip the data file")
        datafile <- dounzip(zipfile)      # extract the data file 
        print("Step 3 - Load and prepare the data in a data frame")
        epc <- prepdata(datafile)         # load and prep data in data frame
        print("Step 4 - Set up PNG graphics device")
        ## The PNG plot dimensions will have a width and height of 480 pixels.
        png(filename = "plot4.png")       # set up graphics device
        print("Step 5 - Generate 4 plots")
        make4plots(epc)                   # generate 4 plots
        print("Step 6 - Close PNG graphics device")
        dev.off()                         # close graphics device
        
}
## Invoke the PNG generation when plot4.R is loaded by source().
plot4png()

## Optional - Display line graph in default graphics display
## Step 4 is different from plot3.R plot3display()
plot4display <- function(){
        print("Step 1 - Download zip file containing data")
        zipfile <- getzip()               # download the zip file
        print("Step 2 - Unzip the data file")
        datafile <- dounzip(zipfile)      # extract the data file 
        print("Step 3 - Load and prepare the data in a data frame")
        epc <- prepdata(datafile)         # load and prep data in data frame
        print("Step 4 - Generate 4 plots")
        make4plots(epc)                   # generate 4 plots
        
}

