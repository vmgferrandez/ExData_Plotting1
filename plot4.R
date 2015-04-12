#
# PLOT 3
#
require(tools)

library(tools)

#
# input data file in txt format must exist in the working directory, anyway the function
# handle the zip format also, and even the file name as parameter.
plot4 <- function(in_file = "household_power_consumption.txt", i_date_str = "2007-02-01", e_date_str= "2007-02-02") {
    
    
    if (file_ext(in_file) == "zip") {
        in_file_name <- tempfile(fileext = "txt")
        unz(in_file, in_file_name)
    } else if (file_ext(in_file) == "txt") {
        in_file_name <- in_file
    }
    # read the txt file and build a dataframe
    data_raw <- read.table(in_file_name, sep = ";", dec = ".", header = TRUE, stringsAsFactors= FALSE)
    # join data and time info in a POSIXlt date+time variable
    data_raw$Date_time <- as.POSIXct(strptime(paste (data_raw$Date,data_raw$Time), format = "%d/%m/%Y %T" ))
    # delete some columns not needed for this plot
    drops <- c("Date","Time")
    data_raw <- data_raw[,!(names(data_raw) %in% drops)]
    # get the data within the dates to be plotted
    i_date <- as.Date(i_date_str, "%Y-%m-%d")
    e_date <- as.Date(e_date_str, "%Y-%m-%d")
    data_raw <- data_raw[as.Date(data_raw$Date_time) >= i_date & as.Date(data_raw$Date_time) <= e_date, ]
    # use numeric values !!
    data_raw$Global_active_power <- as.numeric(data_raw$Global_active_power)
    data_raw$Global_reactive_power <- as.numeric(data_raw$Global_reactive_power)
    data_raw$Global_intensity <- as.numeric(data_raw$Global_intensity) 
    data_raw$Voltage <- as.numeric(data_raw$Voltage)   
    data_raw$Sub_metering_1 <- as.numeric(data_raw$Sub_metering_1)
    data_raw$Sub_metering_2 <- as.numeric(data_raw$Sub_metering_2)
    data_raw$Sub_metering_3 <- as.numeric(data_raw$Sub_metering_3)    
    # create the plot and save it
    
    png (filename = "plot4.png", width = 480, height=480)
    # four plots in a 2x2 grid
    par(mfrow = c(2, 2))
    with (data_raw, {
        # First Plot
        plot (Global_active_power ~ Date_time,xaxt="n", type = "l", ylab = "Global Active Power", xlab="" )
        # create the labels for the x-axis as requested
        axis.POSIXct(side=1,at=seq(i_date,e_date +1 ,by="days"),format="%a")
        
        # Second Plot
        plot (Voltage ~ Date_time,xaxt="n", type = "l", ylab = "Voltage", xlab="datetime" )
        # create the labels for the x-axis as requested
        axis.POSIXct(side=1,at=seq(i_date,e_date +1 ,by="days"),format="%a")
        
        # Third Plot
        plot (Sub_metering_1 ~ Date_time,xaxt="n", type = "l", ylab = "Energy sub metering", xlab="" )
        lines(Sub_metering_2 ~ Date_time, col = "red")
        lines(Sub_metering_3 ~ Date_time, col = "blue")
        #create the legend
        legends = c("Sub_metering_1","Sub_metering_2","Sub_metering_3")
        legend (x="topright", legend = legends , col = c("black", "red", "blue"),lty=c(1,1,1), lwd=c(1,1,1),bty = "n")
        # create the labels for the x-axis as requested
        axis.POSIXct(side=1,at=seq(i_date,e_date +1 ,by="days"),format="%a")
        
        
        # Fourth Plot
        plot (Global_reactive_power ~ Date_time,xaxt="n", type = "l",  xlab="datetime" )
        # create the labels for the x-axis as requested
        axis.POSIXct(side=1,at=seq(i_date,e_date +1 ,by="days"),format="%a")
        
        
    })

    dev.off()



}