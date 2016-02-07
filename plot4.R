plot4 <- function() {
        
        ## get a tidy data set with a subset of the data
        energyData <- makeTidyData()
        
        ## open the png device
        png(filename = "plot4.png", width = 480, height = 480, units = "px")
        
        ## we need a 4 plot group of graphs
        ## graphs will be arranged by col, then row, 2x2
        par(mfcol = c(2,2))
        
        #plot1 r1,c1
        with(energyData,
             plot(datetime, 
                  globalactivepower,
                  type = "l",
                  ylab = "Global Active Power (kilowatts)",
                  xlab = ""
             ))
        
        ## plot2 r2,c1
        with(energyData,
             plot(datetime, 
                  submetering1,
                  type = "l",
                  ylab = "Energy sub metering",
                  xlab = ""
             )
        )
        ## create the overlay
        lines(energyData$datetime, energyData$submetering2, col = "red")
        lines(energyData$datetime, energyData$submetering3, col = "blue")
        ## create the legend
        legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col = c("black", "red", "blue"), lty = 1)
        
        #plot3 r1,c2
        with(energyData,
             plot(datetime, 
                  voltage,
                  type = "l",
                  ylab = "Voltage",
                  xlab = "datetime"
             )
        )
        
        # plot4 r2,c2
        with(energyData,
             plot(datetime, 
                  globalreactivepower,
                  type = "l",
                  ylab = "Global_reactive_power",
                  xlab = "datetime"
             )
        )
        
        ## close the png graphic device
        dev.off()
}

makeTidyData <- function() {
        ## NOTE this function could obviously be improved to take
        ## a filepath
        ## either a subset of the or return the entire file contents
        ## separator chars
        ## date formats,
        ## but i'll save that for another time
        
        ## load data
        ## read header row first
        header <- read.table("household_power_consumption.txt", header = FALSE, sep = ";"
                             , quote = "", as.is = TRUE, na.strings = "?", stringsAsFactors = FALSE, nrows = 1)
        
        ## tidy the header vars
        header <- gsub("[^a-z0-9]", "", tolower(header))
        
        ## only get the data corresponding to 01-Feb-2007 to 02-Feb-2007
        ## add tidy header vars to dataset
        energyData <- read.table("household_power_consumption.txt", header = FALSE, sep = ";"
                                 , quote = "", as.is = TRUE, na.strings = "?", stringsAsFactors = FALSE,
                                 skip = 66637, nrows = (69518 - 66638), col.names = header)
        
        ## convert the Date column to Date class
        energyData$date <- as.Date(energyData$date, "%d/%m/%Y")
        
        ## add a new field that combines date and time
        energyData$datetime <- strptime(paste(energyData$date, energyData$time), "%Y-%m-%d %H:%M:%S")
        
        return(energyData)
}