plot2 <- function() {
        
        ## get a tidy data set with a subset of the data
        energyData <- makeTidyData()
        
        ## open the png device
        png(filename = "plot2.png", width = 480, height = 480, units = "px")
        
        ## create the plot
        with(energyData,
             plot(datetime, 
                  globalactivepower,
                  type = "l",
                  ylab = "Global Active Power (kilowatts)",
                  xlab = ""
        ))
        
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
        fileName <- file("household_power_consumption.txt")
        
        energyData <- read.table(text = grep("^[1-2]/2/2007", readLines(fileName), value = TRUE), 
                                 header = FALSE, sep = ";", quote = "", as.is = TRUE, 
                                 na.strings = "?", stringsAsFactors = FALSE, col.names = header)
        
        close(fileName)
        
        ## convert the Date column to Date class
        energyData$date <- as.Date(energyData$date, "%d/%m/%Y")
        
        ## add a new field that combines date and time
        energyData$datetime <- strptime(paste(energyData$date, energyData$time), "%Y-%m-%d %H:%M:%S")
        
        return(energyData)
}