#I assume the required file "household_power_consumption.txt"
#is in current working directory, if not change the current
#working directory using setwd()
infile <- file("household_power_consumption.txt", "r")

#ignore the header line
readLines(infile, 1)

powerData <- data.frame(Date = character(0)
                       ,Time = character(0)
                       ,Global_Active_Power = character(0)
                       ,Global_Reactive_Power = character(0)
                       ,Voltage = character(0)
                       ,Sub_metering_1 = character(0)
                       ,Sub_metering_2 = character(0)
                       ,Sub_metering_3 = character(0)
                       ,stringsAsFactors = FALSE
                       )
nData <- 0

repeat{
    rawInput <- readLines(infile, 10)
    if(length(rawInput) == 0) break

    reqRawInput <- grep("^(1|01|2|02)/(2|02)/2007", rawInput, value = TRUE)
    n <- length(reqRawInput)
    if(n == 0) next

    for(i in 1:n){
        powerData[nData + i, ] <- strsplit(reqRawInput[[i]], split = ";", fixed = TRUE)[[1]][-6]
    }
    nData <- nData + n
}

close(infile)

powerData[,"Date_Time"] <- as.data.frame(strptime(paste(powerData[,"Date"], powerData[,"Time"], sep = " "), format = "%d/%m/%Y %H:%M:%S"))
powerData[,"Date"] <- NULL
powerData[,"Time"] <- NULL
powerData[,"Global_Active_Power"] <- as.numeric(powerData[,"Global_Active_Power"])
powerData[,"Global_Reactive_Power"] <- as.numeric(powerData[,"Global_Reactive_Power"])
powerData[,"Voltage"] <- as.numeric(powerData[,"Voltage"])
powerData[,"Sub_metering_1"] <- as.numeric(powerData[,"Sub_metering_1"])
powerData[,"Sub_metering_2"] <- as.numeric(powerData[,"Sub_metering_2"])
powerData[,"Sub_metering_3"] <- as.numeric(powerData[,"Sub_metering_3"])

png(filename = "plot4.png", width = 480, height = 480, units = "px")
par(mfrow = c(2,2))
par(cex = 0.75) #set cex after mfrow, as order matters
par(mgp = c(2.5,1,0))
par(mar = c(5.1,4.1,1.1,1.1))
plot(powerData$Date_Time
    ,powerData$Global_Active_Power
    ,type = "l"
    ,xlab = ""
    ,ylab = "Global Active Power"
    )
plot(powerData$Date_Time
    ,powerData$Voltage
    ,type = "l"
    ,xlab = "datetime"
    ,ylab = "Voltage"
    )
plot(powerData$Date_Time
    ,powerData$Sub_metering_1
    ,type = "l"
    ,col = "black"
    ,xlab = ""
    ,ylab="Energy sub metering"
    )
lines(powerData$Date_Time
     ,powerData$Sub_metering_2
     ,col = "red"
     )
lines(powerData$Date_Time
     ,powerData$Sub_metering_3
     ,col = "blue"
     )
legend("topright"
      ,legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
      ,lty = 1
      ,col = c("black","blue","red")
      ,bty = "n"
      ,cex = 1
      )
plot(powerData$Date_Time
    ,powerData$Global_Reactive_Power
    ,type = "l"
    ,xlab = "datetime"
    ,ylab = "Global_reactive_power"
    )
dev.off()