#I assume the required file "household_power_consumption.txt"
#is in current working directory, if not change the current
#working directory using setwd()
infile <- file("household_power_consumption.txt", "r")

#ignore the header line
readLines(infile, 1)

powerData <- data.frame(Date = character(0)
                       ,Time = character(0)
                       ,Global_Active_Power = character(0)
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
        powerData[nData + i, ] <- strsplit(reqRawInput[[i]], split = ";", fixed = TRUE)[[1]][c(1,2,3)]
    }
    nData <- nData + n
}

close(infile)

powerData[,"Date_Time"] <- as.data.frame(strptime(paste(powerData[,"Date"], powerData[,"Time"], sep = " "), format = "%d/%m/%Y %H:%M:%S"))
powerData[,"Date"] <- NULL
powerData[,"Time"] <- NULL
powerData[,"Global_Active_Power"] <- as.numeric(powerData[,"Global_Active_Power"])

png(filename = "plot2.png", width = 480, height = 480, units = "px")
par(cex = 1.0)
plot(powerData$Date_Time
    ,powerData$Global_Active_Power
    ,type = "l"
    ,xlab = ""
    ,ylab = "Global Active Power (kilowatts)"
    )
dev.off()