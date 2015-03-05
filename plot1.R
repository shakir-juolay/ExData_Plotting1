#I assume the required file "household_power_consumption.txt"
#is in current working directory, if not change the current
#working directory using setwd()
infile <- file("household_power_consumption.txt", "r")

#ignore the header line
readLines(infile, 1)

powerData <- data.frame(Date = character(0)
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
        powerData[nData + i, ] <- strsplit(reqRawInput[[i]], split = ";", fixed = TRUE)[[1]][c(1,3)]
    }
    nData <- nData + n
}

close(infile)

powerData[,"Date"] <- as.Date(powerData[,"Date"], format = "%d/%m/%Y")
powerData[,"Global_Active_Power"] <- as.numeric(powerData[,"Global_Active_Power"])

png(filename = "plot1.png", width = 480, height = 480, units = "px")
par(cex = 1.0)
hist(powerData$Global_Active_Power
    ,col = "red"
    ,main = "Global Active Power"
    ,xlab = "Global Active Power (kilowatts)"
    )
dev.off()