
##constants
dateFormat<-"%d-%m-%Y"
initialDate<-strptime("01-02-2007",dateFormat)
endDate<-strptime("02-02-2007", dateFormat)

readData<- function(){
      inputFile <- "household_power_consumption.txt"
      ##open file
      con  <- file(inputFile, open = "r")
      
      ##example line
      ##Date;Time;Global_active_power;Global_reactive_power;Voltage;Global_intensity;Sub_metering_1;Sub_metering_2;Sub_metering_3
      ##16/12/2006;17:24:00;4.216;0.418;234.840;18.400;0.000;1.000;17.000
      
      
      ##Date & time
      dateTime<-c()
      ##global active power #3
      globalActivePower<-numeric()
      
      
      
      
      ##read lines
      ##skip first
      readLines(con, n = 1, warn = FALSE)
      line<-1
      upperLimitReach<-FALSE
      while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0 & !upperLimitReach) {
            line<-line+1
            if(line %% 10000 ==0){
                  message(paste("read lines:",line))
            }
            rowVector<-strsplit(oneLine,";")[[1]]
            rowDate<-strptime(rowVector[1], "%d/%m/%Y")
            
            ##check date
            if(rowDate>endDate){
                  upperLimitReach=TRUE
            }else if(rowDate>=initialDate){
                  ##use row data
                  ##dateTime
                  rowDateTime<-strptime(paste(rowVector[1],rowVector[2]), "%d/%m/%Y %H:%M:%S")
                  dateTime<-c(dateTime, rowDateTime)
                  ##global active power #3
                  globalActivePower<-c(globalActivePower, as.numeric(rowVector[3]))
                  
            }
            
            
      }  
      message("end reading")
      ##Date;Time;Global_active_power;Global_reactive_power;Voltage;Global_intensity;
      ##Sub_metering_1;Sub_metering_2;Sub_metering_3
      resultData<-data.frame("date_time"=dateTime,
            "Global_active_power"=globalActivePower)
      
      ##close file
      close(con)
      resultData
}


##create data
d1<-readData()
##open device
png(filename="plot1.png", width=480, height=480)
##create plot
hist(d1$Global_active_power, 
     col="red", 
     xlab="Global Active Power (kilowatts)", 
     main="Global Active Power")

##close device
dev.off()
