##constants
dateFormat<-"%d-%m-%Y"
initialDate<-strptime("01-02-2007",dateFormat)
endDate<-strptime("02-02-2007", dateFormat)

readData3<- function(){
      inputFile <- "household_power_consumption.txt"
      ##open file
      con  <- file(inputFile, open = "r")
      
      ##example line
      ##Date;Time;Global_active_power;Global_reactive_power;Voltage;Global_intensity;Sub_metering_1;Sub_metering_2;Sub_metering_3
      ##16/12/2006;17:24:00;4.216;0.418;234.840;18.400;0.000;1.000;17.000
      
      
      ##Date & time
      dateTime<-c()
      ##sub metering 1 #7
      subMetering1<-c() 
      ##sub metering 2 #8
      subMetering2<-c()
      ##sub metering 3 #9
      subMetering3<-c()
      
      
      
      ##read lines
      ##skip first
      readLines(con, n = 1, warn = FALSE)
      line<-1
      upperLimitReach<-FALSE
      initialization<-TRUE
      while (length(oneLine <- readLines(con, n = 1, warn = F)) > 0 & !upperLimitReach) {
            line<-line+1
            if(line %% 10000 ==0){
                  message(paste("read lines:",line))
            }
            rowVector<-strsplit(oneLine,";")[[1]]
            rowDate<-strptime(rowVector[1], "%d/%m/%Y")
            
            ##check date 
            if(rowDate>endDate){
                  upperLimitReach=TRUE
            }else if(rowDate>=initialDate & rowDate<=endDate){
                  ##use row data
                  ##dateTime
                  rowDateTime<-strptime(paste(rowVector[1],rowVector[2]), "%d/%m/%Y %H:%M:%S")
                  if(initialization){
                        dateTime<-c(rowDateTime)
                        initialization<-FALSE
                  }else{
                        dateTime<-c(dateTime,rowDateTime)
                        
                  }
                  
                  ##sub metering 1 #7
                  subMetering1<-c(subMetering1,as.numeric(rowVector[7])) 
                  ##sub metering 2 #8
                  subMetering2<-c(subMetering2,as.numeric(rowVector[8]))
                  ##sub metering 3 #9
                  subMetering3<-c(subMetering3,as.numeric(rowVector[9]))
                  
            }
            
            
      }      
      ##Date;Time;Global_active_power;Global_reactive_power;Voltage;Global_intensity;
      ##Sub_metering_1;Sub_metering_2;Sub_metering_3
      resultData<-data.frame("date_time"=dateTime,
                             "Sub_metering_1"=subMetering1,
                             "Sub_metering_2"=subMetering2,
                             "Sub_metering_3"=subMetering3)
      
      ##close file
      close(con)
      resultData
}

##create data
d3<-readData3()
message("end read data")
##open device
png(filename="plot3.png", width=480, height=480)
##create plot
plot(d3$date_time,d3$Sub_metering_1, ylab="Energy sub metering", xlab="",type="n")

lines(d3$date_time, d3$Sub_metering_1, col="black",type="l")
lines(d3$date_time,type="l", d3$Sub_metering_2, col="red")
lines(d3$date_time,type="l", d3$Sub_metering_3, col="blue")

legend("topright", 
       legend = c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"), 
       text.width = strwidth("Sub_metering_2"), 
       lty = 1, 
       xjust = 1, yjust = 1,
       col=c("black","red","blue"))

##close device
dev.off()

