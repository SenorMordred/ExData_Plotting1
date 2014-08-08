
#Download data if it is not in the working directory
if(!file.exists("household_power_consumption.txt")){
  download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip","data.zip")
  unzip("data.zip")
}

#Read and subset the data, put the date in the right format
data<-read.csv("household_power_consumption.txt",sep=";",na.strings="?")
data$Date<-as.Date(data$Date,format="%d/%m/%Y")
data<-subset(data,data$Date %in% c(as.Date("2007-02-01"),as.Date("2007-02-02")))
data$FullDate<-strptime(paste(as.character(data$Date),as.character(data$Time)),format="%Y-%m-%d %H:%M:%S")

#Plot1
png(filename="Plot1.png")
hist(data$Global_active_power,xlab="Global Active Power (kilowatts)",ylab="Frequency",col="red",main="Global Active Power")
dev.off()

#Plot2
png(filename="Plot2.png")
par(mar=c(2,4,2,2))
plot(data$FullDate,data$Global_active_power,type="l",ylab="Global Active Power (kilowatts)",xlab="")
dev.off()

#Plot3
png(filename="Plot3.png")
par(mar=c(2,4,2,2))
with(data,plot(FullDate,Sub_metering_1,type="l",ylab="Energy sub metering"))
with(data,lines(FullDate,Sub_metering_2,col="red"))
with(data,lines(FullDate,Sub_metering_3,col="blue"))
legend("topright", lty=c(1,1,1),col=c("black","red","blue"), legen=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
dev.off()

#Plot4
png(filename="Plot4.png")
par(mar=c(4,4,2,2),mfrow=c(2,2))
plot(data$FullDate,data$Global_active_power,type="l",ylab="Global Active Power",xlab="")
plot(data$FullDate,data$Voltage,ylab="Voltage",xlab="datetime",type="l")
with(data,plot(FullDate,Sub_metering_1,type="l",ylab="Energy sub metering",xlab=""))
with(data,lines(FullDate,Sub_metering_2,col="red"))
with(data,lines(FullDate,Sub_metering_3,col="blue"))
legend("topright", lty=c(1,1,1),col=c("black","red","blue"), legen=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),box.lty=0,inset=c(0,0.01))
plot(data$FullDate,data$Global_reactive_power,type="l",xlab="datetime",ylab="Global_reactive_power")
dev.off()