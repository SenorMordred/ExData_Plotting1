#Download data if it is not in the working directory
if(!file.exists("summarySCC_PM25.rds") || !file.exists("Source_Classification_Code.rds")){
  download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip","data.zip")
  unzip("data.zip")
}

data<-readRDS("summarySCC_PM25.rds")
ClassificationCodes<-readRDS("Source_Classification_Code.rds")

require(ggplot2)

#Question1
temp<-tapply(data$Emissions,data$year,sum,na.rm=T)
png(filename="PlotForQuestion1.png")
par(mfrow=c(1,2))
barplot(temp,col="red",ylab="Total Emissions in the US",xlab="Year")
barplot(log(temp,10),col="red",ylab="logarithm of Total Emissions in the US to base 10",xlab="Year")
abline(lm(log(temp,10)~names(temp)), lwd=3)
dev.off()

#Question2
balt<-subset(data,data$fips=="24510")
temp<-tapply(balt$Emissions,balt$year,sum,na.rm=T)
png(filename="PlotForQuestion2.png")
par(mfrow=c(1,2))
barplot(temp,col="red",ylab="Total Emissions in Baltimore",xlab="Year")
barplot(log(temp,10),col="red",ylab="logarithm of Total Emissions in the US to base 10",xlab="Year")
abline(lm(log(temp,10)~names(temp)), lwd=3)
dev.off()

#Question3
balt$type<-as.factor(balt$type)
png(filename="PlotForQuestion3.png",width=1024,height=768)
qplot(year,Emissions,data=balt,facets=.~type,geom=c("point","smooth"),method="lm",ylab="Emissions in Baltimore")+coord_cartesian(ylim=c(0,400))
dev.off()

#Question4
coalcomb<-subset(ClassificationCodes,ClassificationCodes$SCC.Level.One %in% c("External Combustion Boilers","Internal Combustion Engines","Stationary Source Fuel Combustion"))
coalcomb<-subset(coalcomb,grepl("Coal",coalcomb$SCC.Level.Three,fixed=T))
coaldata<-subset(data,data$SCC %in% coalcomb$SCC)
png(filename="PlotForQuestion4.png")
qplot(year,Emissions,data=coaldata,facets=type~.,geom=c("point","smooth"),method="lm",col=type,ylab="Emissions from coal combustion")+coord_cartesian(ylim=c(0,7500))
dev.off()

#Question5
carbalt<-subset(balt,balt$type=="ON-ROAD")
png(filename="PlotForQuestion5.png",width=1024,height=768)
qplot(year,Emissions,data=carbalt,facets=.~type,geom=c("point","smooth"),method="lm",main = "Smoother in blue, development of the median in red",ylab="Emissions from motor vehicles")+coord_cartesian(ylim=c(0,3))+stat_summary(fun.y=median, colour="red", geom="line", aes(group = 1))#+geom_line(stat = "hline", yintercept = "mean", colour="red")
dev.off()

#Question6
cardata<-subset(data,data$type=="ON-ROAD")
cardata<-subset(cardata,cardata$fips %in% c("06037","24510"))
require(plyr)
cardata$fips<-revalue(cardata$fips,c("06037"="Los Angeles","24510"="Baltimore"))
png(filename="PlotForQuestion6.png",width=1024,height=768)
qplot(year,Emissions,data=cardata,facets=type~fips,geom=c("point","smooth"),method="lm",main = "Smoother in blue, development of the median in red",ylab="Emissions from motor vehicles")+coord_cartesian(ylim=c(0,50))+stat_summary(fun.y=median, colour="red", geom="line", aes(group = 1))
dev.off()