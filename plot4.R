library(ggplot2)
library(plyr)
library(data.table)
library(grid)
library(gridExtra)
library(scales)
setwd("/Users/pesto/Documents/school/Coursera/eda")
HPC <- read.table("household_power_consumption.txt",sep=";",skip=23437,nrows=50000,header=FALSE)
hpcdf<-rbind(HPC[HPC$V1=="1/2/2007",],HPC[HPC$V1=="2/2/2007",])
names(hpcdf)<-c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")
hpcdf$Date<-as.Date(hpcdf$Date,"%d/%m/%Y")
hpcdf$grap<-as.numeric(as.character(factor(hpcdf$Global_reactive_power)))
hpcdf$gap<-as.numeric(as.character(factor(hpcdf$Global_active_power)))
hpcdf$vfac<-as.numeric(as.character(factor(hpcdf$Voltage)))
hpcdf$dtime<-strptime(paste(hpcdf$Date,hpcdf$Time), "%Y-%m-%d %H:%M:%S")

hpplot3<-hpcdf
hpplot3$ctime<-as.character(hpplot3$dtime)
hpplot3$Sub_metering_1<-as.numeric(as.character(hpplot3$Sub_metering_1))
hpplot3$Sub_metering_2<-as.numeric(as.character(hpplot3$Sub_metering_2))
tomelt<-cbind(hpplot3$ctime,hpplot3$Sub_metering_1,hpplot3$Sub_metering_2,hpplot3$Sub_metering_3)
tmdf<-as.data.frame(tomelt)
names(tmdf)<-c("time","Sub_metering_1","Sub_metering_2","Sub_metering_3")
toplot<-melt(tmdf,id.vars="time")
toplot$time<-strptime(toplot$time, "%Y-%m-%d %H:%M:%S")
toplot$value<-as.numeric(as.character(toplot$value))


plot2<-ggplot(data=hpcdf,aes(x=dtime,y=gap)) + geom_line() + xlab("") + ylab("GLobal Active Power (kilowatts)")
plot2<-plot2 + scale_x_datetime(breaks="1 day",labels=date_format("%a")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
  panel.background = element_blank(), axis.line = element_line(color = "black"),
  legend.background=element_rect(color="black"),axis.text.x = element_text(color="black"),
  axis.text.y = element_text(color="black"))

plot3<-ggplot(toplot, aes(x=time, y=value, group=variable, colour=variable )) + geom_line()
plot3 <- plot3 + scale_y_discrete(breaks=c("0","10","20","30")) + ylab("Energy sub metering")
plot3 <- plot3 + scale_x_datetime(breaks="1 day",labels=date_format("%a")) + xlab("")
plot3 <- plot3 + scale_colour_manual(values=c("black","red","blue"))
plot3 <- plot3 + theme(legend.position=c(.80,.80),
                 legend.background = element_rect(size = 0),
                 legend.title=element_blank(),
                 panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(), 
                 panel.background = element_blank(), axis.line = element_line(color = "black"),
                 axis.text.x = element_text(color="black"),
                 axis.text.y = element_text(color="black"))

plot4a<-ggplot(data=hpcdf,aes(x=dtime,y=grap)) + geom_line() + xlab("datetime") + ylab("GLobal Rective Power (kilowatts)")
plot4a<-plot4a + scale_x_datetime(breaks="1 day",labels=date_format("%a")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
  panel.background = element_blank(), axis.line = element_line(color = "black"),
  legend.background=element_rect(color="black"),axis.text.x = element_text(color="black"),
  axis.text.y = element_text(color="black"))

plot4b<-ggplot(data=hpcdf,aes(x=dtime,y=vfac)) + geom_line() + xlab("datetime") + ylab("Voltage")
plot4b<-plot4b + scale_x_datetime(breaks="1 day",labels=date_format("%a")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
  panel.background = element_blank(), axis.line = element_line(color = "black"),
  legend.background=element_rect(color="black"),axis.text.x = element_text(color="black"),
  axis.text.y = element_text(color="black"))

png('plot4.png')

grid.arrange(plot2,plot4b,plot3,plot4a, ncol = 2)

dev.off()
