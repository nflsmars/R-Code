rm(list=ls())
setwd("C:/Users/HongYu/Desktop/")
#-----------------initial parameter----------------------
ROE=10
#-----------------initial parameter----------------------
f_data<-read.csv("fundamental.csv",stringsAsFactors=F)
f_data[,6]<-as.numeric(f_data[,6])
f_data[,5]<-as.numeric(f_data[,5])
f_data1<-na.omit(f_data)
colnames(f_data1)<-c("Company","ROE","DY","DR","321Close","1121Close")

#ROE>10
f_data2<-filter(f_data1,ROE>10)
#Companies which have DY in top 30% of f_data2
f_data3<-arrange(f_data2,desc(DY))
f_data3<-head(f_data3,round(0.3*(dim(f_data3)[1])))
#arrange according to DR
f_data4<-arrange(f_data3,DR)
#calculate holding period return
f_data4$HPR<-f_data4$`321Close`/f_data4$`1121Close`-1
#5-stock portfolio
(HPR_5=mean(f_data4$HPR[1:5]))
#10-stock portfolio
(HPR_10=mean(f_data4$HPR[1:10]))
#20-stock portfolio
(HPR_20=mean(f_data4$HPR[1:20]))
#stock list for top 20
print(f_data4$Company[1:20])