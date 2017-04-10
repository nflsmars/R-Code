rm(list=ls())
setwd("C:/Users/HongYu/Desktop/")
#-----------------initial parameter----------------------
DR_ratio<-0.5*100
NV_value<-10
PE_rank<-100
PB_rank<-50
DY_rank<-10
CDR_rank<-5
#-----------------initial parameter----------------------
f_data<-read.csv("Fundamentaldata.csv",stringsAsFactors=F)
f_data[,2]<-as.Date(f_data[,2])
f_data[,4]<-as.numeric(f_data[,4])
f_data[,5]<-as.numeric(f_data[,5])
f_data[,10]<-as.numeric(f_data[,10])
f_data[,11]<-as.numeric(f_data[,11])
f_data1<-na.omit(f_data)
colnames(f_data1)<-c("Company","Date","DR","PE","PB","PSR","DY","CDR","NV","1121Close","321Close")

#DR<0.5
f_data2<-f_data1[which(f_data1$DR<DR_ratio),]
#NV>10
f_data3<-f_data2[which(f_data2$NV>NV_value),]
#PE(small->big) 
f_data4<-f_data3[order(f_data3$PE),][1:PE_rank,]
#PB(small->big) 
f_data5<-f_data4[order(f_data4$PB),][1:PB_rank,]
#DY_rank(big->small)
f_data6<-f_data5[order(-f_data5$DY),][1:DY_rank,]
#CDR_rank(big->small)
f_data7<-f_data6[order(-f_data6$CDR),][1:CDR_rank,]

print(f_data7$Company)
PortfolioRt=mean(f_data7[,11]/f_data7[,10])-1
