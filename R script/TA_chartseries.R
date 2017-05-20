rm(list=ls());gc;
library(lubridate)
library(quantmod)
stockprice<-read.table("tw_stocks.txt",sep = "\t",T)
stockprice<-stockprice[,c(4:10)]
colnames(stockprice)<-c("Code","Date","Open","High","Low","Close","Volume")
stockprice$Code<-as.character(stockprice$Code)
stockprice$Date=ymd(stockprice$Date)
stockprice$Open<-as.numeric(as.character(stockprice$Open))
stockprice$High<-as.numeric(as.character(stockprice$High))
stockprice$Low<-as.numeric(as.character(stockprice$Low))
stockprice$Close<-as.numeric(as.character(stockprice$Close))
stockprice$Volume<-as.numeric(as.character(stockprice$Volume))
symbol_all<-unique(stockprice$Code)
symbol_all<-symbol_all[-1]
##
qq=50
stock_now<-stockprice[which(stockprice[,1]==symbol_all[qq]),]
symbol<-xts(stock_now[,3:7],order.by = stock_now$Date)
Op<-symbol[,1]
Hp<-symbol[,2]
Lp<-symbol[,3]
Cp<-symbol[,4]
HLC=symbol[,2:4]
ma5<-SMA(Cp, n = 5)
Vol_s<-symbol[,5]
bd<-BBands(HLC, n = 20, sd = 2)
bd_lb<-bd[,1]
RSI_now<-RSI(Cp,14)
stoch_now<-stoch(HLC,20,3,3)
k<-100*stoch_now[,1]
price_mat<-cbind(RSI_now,lag(RSI_now,1)) #col 1&2
price_mat<-cbind(price_mat,k,lag(k,1:9)) #col 3-12
price_mat<-cbind(price_mat,Lp,Op,Cp,bd_lb) #col 13-16 day(t)
price_mat<-cbind(price_mat,lag(Hp,1),lag(Op,1),lag(Cp,1)) #17-19 t-1
price_mat<-cbind(price_mat,lag(Hp,2),lag(Op,2),lag(Cp,2)) #20-22 t-2
price_mat$Op_lead1<-dplyr::lead(as.numeric(Op),1)
price_mat$Cp_lead1<-dplyr::lead(as.numeric(Cp),1)
price_mat$Cp_lead3<-dplyr::lead(as.numeric(Cp),3)
price_mat$ma5_lead5<-dplyr::lead(as.numeric(ma5),5)
price_mat$ret_1_day<-(price_mat$Cp_lead1-price_mat$Op_lead1)/price_mat$Op_lead1
price_mat$ret_3_day<-(price_mat$Cp_lead3-price_mat$Op_lead1)/price_mat$Op_lead1
price_mat$ret_5ma_day<-(price_mat$ma5_lead5-price_mat$Op_lead1)/price_mat$Op_lead1
colnames(price_mat)<-c("RSI_14","RSI_14(-1)","K","K(-1)","K(-2)","K(-3)","K(-4)","K(-5)","K(-6)","K(-7)","K(-8)","K(-9)","Lp","Op","Cp","bd_lb","Hp(-1)","Op(-1)","Cp(-1)","Hp(-2)","Op(-2)","Cp(-2)","Op_lead1","Cp_lead1","Cp_lead3","ma5_lead5","ret_1_day","ret_3_day","ret_5ma_day")
Trend_Reversal<-function(x)
{
  #cond1
  res_cond1<-x[1]<28
  #cond2
  res_cond2<-(sum(x[3:5]<30)>1)&(sum(x[3:12]<50)>5)
  #cond3
  res_cond3<-x[13]<x[16]
  #cond4
  res_cond4<-(min(x[14],x[15])-x[13])/x[15]>0.02
  #cond5
  res_cond5<-((x[17]-max(x[18],x[19]))/x[19]<0.02)&((x[20]-max(x[21],x[22]))/x[22]<0.02)
  #cond6
  res_cond6<-((x[17]-max(x[18],x[19]))<abs(x[18]-x[19]))&((x[20]-max(x[21],x[22]))<abs(x[21]-x[22]))
  return(res_cond1&res_cond2&res_cond3&res_cond4&res_cond5&res_cond6)
  }
is_Trend_Reversal<-apply(price_mat,1,Trend_Reversal)
price_mat$is_Trend_Reversal<-as.numeric(is_Trend_Reversal)
if (sum(price_mat$is_Trend_Reversal,na.rm=T)>0)
{
  Trend_Reversal_signal<-Hp*is_Trend_Reversal*1.005
  Trend_Reversal_location<-which(price_mat$is_Trend_Reversal==1)
  for (ix in (1:sum(price_mat$is_Trend_Reversal,na.rm=T)))
  {
    s_l<-(Trend_Reversal_location[ix]-10)
    e_l<-min((Trend_Reversal_location[ix]+40),nrow(symbol))
    myfilename<-paste("figure",ix,".png",sep='')
    png(file=myfilename,1440,1440,res=300)
    chartSeries(symbol[s_l:e_l,], theme=chartTheme('white'),TA=c(addVo(),addTA(price_mat$bd_lb[s_l:e_l,],lty=2,col=1,on=1),addTA(Trend_Reversal_signal[s_l:e_l],on=1,col=6,lty=2,pch=8,type="p",lwd=4),addTA(price_mat$RSI_14[s_l:e_l,],lty=2,col=2),addTA(price_mat$K[s_l:e_l,],lty=2,col=3)),up.col="red",dn.col="green")
    title(as.character(symbol_all[qq]))
    dev.off()
    print(Trend_Reversal_location[ix])
  }
}  
  