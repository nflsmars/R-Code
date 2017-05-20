rm(list=ls());gc;
library(lubridate)
library(quantmod)
#initial parameters#
transaction_cost<-0.005
#initial parameters#
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
trading_table<-data.frame()
for (i in 1:length(symbol_all)){
stock_now<-stockprice[which(stockprice[,1]==symbol_all[i]),]
symbol<-xts(stock_now[,3:7],order.by = stock_now$Date)
Op<-symbol[,1]
Op_num<-as.numeric(Op)
Hp<-symbol[,2]
HP_num<-as.numeric(Hp)
Lp<-symbol[,3]
Lp_num<-as.numeric(Lp)
Cp<-symbol[,4]
Cp_num<-as.numeric(Cp)
HLC=symbol[,2:4]
ma2<-SMA(Cp, n = 2)
ma2_num<-as.numeric(ma2)
ma3<-SMA(Cp, n = 3)
ma3_num<-as.numeric(ma3)
ma4<-SMA(Cp, n = 4)
ma4_num<-as.numeric(ma4)
ma5<-SMA(Cp, n = 5)
ma5_num<-as.numeric(ma5)
Vol_s<-symbol[,5]
bd<-BBands(HLC, n = 20, sd = 2)
bd_lb<-bd[,1]
RSI_now<-RSI(Cp,14)
stoch_now<-stoch(HLC,20,3,3)
k<-100*stoch_now[,1]
price_mat<-cbind(RSI_now,lag(RSI_now,1))
price_mat<-cbind(price_mat,k,lag(k,1:9))
price_mat<-cbind(price_mat,Lp,Op,Cp,bd_lb)
price_mat<-cbind(price_mat,lag(Hp,1),lag(Op,1),lag(Cp,1))
price_mat<-cbind(price_mat,lag(Hp,2),lag(Op,2),lag(Cp,2))
colnames(price_mat)<-c("RSI_14","RSI_14(-1)","K","K(-1)","K(-2)","K(-3)",
                       "K(-4)","K(-5)","K(-6)","K(-7)","K(-8)","K(-9)",
                       "Lp","Op","Cp","bd_lb","Hp(-1)","Op(-1)","Cp(-1)",
                       "Hp(-2)","Op(-2)","Cp(-2)")
Trend_Reversal<-function(x)
{
  #cond1
  res_cond1<-x[1]<28
  #cond2
  res_cond2<-(sum(x[3:5]<30,na.rm = T)>1)&(sum(x[3:12]<50,na.rm = T)>5)
  #cond3
  res_cond3<-x[13]<x[16]
  #cond4
  res_cond4<-(min(x[14],x[15],na.rm = T)-x[13])/x[15]>0.02
  #cond5
  res_cond5<-((x[17]-max(x[18],x[19],na.rm = T))/x[19]<0.02)&
    ((x[20]-max(x[21],x[22],na.rm = T))/x[22]<0.02)
  #cond6
  res_cond6<-((x[17]-max(x[18],x[19],na.rm = T))<abs(x[18]-x[19]))&
    ((x[20]-max(x[21],x[22],na.rm = T))<abs(x[21]-x[22]))
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
    if (Trend_Reversal_location[ix]<=(nrow(symbol)-20))
    {
      signal_location<-Trend_Reversal_location[ix]
      buy_date<-index(symbol)[signal_location+1]
      buy_price<-Op_num[signal_location+1]
      sell_date1<-index(symbol)[signal_location+1]
      sell_price1<-Cp_num[signal_location+1]
      ret1<-(sell_price1-buy_price)/buy_price-transaction_cost
      sell_date2<-index(symbol)[signal_location+2]
      sell_price2_Cp<-Cp_num[signal_location+2]
      sell_price2_MA<-ma2_num[signal_location+2]
      ret2_Cp<-(sell_price2_Cp-buy_price)/buy_price-transaction_cost
      ret2_MA<-(sell_price2_MA-buy_price)/buy_price-transaction_cost
      sell_date3<-index(symbol)[signal_location+3]
      sell_price3_Cp<-Cp_num[signal_location+3]
      sell_price3_MA<-ma3_num[signal_location+3]
      ret3_Cp<-(sell_price3_Cp-buy_price)/buy_price-transaction_cost
      ret3_MA<-(sell_price3_MA-buy_price)/buy_price-transaction_cost
      sell_date4<-index(symbol)[signal_location+4]
      sell_price4_Cp<-Cp_num[signal_location+4]
      sell_price4_MA<-ma4_num[signal_location+4]
      ret4_Cp<-(sell_price4_Cp-buy_price)/buy_price-transaction_cost
      ret4_MA<-(sell_price4_MA-buy_price)/buy_price-transaction_cost
      sell_date5<-index(symbol)[signal_location+5]
      sell_price5_Cp<-Cp_num[signal_location+5]
      sell_price5_MA<-ma5_num[signal_location+5]
      ret5_Cp<-(sell_price5_Cp-buy_price)/buy_price-transaction_cost
      ret5_MA<-(sell_price5_MA-buy_price)/buy_price-transaction_cost
      sell_date6<-index(symbol)[signal_location+10]
      sell_price6_Cp<-Cp_num[signal_location+10]
      sell_price6_MA<-ma5_num[signal_location+10]
      ret6_Cp<-(sell_price6_Cp-buy_price)/buy_price-transaction_cost
      ret6_MA<-(sell_price6_MA-buy_price)/buy_price-transaction_cost
      sell_date7<-index(symbol)[signal_location+15]
      sell_price7_Cp<-Cp_num[signal_location+15]
      sell_price7_MA<-ma5_num[signal_location+15]
      ret7_Cp<-(sell_price7_Cp-buy_price)/buy_price-transaction_cost
      ret7_MA<-(sell_price7_MA-buy_price)/buy_price-transaction_cost
      sell_date8<-index(symbol)[signal_location+15]
      sell_price8_Cp<-Cp_num[signal_location+20]
      sell_price8_MA<-ma5_num[signal_location+20]
      ret8_Cp<-(sell_price8_Cp-buy_price)/buy_price-transaction_cost
      ret8_MA<-(sell_price8_MA-buy_price)/buy_price-transaction_cost
      temp_data<-data.frame(code=symbol_all[i],buy_date=buy_date,buy_price=buy_price,
        sell_date1=sell_date1,sell_price1=sell_price1,ret1=ret1,
        sell_date2=sell_date2,sell_price2_Cp=sell_price2_Cp,
        sell_price2_MA=sell_price2_MA,ret2_Cp=ret2_Cp,ret2_MA=ret2_MA,
        sell_date3=sell_date3,sell_price3_Cp=sell_price3_Cp,
        sell_price3_MA=sell_price3_MA,ret3_Cp=ret3_Cp,ret3_MA=ret3_MA,
        sell_date4=sell_date4,sell_price4_Cp=sell_price4_Cp,
        sell_price4_MA=sell_price4_MA,ret4_Cp=ret4_Cp,ret4_MA=ret4_MA,
        sell_date5=sell_date5,sell_price5_Cp=sell_price5_Cp,
        sell_price5_MA=sell_price5_MA,ret5_Cp=ret5_Cp,ret5_MA=ret5_MA,
        sell_date6=sell_date6,sell_price6_Cp=sell_price6_Cp,
        sell_price6_MA=sell_price6_MA,ret6_Cp=ret6_Cp,ret6_MA=ret6_MA,
        sell_date7=sell_date7,sell_price7_Cp=sell_price7_Cp,
        sell_price7_MA=sell_price7_MA,ret7_Cp=ret7_Cp,ret7_MA=ret7_MA,
        sell_date8=sell_date8,sell_price8_Cp=sell_price8_Cp,
        sell_price8_MA=sell_price8_MA,ret8_Cp=ret8_Cp,ret8_MA=ret8_MA)
      trading_table<-rbind(trading_table,temp_data)
    }
  }
}
}
final_result<-data.frame(rbind(c(p_win_1d=mean(trading_table[,6]>=0),
                                  avg_ret_1d=mean(trading_table[,6])),
                                c(p_win_2d_Cp=mean(trading_table[,10]>=0),
                                  avg_ret_2d_Cp=mean(trading_table[,10]),
                                  p_win_2d_MA=mean(trading_table[,11]>=0),
                                  avg_ret_2d_MA=mean(trading_table[,11])),
                                c(p_win_3d_Cp=mean(trading_table[,15]>=0),
                                  avg_ret_3d_Cp=mean(trading_table[,15]),
                                  p_win_3d_MA=mean(trading_table[,16]>=0),
                                  avg_ret_3d_MA=mean(trading_table[,16])),
                                c(p_win_4d_Cp=mean(trading_table[,20]>=0),
                                  avg_ret_4d_Cp=mean(trading_table[,20]),
                                  p_win_4d_MA=mean(trading_table[,21]>=0),
                                  avg_ret_4d_MA=mean(trading_table[,21])),
                                c(p_win_5d_Cp=mean(trading_table[,25]>=0),
                                  avg_ret_5d_Cp=mean(trading_table[,25]),
                                  p_win_5d_MA=mean(trading_table[,26]>=0),
                                  avg_ret_5d_MA=mean(trading_table[,26])),
                                c(p_win_10d_Cp=mean(trading_table[,30]>=0),
                                  avg_ret_10d_Cp=mean(trading_table[,30]),
                                  p_win_10d_MA=mean(trading_table[,31]>=0),
                                  avg_ret_10d_MA=mean(trading_table[,31])),
                                c(p_win_15d_Cp=mean(trading_table[,35]>=0),
                                  avg_ret_15d_Cp=mean(trading_table[,35]),
                                  p_win_15d_MA=mean(trading_table[,36]>=0),
                                  avg_ret_15d_MA=mean(trading_table[,36])),
                                c(p_win_20d_Cp=mean(trading_table[,40]>=0),
                                  avg_ret_20d_Cp=mean(trading_table[,40]),
                                  p_win_20d_MA=mean(trading_table[,41]>=0),
                                  avg_ret_20d_MA=mean(trading_table[,41]))))
rownames(final_result)=c(1,2,3,4,5,10,15,20)
colnames(final_result)=c('winrate_Cp','Ret_Cp','winrate_MA','Ret_MA')