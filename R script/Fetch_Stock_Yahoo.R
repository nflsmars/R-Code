#This is the code to fetch taiwan stocks from yahoo
library(RCurl)
library(XML)
stockcode=c(2330，2317，6505，2412)
recordtable=matrix(0,nrow = 0,ncol = 6)
colnames(recordtable)=c('Code','Time','Open','High','Low','Close')
for (i in 1:4){
  url=paste0("https://tw.stock.yahoo.com/q/q?s=",stockcode[i])
  html=getURL(url,.encoding = "big5") #since tw yahoo uses big5
  html=iconv(html,from="big5",to="utf-8") #big5 to utf-8
  html=htmlParse(html,encoding = "utf-8") #convert to code for XML
  data=xpathSApply(html,"//tr[2]/td",xmlValue) #extract data
  recordData=c(stockcode[i],data[c(3,10:12,4)]) #save data
  recordtable=rbind(recordtable,recordData)
}
