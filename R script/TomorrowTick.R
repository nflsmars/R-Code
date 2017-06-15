#Find all ticks for tomorrow
All_tick<-c(seq(0,10,0.01),seq(10.05,50,0.05),seq(50.1,100,0.1),seq(100.5,500,0.5),seq(501,1000,1),seq(1005,10000,5))
cl_lag<-49.95
price_limit<-0.1
cl_tomorrow<-All_tick[(All_tick<=cl_lag*(1+price_limit))&(All_tick>=cl_lag*(1-price_limit))]