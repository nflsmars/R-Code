# data for the scrap rates examples:
SR87<-c(10,1,6,.45,1.25,1.3,1.06,3,8.18,1.67,.98,1,.45,5.03,8,9,18,.28,
        7,3.97)
SR88<-c(3,1,5,.5,1.54,1.5,.8,2,.67,1.17,.51,.5,.61,6.7,4,7,19,.2,5,3.83)
Change <- SR88 - SR87

#method1 for two-sided CI(test if change is equal to 0 at 95% confidence level two sided)
t.test(Change)
#method2 for two-sided CI
(avgCh<- mean(Change))             #sample mean
(n    <- length(Change))           #sample size
(sdCh <- sd(Change))               #calc sigma-sqaure
(se   <- sdCh/sqrt(n))             #calc std error of mean(Change) estimator
(c    <- qt(.975, n-1))            #97.5% quantile,DoF=19 score
(avgCh+se*c*c(-1,1))               #95% CI
(pvalue=2*pt(avgCh/se,n-1))        #p value two-sided

#method1 for 1-sided test:
t.test(Change, alternative="less")
#method2 for 1-sided test:
(avgCh<- mean(Change))             #sample mean
(n    <- length(Change))           #sample size
(sdCh <- sd(Change))               #calc sigma-sqaure
(se   <- sdCh/sqrt(n))             #calc std error of mean(Change) estimator
(c    <- qt(.95, n-1))             #95% quantile,DoF=19 score
(avgCh+se*c*c(-Inf,1))             #95% CI
(pvalue=pt(avgCh/se,n-1))          #p value one-sided