#first method for grouping
system.time({
x<-matrix(runif(20),1,20)
x<-cbind(x,matrix(c(0.2,0.4,0.6,0.8,1),1,5))
x1<-1*(x<=0.2)+2*((x>0.2)&(x<=.4))+3*((x>0.4)&(x<=.6))+4*((x>0.6)&(x<=.8))+5*(x>0.8)
r1<-LETTERS[1:5]
as.factor(r1[x1])
})
#second method for grouping
cut(x,breaks = c(0,0.2,0.4,0.6,0.8,1),right = T,labels = c("A1","B2","C3","D4","E5"),include.lowest = T)

#third method for grouping
x3<-ifelse(x<=0.2,"A1",ifelse(x<=0.4,"B2",ifelse(x<=0.6,"C3",ifelse(x<=0.8,"D4",ifelse(x<=1,"E5")))))

#fourth method for grouping
x4<-matrix(NA,1,ncol=ncol(x))
for (i in 1:ncol(x))
{
  if (x[i]<=0.2)
  {
    x4[i]<-"A1"
  } else if (x[i]<=0.4)   
    {
    x4[i]<-"B2"
  } else if (x[i]<=0.6)   
    {
    x4[i]<-"C3"
  } else if (x[i]<=0.8)
  {
    x4[i]<-"D4"
  } else
  {
    x4[i]<-"E5"
  }
}




