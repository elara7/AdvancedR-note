diff()
#不用数值索引，用列名

#装
microbenchmark
dplyr
lineprof

#测试答案代码速度和以下代码速度（runs不定长度）
findRuns <- function(x,tag,k) {
  n <- length(x)
  runs <- vector()
  count <- 0
  for (i in 1:(n-k+1)) {
    if (all(x[i:(i+k-1)]==tag)) {
      count <- count + 1
      runs[count] <- i
    }
  }
  if (count > 0) {
    runs <- runs[1:count]
  } else runs <- NULL
  return(runs)
}
#On（向量遍历一遍的时间）

#第一题：游程算法（连续几天出现同样事件）

#第二题：tidy data，每一行都是一个观察，每一列都是一个属性，如果是分类的要做成分类变量
#第二题扩展；anscombe转换成tidydata：x y group（分类属性）

#第三题扩展，dplyr传入参数问题 function(x,y)穿不进去
#尽量不用sapply，用vapply



dt_flight<-data.frame(hflights$UniqueCarrier,hflights$Year,hflights$Month,hflights$ArrDelay)
colnames(dt_flight)<-c("UniqueCarrier","Year","Month","ArrDelay")
UniqueCarrier<-unique(dt_flight$UniqueCarrier)
dt_flight<-dt_flight[order(dt_flight$UniqueCarrier,dt_flight$Month),]
lst<-list()
mean_quantile<-data.frame(UniqueCarrier)
i=1
for (i in 1:nlevels(UniqueCarrier)){
  temp<-na.omit(dt_flight[which(dt_flight$UniqueCarrier==UniqueCarrier[i]),])
  print(i)
  lst[[i]]<-tapply(temp$ArrDelay,temp$Month,function(x) {return(quantile(x,probs=seq(0,1,0.1),na.rm = T))})
  # 每个月的分位数
  mean_quantile$mean_quantile[i]<-mean(lst[[i]])
}



lapply(lapply(lst, unlist),mean)

cut

findInterval

#闭包（功能通过函数名传入）

by tapply aggregate

dt_flight<-data.frame(hflights$UniqueCarrier,hflights$Year,hflights$Month,hflights$ArrDelay)
colnames(dt_flight)<-c("UniqueCarrier","Year","Month","ArrDelay")
quan <- function(x){
  quantile(x$ArrDelay, seq(0,1,0.1),na.rm = T)
  }
tapply(dt_flight, ind, FUN = quan )







dt_flight<-data.frame(hflights$UniqueCarrier,hflights$Year,hflights$Month,hflights$ArrDelay)
colnames(dt_flight)<-c("UniqueCarrier","Year","Month","ArrDelay")
UniqueCarrier<-unique(dt_flight$UniqueCarrier)
dt_flight<-dt_flight[order(dt_flight$UniqueCarrier,dt_flight$Month),]
lst<-list()
mean_quantile<-data.frame(UniqueCarrier)
i=1
for (i in 1:nlevels(UniqueCarrier)){
  temp<-na.omit(dt_flight[which(dt_flight$UniqueCarrier==UniqueCarrier[i]),])
  print(i)
  lst[[i]]<-tapply(temp$ArrDelay,temp$Month,function(x) {return(quantile(x,probs=seq(0,1,0.1),na.rm = T))})
  # 每个月的分位数
  mean_quantile$mean_quantile[i]<-mean(lst[[i]])
}