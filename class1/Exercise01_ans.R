# time transform function
timetrans <- function(x){
  as.POSIXct(paste0(paste(as.character(Sys.Date()),x), ":00"))
}

#load data
ds<-data.frame(id=seq(10,80,by=10),
               anest=c("baker","baker",rep("dow",6)),
               start=c("08:00","09:00","09:00","08:00","10:00","12:30","13:30","18:00"),
               end=c("11:00","13:00","15:30","13:30","11:30","13:30","14:30","19:00"),
               stringsAsFactors = F)

# transform data
require(data.table)
setDT(ds)
d <- ds[anest == "dow"][, `:=`(start = timetrans(start), end = timetrans(end))]

# find interval
int <- sort(unique((c(d$start,d$end)),rightmost.closed = T))

range <- data.frame(row.names = d$id)

for (i in 1:nrow(range)){
  for (j in 1:(length(int)-1)){
    if(max(c(d$start[i],int[j]))<min(c(d$end[i],int[j+1]))){
      range[i,j] <- 1
    }else{
      range[i,j] <- 0
    }
  }
}

max_time <- max(colSums(range[,]))

max_time

range[,which(colSums(range[,])==max_time)]

#Unit: milliseconds
#expr      min       lq     mean  median       uq      max neval
#t() 27.23756 28.04619 31.12702 28.7529 32.54651 64.87426   100

# time: 30min