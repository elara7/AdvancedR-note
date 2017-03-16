# benchmark based on surface pro 4 Intel® Core™ m3-6Y30 Processor
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
d <- ds[ds$anest == "dow",]
d$start <- timetrans(d$start)
d$end <- timetrans(d$end)



# solution 1

# check overlap
solution1 <- function(){
check <- function(index, data){
  res <- index[max(data[index,]$start) < min(data[index,]$end)]
  if(length(res)!=0){
    return(res)
  }
}

# generate combn
index <- function(n){
  ind <- NULL
  for( k in 2:n){
    ind <- c(ind, combn(n, k, simplify = F))
  }
  return(ind)
}

# find overlap
findoverlap <- function(data){
  n <- nrow(data)
  Filter(Negate(is.null), lapply(index(n),function(x){check(x,data)}))
}

res <- findoverlap(d)
l <- sapply(res,length)
postion <- which(l == max(l))
lapply(postion, function(x){d[res[[x]],"id"]})
}
microbenchmark::microbenchmark(solution1())

#Unit: milliseconds
#expr      min       lq     mean   median       uq      max neval
#solution1() 18.18208 19.14783 20.08346 19.70622 20.34385 29.04581   100

# solution 2

solution2 <- function(){
# find interval
interval <- sort(unique((c(d$start,d$end)),rightmost.closed = T))

range <- data.frame(row.names = d$id)

for (i in 1:nrow(range)){
  for (j in 1:(length(interval)-1)){
    if(max(c(d$start[i],interval[j]))<min(c(d$end[i],interval[j+1]))){
      range[i,j] <- 1
    }else{
      range[i,j] <- 0
    }
  }
}

max_time <- max(colSums(range[,]))

max_time

range[,which(colSums(range[,])==max_time)]
}

microbenchmark::microbenchmark(solution2())
#Unit: milliseconds
#expr      min       lq     mean   median       uq      max neval
#solution2() 14.55474 15.18695 18.05417 16.28307 18.80142 81.79395   100


solution3 <- function(data){
  n <- nrow(data)
  for( k in n:2){
    index <- combn(n, k, simplify = F)
    res <- Filter(Negate(is.null), lapply(index, function(x){if(max(data[x,]$start) < min(data[x,]$end)){data[x,"id"]}}))
    if(length(res)!=0){
      return(res)
    }
  }
}

Rprof("Rprof.out")
solution3(d)
Rprof(NULL)
summaryRprof("Rprof.out")
microbenchmark::microbenchmark(solution3(d))

solution4 <- function(data){
  n <- nrow(data)
  Filter(Negate(is.null), lapply(lapply(n:2, function(x){combn(n,x)}), function(x){Filter(Negate(is.null), apply(x ,2, function(x){if(max(data[x,]$start) < min(data[x,]$end)){data[x,"id"]}}))}))
}
solution4(d)
microbenchmark::microbenchmark(solution4(d))


