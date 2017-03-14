ds<-data.frame(id=seq(10,80,by=10),
               anest=c("baker","baker",rep("dow",6)),
               start=c("08:00","09:00","09:00","08:00","10:00","12:30","13:30","18:00"),
               end=c("11:00","13:00","15:30","13:30","11:30","13:30","14:30","19:00"),
              stringsAsFactors = F)

ds

d <- ds[ds$anest=="dow",]

timetrans <- function(x){
  as.POSIXct(paste0(paste(as.character(Sys.Date()),x), ":00"))
}

d$start <- timetrans(d$start)
d$end <- timetrans(d$end)

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

max(colSums(range[,]))

range[,which(colSums(range[,])==3)]


max(d$start[ind])<min(d$end[ind])

for()


ind <- NULL
d <- ds[ds$anest=="dow",]
for (i in 1:(nrow(d)-1)){
  start <- ds[i,"start"]
  end <- ds[i,"end"]
  for (j in (i+1):nrow(d)){
    if(timecompare(ds[i,"end"], ds[j,"start"]) | timecompare(ds[j,"end"], ds[i,"start"])){
      ind <- rbind(ind, c(i,j))
    }
    
  }
}
ind
i=1
j=2



timecompare <- function(timex,timey,con){ # con == l 返回大的，s 返回小的
  if (con == "l"){
    con <- function(a,b){
      return(a>b)
    }
  } else if(con == "s"){
    con <- function(a,b){
      return(a<b)
    }
  }
  x <- as.numeric(unlist(strsplit(timex,":")))  # split hours and min
  y <- as.numeric(unlist(strsplit(timey,":")))
  if (con(x[1], y[1])){ 
    return(timex)  # if x hours > y hours, x > y
  } else if(x[1] == y[1] & con(x[2] ,y[2])){
    return(timex)  # if x hours == y hours, and x min > y min, x>y
  } else {
    return(timey)
  }
}
timecompare("08:30","09:15","s")