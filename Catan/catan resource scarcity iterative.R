#Method is agnostic of resource scarcity and ports
#Assumes that the best spot is the spot with highest total number of dots

library(reshape)
library(dplyr)
library(sqldf)

#define 19 variables for the 19 spaces on catan board. Numbers are the number of dots on each tile.
#data.frame(letters[1:19],sample(set,19,replace=FALSE,prob=NULL))
# ^^^ can be used for random laying which is actually much easier for simulations
#no actual "s" tile exists but it is introduced to account for the desert

#logic: if the desert has not been placed, probability of (n-1/n) to place desert 
#   where n is number of tiles remaining to be placed. If desert has already been placed,
#   place the value of the previous tile instead. Note that tile-laying is allowed to begin 
#   on a desert vertex.

#a <- 4,b <- 1,c <- 5,d <- 2,e <- 5,f <- 3,g <- 4,h <- 1,i <- 2,j <- 3,k <- 0,l <- 5,m <- 3,n <- 4,o <- 3,p <- 4,q <- 5,r <- 2,s <- 2

# sheep - 4
# wood - 4
# brick - 3
# stone - 3
# wheat - 4
# desert - 1


agg_results <- NULL


for (i in 1:100) {
  
  #tile variables need to be set to null otherwise there could be interference from other simulations
  
  a <- NULL
  b <- NULL
  c <- NULL
  d <- NULL
  e <- NULL
  f <- NULL
  g <- NULL
  h <- NULL
  i <- NULL
  j <- NULL
  k <- NULL
  l <- NULL
  m <- NULL
  n <- NULL
  o <- NULL
  p <- NULL
  q <- NULL
  r <- NULL
  s <- NULL
  
  set <- NULL
  
  #this is kind of a lame way to do resources but it should work okay
  res <- c("WHEAT1","WHEAT2","WHEAT3","WHEAT4","BRICK1","BRICK2","BRICK3","STONE1","STONE2","STONE3","WOOD1","WOOD2","WOOD3","WOOD4","SHEEP1","SHEEP2","SHEEP3","SHEEP4")
  #names(res) <- c("name")
  
  a$n <- ifelse(length(set[which(set==0)])==0,sample(c(4,0),1,replace=FALSE,prob=c(18/19,1/19)),4)
  a$r <- ifelse(a$n==0,"DESERT",sample(res,1,replace=FALSE))
  res <- setdiff(res,a$r)
  set <- c(a$n)
  
  b$n <- ifelse(length(set[which(set==0)])==0,sample(c(1,0),1,replace=FALSE,prob=c(17/18,1/18)),4)
  b$r <- ifelse(b$n==0,"DESERT",sample(res,1,replace=FALSE))
  res <- setdiff(res,b$r)
  set <- c(a$n,b$n)
  
  c$n <- ifelse(length(set[which(set==0)])==0,sample(c(5,0),1,replace=FALSE,prob=c(16/17,1/17)),1)
  c$r <- ifelse(c$n==0,"DESERT",sample(res,1,replace=FALSE))
  res <- setdiff(res,c$r)
  set <- c(a$n,b$n,c$n)
  
  d$n <- ifelse(length(set[which(set==0)])==0,sample(c(2,0),1,replace=FALSE,prob=c(15/16,1/16)),5)
  d$r <- ifelse(d$n==0,"DESERT",sample(res,1,replace=FALSE))
  res <- setdiff(res,d$r)
  set <- c(a$n,b$n,c$n,d$n)
  
  e$n <- ifelse(length(set[which(set==0)])==0,sample(c(5,0),1,replace=FALSE,prob=c(14/15,1/15)),2)
  e$r <- ifelse(e$n==0,"DESERT",sample(res,1,replace=FALSE))
  res <- setdiff(res,e$r)
  set <- c(a$n,b$n,c$n,d$n,e$n)
  
  f$n <- ifelse(length(set[which(set==0)])==0,sample(c(3,0),1,replace=FALSE,prob=c(13/14,1/14)),5)
  f$r <- ifelse(f$n==0,"DESERT",sample(res,1,replace=FALSE))
  res <- setdiff(res,f$r)
  set <- c(a$n,b$n,c$n,d$n,e$n,f$n)
  
  g$n <- ifelse(length(set[which(set==0)])==0,sample(c(4,0),1,replace=FALSE,prob=c(12/13,1/13)),3)
  g$r <- ifelse(g$n==0,"DESERT",sample(res,1,replace=FALSE))
  res <- setdiff(res,g$r)
  set <- c(a$n,b$n,c$n,d$n,e$n,f$n,g$n)
  
  h$n <- ifelse(length(set[which(set==0)])==0,sample(c(1,0),1,replace=FALSE,prob=c(11/12,1/12)),4)
  h$r <- ifelse(h$n==0,"DESERT",sample(res,1,replace=FALSE))
  res <- setdiff(res,h$r)
  set <- c(a$n,b$n,c$n,d$n,e$n,f$n,g$n,h$n)
  
  i$n <- ifelse(length(set[which(set==0)])==0,sample(c(2,0),1,replace=FALSE,prob=c(10/11,1/11)),1)
  i$r <- ifelse(i$n==0,"DESERT",sample(res,1,replace=FALSE))
  res <- setdiff(res,i$r)
  set <- c(a$n,b$n,c$n,d$n,e$n,f$n,g$n,h$n,i$n)
  
  j$n <- ifelse(length(set[which(set==0)])==0,sample(c(3,0),1,replace=FALSE,prob=c(9/10,1/10)),2)
  j$r <- ifelse(j$n==0,"DESERT",sample(res,1,replace=FALSE))
  res <- setdiff(res,j$r)
  set <- c(a$n,b$n,c$n,d$n,e$n,f$n,g$n,h$n,i$n,j$n)
  
  k$n <- ifelse(length(set[which(set==0)])==0,sample(c(5,0),1,replace=FALSE,prob=c(8/9,1/9)),3)
  k$r <- ifelse(k$n==0,"DESERT",sample(res,1,replace=FALSE))
  res <- setdiff(res,k$r)
  set <- c(a$n,b$n,c$n,d$n,e$n,f$n,g$n,h$n,i$n,j$n,k$n)
  
  l$n <- ifelse(length(set[which(set==0)])==0,sample(c(3,0),1,replace=FALSE,prob=c(7/8,1/8)),5)
  l$r <- ifelse(l$n==0,"DESERT",sample(res,1,replace=FALSE))
  res <- setdiff(res,l$r)
  set <- c(a$n,b$n,c$n,d$n,e$n,f$n,g$n,h$n,i$n,j$n,k$n,l$n)
  
  m$n <- ifelse(length(set[which(set==0)])==0,sample(c(4,0),1,replace=FALSE,prob=c(6/7,1/7)),3)
  m$r <- ifelse(m$n==0,"DESERT",sample(res,1,replace=FALSE))
  res <- setdiff(res,m$r)
  set <- c(a$n,b$n,c$n,d$n,e$n,f$n,g$n,h$n,i$n,j$n,k$n,l$n,m$n)
  
  n$n <- ifelse(length(set[which(set==0)])==0,sample(c(3,0),1,replace=FALSE,prob=c(5/6,1/6)),4)
  n$r <- ifelse(n$n==0,"DESERT",sample(res,1,replace=FALSE))
  res <- setdiff(res,n$r)
  set <- c(a$n,b$n,c$n,d$n,e$n,f$n,g$n,h$n,i$n,j$n,k$n,l$n,m$n,n$n)
  
  o$n <- ifelse(length(set[which(set==0)])==0,sample(c(4,0),1,replace=FALSE,prob=c(4/5,1/5)),3)
  o$r <- ifelse(o$n==0,"DESERT",sample(res,1,replace=FALSE))
  res <- setdiff(res,o$r)
  set <- c(a$n,b$n,c$n,d$n,e$n,f$n,g$n,h$n,i$n,j$n,k$n,l$n,m$n,n$n,o$n)
  
  p$n <- ifelse(length(set[which(set==0)])==0,sample(c(5,0),1,replace=FALSE,prob=c(3/4,1/4)),4)
  p$r <- ifelse(p$n==0,"DESERT",sample(res,1,replace=FALSE))
  res <- setdiff(res,p$r)
  set <- c(a$n,b$n,c$n,d$n,e$n,f$n,g$n,h$n,i$n,j$n,k$n,l$n,m$n,n$n,o$n,p$n)
  
  q$n <- ifelse(length(set[which(set==0)])==0,sample(c(2,0),1,replace=FALSE,prob=c(2/3,1/3)),5)
  q$r <- ifelse(q$n==0,"DESERT",sample(res,1,replace=FALSE))
  res <- setdiff(res,q$r)
  set <- c(a$n,b$n,c$n,d$n,e$n,f$n,g$n,h$n,i$n,j$n,k$n,l$n,m$n,n$n,o$n,p$n,q$n)
  
  r$n <- ifelse(length(set[which(set==0)])==0,sample(c(2,0),1,replace=FALSE,prob=c(1/2,1/2)),2)
  r$r <- ifelse(r$n==0,"DESERT",sample(res,1,replace=FALSE))
  res <- setdiff(res,r$r)
  set <- c(a$n,b$n,c$n,d$n,e$n,f$n,g$n,h$n,i$n,j$n,k$n,l$n,m$n,n$n,o$n,p$n,q$n,r$n)
  
  s$n <- ifelse(length(set[which(set==0)])==0,0,2)
  s$r <- ifelse(s$n==0,"DESERT",sample(res,1,replace=FALSE))
  res <- setdiff(res,s$r)
  
  #resource desirability constants based on prob of vp from 1 resource
  des <- data.frame(c("BRICK","WOOD","WHEAT","STONE","SHEEP","DESERT"),c(-0.15,-0.15,0.30,0.25,-0.10,0))
  names(des) <- c("r","desire_constant")
    
  #setagg <- data.frame(t(aggregate(set,by=list(set),FUN=length)))[2,]
  #names(setagg) <- paste("n",t(aggregate(set,by=list(set),FUN=length))[1,],sep="")
  #s <-  ifelse(length(set[which(set==0)])==0,0,ifelse(setagg$n1<2,1,ifelse(setagg$n2<4,2,ifelse(setagg$n3<4,3,ifelse(setagg$n4<4,4,ifelse(setagg$n5<4,5,0))))))
  setcomp <- c(a$n,b$n,c$n,d$n,e$n,f$n,g$n,h$n,i$n,j$n,k$n,l$n,m$n,n$n,o$n,p$n,q$n,r$n,s$n)  
  rescomp <- c(a$r,b$r,c$r,d$r,e$r,f$r,g$r,h$r,i$r,j$r,k$r,l$r,m$r,n$r,o$r,p$r,q$r,r$r,s$r)
  lets <- letters[1:19]
  resources <- data.frame(cbind(rescomp,setcomp,lets),stringsAsFactors=FALSE)
  resources$setcomp <- as.numeric(resources$setcomp)
  resources$rescomp <- gsub("[0-9]+","",resources$rescomp)
  names(resources) <- c("r","n","tile")
  rr <- resources
  resources <- resources[which(resources$r != "DESERT"),]
  resagg <- aggregate(n~r,data=resources,sum)
  avg <- mean(resources$n)
  resexp <- data.frame(aggregate(n~r,data=resources,length),resagg$n,avg)
  names(resexp) <- c("r","n_len","n_sum","n_avg")
  resexp$n_exp <- resexp$n_len * resexp$n_avg
  resexp$n_dev <- resexp$n_sum - resexp$n_exp
  #desirability assigned by # spots of rank reduction measured by scarcity of the resources vs ideal average. 
  #1.75 is a "magic number" - one idea is to benchmark against the ACTUAL desirability of the resource viz victory pts production
  resexp <- merge(resexp,des)
  resexp$desirability_factor <- round((resexp$n_exp - resexp$n_sum)/1.75,2)+resexp$desire_constant
  #resexp$desirability_factor <- (mean(resexp$n_len)-resexp$n_len)*as.numeric(resexp$desirability_factor)+as.numeric(resexp$desirability_factor)
  desert <- c("DESERT",1,0,0,0,0,0,0)
  resexp <- rbind(resexp,desert)
  names(rr) <- c("r","n","tile")
  rr <- merge(rr,resexp)
  rr <- rr[,c(1:3,10)]
  rr$desirability_factor <- as.numeric(rr$desirability_factor)
  rr$mn <- rr$n + rr$desirability_factor
  
  
  
  #tile laying has to start on a vertex 
  #assumes that tiles are laid alphabetically (per rulebook) not randomly as in house rules
  #it's not necessary to define every single one of the 86 on the board
  #rather, the intersections of 3 tiles can be defined, as only the top 4 to 12 spots will be taken
  #these intersections will total 24 spots
  s1  <- rr$mn[which(rr$tile %in% c("a","m","l"))]
  s2  <- rr$mn[which(rr$tile %in% c("a","b","m"))]
  s3  <- rr$mn[which(rr$tile %in% c("b","m","n"))]
  s4  <- rr$mn[which(rr$tile %in% c("b","c","n"))]
  s5  <- rr$mn[which(rr$tile %in% c("c","n","d"))]
  s6  <- rr$mn[which(rr$tile %in% c("d","n","o"))]
  s7  <- rr$mn[which(rr$tile %in% c("d","o","e"))]
  s8  <- rr$mn[which(rr$tile %in% c("e","o","f"))]
  s9  <- rr$mn[which(rr$tile %in% c("f","o","p"))]
  s10 <- rr$mn[which(rr$tile %in% c("f","p","g"))]
  s11 <- rr$mn[which(rr$tile %in% c("g","p","h"))]
  s12 <- rr$mn[which(rr$tile %in% c("h","p","q"))]
  s13 <- rr$mn[which(rr$tile %in% c("h","i","q"))]
  s14 <- rr$mn[which(rr$tile %in% c("i","q","j"))]
  s15 <- rr$mn[which(rr$tile %in% c("j","q","r"))]
  s16 <- rr$mn[which(rr$tile %in% c("j","r","k"))]
  s17 <- rr$mn[which(rr$tile %in% c("k","r","l"))]
  s18 <- rr$mn[which(rr$tile %in% c("l","r","m"))]
  s19 <- rr$mn[which(rr$tile %in% c("s","r","m"))]
  s20 <- rr$mn[which(rr$tile %in% c("s","m","n"))]
  s21 <- rr$mn[which(rr$tile %in% c("s","n","o"))]
  s22 <- rr$mn[which(rr$tile %in% c("s","o","p"))]
  s23 <- rr$mn[which(rr$tile %in% c("s","p","q"))]
  s24 <- rr$mn[which(rr$tile %in% c("s","q","r"))]
  
  
  #which spots are incompatible with other spots? format (chosen spot, ... incompatible spots)
  inc1 <- c("s1","s2","s18",NA)
  inc2 <- c("s2","s1","s3",NA)
  inc3 <- c("s3","s2","s4","s20")
  inc4 <- c("s4","s3","s5",NA)
  inc5 <- c("s5","s4","s6",NA)
  inc6 <- c("s6","s5","s7","s21")
  inc7 <- c("s7","s6","s8",NA)
  inc8 <- c("s8","s7","s9",NA)
  inc9 <- c("s9","s8","s10","s22")
  inc10 <- c("s10","s9","s11",NA)
  inc11 <- c("s11","s10","s12",NA)
  inc12 <- c("s12","s11","s13","s23")
  inc13 <- c("s13","s12","s14",NA)
  inc14 <- c("s14","s13","s15",NA)
  inc15 <- c("s15","s14","s16","s24")
  inc16 <- c("s16","s15","s17",NA)
  inc17 <- c("s17","s16","s18",NA)
  inc18 <- c("s18","s17","s1","s19")
  inc19 <- c("s19","s18","s20","s24")
  inc20 <- c("s20","s19","s3","s21")
  inc21 <- c("s21","s20","s6","s22")
  inc22 <- c("s22","s21","s9","s23")
  inc23 <- c("s23","s22","s12","s24")
  inc24 <- c("s24","s23","s15","s19")
  
  incomp_base <- data.frame(inc1,inc2,inc3,inc4,inc5,inc6,inc7,inc8,inc9,inc10,inc11,inc12,inc13,inc14,inc15,inc16,inc17,inc18,inc19,inc20,inc21,inc22,inc23,inc24)
  incomp <- data.frame(t(incomp_base))
  names(incomp) <- c("spot","incomp1","incomp2","incomp3")
  
  #aggregate spots into a frame
  board <- data.frame(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16,s17,s18,s19,s20,s21,s22,s23,s24)
  
  #give sum and sd of spots, then give columns nice names
  board_sum <- cbind(melt(apply(board,2,sum)),melt(apply(board,2,sd)))
  board_sum$spot <- names(board)
  names(board_sum) <- c("sum","sd","spot")
  
  # "true rank" is determined by the mean of sum of points and sd of points. 
  # "true rank" is not ideal because it produces ties among spots.
  # "rank" solve this by producing random results to the ties. This just cleans up the method for later on
  # "true rank" method is even less ideal because it should be weighted towards highest number of points... not equally between the two aggregates
  # sum is given a weight of 3:1 to somewhat compensate for this... 
  # the other compensation is to rank sd for high volatility. By weighting sum heavily, this means that spots such as a 6/8 and a 3/11 are sorted 
  # above spots such as 5,4,2. Which is accurate since a 6/8 spot would be highly valuable.
  
  board_sum$true_rank <- apply(cbind(rank(-board_sum$sum)*4,rank(-board_sum$sd)),1,mean)
  board_sum$rank <- rank(board_sum$true_rank,ties.method="random")
  board_sum$sum_rank <- rank(-board_sum$sum,ties.method="min")
  board_sum$sd_rank <- rank(board_sum$sd,ties.method="min")
  board_sum <- board_sum[order(board_sum$rank,decreasing=FALSE),]
  
  
  results <- NULL
  pick <- NULL
  
  for (i in 1:4) { #for a 2 player game there will be 4 picks, 3 player = 6 picks, 4 player = 8 picks
    #pick the very best logical spot, again agnostic of resource scarcity
    #pick$choice <- paste("Pick #",i,sep="")
    pick <- board_sum[which(board_sum$rank == min(board_sum$rank)),]
    pick_name <- cbind(paste("Pick #",i,sep=""),board_sum[which(board_sum$rank == min(board_sum$rank)),])
    results <- rbind(results,pick_name)
    
    #now that we've picked we need to find which spots will be incompatible for the next pick
    pick_ex <- merge(pick,incomp,by.x="spot",by.y="spot")
    
    #antijoin all that stuff in series, then we repeat the loop
    board_sum <- anti_join(board_sum,pick_ex,by=c("spot"="spot"))
    board_sum <- anti_join(board_sum,pick_ex,by=c("spot"="incomp1"))
    board_sum <- anti_join(board_sum,pick_ex,by=c("spot"="incomp2"))
    board_sum <- anti_join(board_sum,pick_ex[which(is.na(pick_ex$incomp3) == FALSE),],by=c("spot"="incomp3"))
    
  }
  
  names(results) <- c("choice","sum","sd","spot","true_rank","rank","sum_rank","sd_rank")
  
  
  #produce some actual analysis
  #logic tree to make some conclusions
  if (nrow(results)==4) results <- cbind(results,c("Player1","Player2","Player1","Player2"),c("Player1","Player2","Player2","Player1"))
  if (nrow(results)==6) results <- cbind(results,c("Player1","Player2","Player3","Player1","Player2","Player3"),c("Player1","Player2","Player3","Player3","Player2","Player1"))
  if (nrow(results)==8) results <- cbind(results,c("Player1","Player2","Player3","Player4","Player1","Player2","Player3","Player4"),c("Player1","Player2","Player3","Player4","Player4","Player3","Player2","Player1"))
  if(nrow(results)==10) results <- cbind(results,c("Player1","Player2","Player3","Player4","Player5","Player1","Player2","Player3","Player4","Player5"),c("Player1","Player2","Player3","Player4","Player5","Player5","Player4","Player3","Player2","Player1"))
  if(nrow(results)==12) results <- cbind(results,c("Player1","Player2","Player3","Player4","Player5","Player6","Player1","Player2","Player3","Player4","Player5","Player6"),c("Player1","Player2","Player3","Player4","Player5","Player6","Player6","Player5","Player4","Player3","Player2","Player1"))
  
  results <- results[,c(9:10,1:8)]
  results <- cbind(rep(i,nrow(results)),results)
  names(results) <- c("iteration_num","sequential_player","serpentine_player","choice","sum","sd","spot","true_rank","rank","sum_rank","sd_rank")
  results$sd <- round(results$sd,2)
  
  #pre <- melt(results)
  #pre <- pre[which(pre$variable=="sum"),]
  #pre <- data.frame(pre$sequential_player,pre$serpentine_player,pre$value)
  #names(pre) <- c("sequential_player","serpentine_player","sum")
  #final_results <- sqldf("select 'serpentine' as draft_method,serpentine_player,sum(sum) as sum from pre group by 2 union all select '','','' from pick union all select 'sequential',sequential_player,sum(sum) as sum from pre group by 2")
  agg_results <- rbind(agg_results,results)
  
}

res <- sqldf("select 'serpentine' as draft_method,serpentine_player as player,sum(sum) as sum from agg_results group by 2 union all select 'sequential',sequential_player,sum(sum) as sum from agg_results group by 2")
sr <- res[which(res$draft_method=="serpentine"),]
srtop <- data.frame(sr$player[which(sr$sum==max(sr$sum))], ((max(sr$sum)-mean(sr$sum[which(sr$sum!=max(sr$sum))]))/mean(sr$sum[which(sr$sum!=max(sr$sum))]))*1,((max(sr$sum)-min(sr$sum[which(sr$sum!=max(sr$sum))]))/min(sr$sum[which(sr$sum!=max(sr$sum))])))
names(srtop) <- c("player","avg_advantage","least_advantage")
srtop$avg_advantage <- paste(round(srtop$avg_advantage*100,2),"%",sep="")
srtop$least_advantage <- paste(round(srtop$least_advantage*100,2),"%",sep="")
serpentine <- left_join(sr,srtop)

sq <- res[which(res$draft_method=="sequential"),]
sqtop <- data.frame(sq$player[which(sq$sum==max(sq$sum))], ((max(sq$sum)-mean(sr$sum[which(sq$sum!=max(sq$sum))]))/mean(sr$sum[which(sq$sum!=max(sq$sum))]))*1,((max(sq$sum)-min(sr$sum[which(sq$sum!=max(sq$sum))]))/min(sr$sum[which(sq$sum!=max(sq$sum))])))
names(sqtop) <- c("player","avg_advantage","least_advantage")
sqtop$avg_advantage <- paste(round(sqtop$avg_advantage*100,2),"%",sep="")
sqtop$least_advantage <- paste(round(sqtop$least_advantage*100,2),"%",sep="")
sequential <- left_join(sq,sqtop)


list(serpentine,sequential)
