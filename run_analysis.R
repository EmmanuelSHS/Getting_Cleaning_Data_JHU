# setwd("D:\\Dream\\Documents\\Data Science\\Getting&Cleaning Data\\cp")

# Read and Merge the raw data
set_tes <- read.table(".\\UCI HAR Dataset\\test\\X_test.txt")
set_tra <- read.table(".\\UCI HAR Dataset\\train\\X_train.txt")
names <- read.table(".\\UCI HAR Dataset\\features.txt")
colnames(set_tes) <- names[,2]
colnames(set_tra) <- names[,2]
lbs_tes <- read.table(".\\UCI HAR Dataset\\test\\y_test.txt")
lbs_tra <- read.table(".\\UCI HAR Dataset\\train\\y_train.txt")
tes <- cbind(lbs_tes, set_tes)
tra <- cbind(lbs_tra, set_tra)
set_sum <- rbind(tes, tra)

# Subtract the mean & deviation features
set_1 <- cbind(set_sum[,1], 
               set_sum[,colnames(set_sum)[grep("(^([a-zA-Z]*)-[Mm]ean)|(^([a-zA-Z]*)-std)", colnames(set_sum))]])
colnames(set_1)[1] <- "activity"

# Merge by activity and get the average
set_2 <- data.frame()
for (i in 1:6){
  temp <- set_1[set_1[,1] == i, ]
  m <- apply(temp, 2, mean)
  set_2 <- rbind(set_2, m)
}
colnames(set_2) <- colnames(set_1)

# Return a tidy data in my own way
data <- matrix(nrow = 474, ncol = 9)
res <- data.frame(data)
colnames(res) <- c("activity", "domain", "type", "source", "dimension", "jerk", "magnitude", "value.type", "value")

res$activity <- rep(c(1, 2, 3, 4, 5, 6), times = 79)
for (j in 2:80){
  cp_tmp <- strsplit(colnames(set_2[j]), split = "-")[[1]][1]
  cp <- strsplit(cp_tmp, split = "[A-Z]")
  nc <- sum(table(cp))
  vt <- strsplit(colnames(set_2)[j], split = "-")[[1]][2]
  dm <- strsplit(colnames(set_2)[j], split = "-")[[1]][3]
  
  if (is.na(dm) == TRUE){
    res[(6*(j-2)+1):(6*(j-2)+6), 5] <- rep(0, times = 6)
  }
  else{
    if (dm == "X"){
      res[(6*(j-2)+1):(6*(j-2)+6), 5] <- rep(1, times = 6)
    }
    else if (dm == "Y"){
      res[(6*(j-2)+1):(6*(j-2)+6), 5] <- rep(2, times = 6)
    }
    else if (dm == "Z"){
      res[(6*(j-2)+1):(6*(j-2)+6), 5] <- rep(3, times = 6)
    }
  }
  
  if (vt == "mean()"){
    res[(6*(j-2)+1):(6*(j-2)+6), 8] <- rep(0, times = 6)
  }
  else if (vt == "meanFreq()"){
    res[(6*(j-2)+1):(6*(j-2)+6), 8] <- rep(0, times = 6)  
  }
  else{
    res[(6*(j-2)+1):(6*(j-2)+6), 8] <- rep(1, times = 6)
  }
  
  if (nc == 3){
    res[(6*(j-2)+1):(6*(j-2)+6), 6] <- rep(0, times = 6)
    res[(6*(j-2)+1):(6*(j-2)+6), 7] <- rep(0, times = 6)    
  }
  else if (nc == 4){
    if (cp[[1]][4] == "erk"){
      res[(6*(j-2)+1):(6*(j-2)+6), 6] <- rep(1, times = 6)
      res[(6*(j-2)+1):(6*(j-2)+6), 7] <- rep(0, times = 6)
    }
    else{
      res[(6*(j-2)+1):(6*(j-2)+6), 6] <- rep(1, times = 6)
      res[(6*(j-2)+1):(6*(j-2)+6), 7] <- rep(0, times = 6)
    }
  }
  else if (nc == 5){
    res[(6*(j-2)+1):(6*(j-2)+6), 6] <- rep(1, times = 6)
    res[(6*(j-2)+1):(6*(j-2)+6), 7] <- rep(1, times = 6)
  }
  else{
    if (cp[[1]][5] == "erk"){
      if ((cp[[1]][6] == "ag")){
        res[(6*(j-2)+1):(6*(j-2)+6), 6] <- rep(1, times = 6)
        res[(6*(j-2)+1):(6*(j-2)+6), 7] <- rep(1, times = 6)
      }
      else{
        res[(6*(j-2)+1):(6*(j-2)+6), 6] <- rep(1, times = 6)
        res[(6*(j-2)+1):(6*(j-2)+6), 7] <- rep(0, times = 6)
      }
    }
    else if(cp[[1]][5] == "ag"){
      res[(6*(j-2)+1):(6*(j-2)+6), 6] <- rep(0, times = 6)
      res[(6*(j-2)+1):(6*(j-2)+6), 7] <- rep(1, times = 6)
    }
  }
  dom <- cp[[1]][1]
  typ <- cp[[1]][2]
  sou <- cp[[1]][3]
  
  if (dom == "t"){
    res[(6*(j-2)+1):(6*(j-2)+6), 2] <- rep(0, times = 6)
  }
  else if (dom == "f"){
    res[(6*(j-2)+1):(6*(j-2)+6), 2] <- rep(1, times = 6)
  }
  
  if (typ == "ody"){
    res[(6*(j-2)+1):(6*(j-2)+6), 3] <- rep(0, times = 6)
  }
  else{
    res[(6*(j-2)+1):(6*(j-2)+6), 3] <- rep(1, times = 6)
  }
  
  if (sou == "cc"){
    res[(6*(j-2)+1):(6*(j-2)+6), 4] <- rep(0, times = 6)
  }
  else{
    res[(6*(j-2)+1):(6*(j-2)+6), 4] <- rep(1, times = 6)
  }
  res[(6*(j-2)+1), 9] <- set_2[1,j]
  res[(6*(j-2)+2), 9] <- set_2[2,j]
  res[(6*(j-2)+3), 9] <- set_2[3,j]
  res[(6*(j-2)+4), 9] <- set_2[4,j]
  res[(6*(j-2)+5), 9] <- set_2[5,j]
  res[(6*(j-2)+6), 9] <- set_2[6,j]  
}
write.csv(res, file = "tidy_data.csv", row.names = FALSE)
