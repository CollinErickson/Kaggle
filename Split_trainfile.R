folderpath <- "./Earthquake/"
system.time(train1 <- read.csv(paste0(folderpath, "train.csv"), nrows = 1e7, header = T))
as.numeric(object.size(train1))/1e6
train1 %>% head
train1$rownum <- 1:nrow(train1) + 0
write.csv(train1, paste0("E://Earthquake//data", 1, ".csv"), row.names = F)

for (i in 2:34) {
  print(i)
  system.time(train1 <- read.csv(paste0(folderpath, "train.csv"), nrows = 1e7, header = F, skip=1+1e7*(i-1)))
  colnames(train1) <- c("acoustic_data", "time_to_failure")
  train1$rownum <- 1:nrow(train1) + 1e7*(i-1)
  pathi <- paste0("E://Earthquake//data", i, ".csv")
  if (file.exists(pathi)) {stop("File already exists")}
  write.csv(train1, pathi, row.names = F)
}
