# Make new data file, a subset of train.csv
folderpath <- "./Earthquake/"
nlines <- 629145481
filemap <- apply(expand.grid(letters, letters[1:3])[,c(2,1)], 1, function(x) paste0(x,collapse=''))[1:63]


numperfile <- 1000*100
newcsvpath <- paste0(folderpath,"trainsmall2.csv")
if (file.exists(newcsvpath)) {stop("file exists 87987")}
for (i in 1:63) {
  print(i)
  # skipi <- ceiling(runif(1)*(nlines-10))
  # tmpdf <- data.table::fread(paste0(folderpath,"train.csv"), nrows = chunksize, skip = skipi, header = F)
  tmpdfall <- data.table::fread(paste0(folderpath,"train/trainsplit", filemap[i],".csv"), header = (i==1))
  colnames(tmpdfall) <- c("acoustic_data", "time_to_failure")
  indsi <- sample(1:nrow(tmpdfall), numperfile, replace=F)
  tmpdf <- tmpdfall[indsi,]
  print(str(tmpdf))
  write.table(x=tmpdf,file=newcsvpath,append=TRUE, row.names = FALSE, col.names=FALSE, sep=",")
  # read.csv(newcsvpath,header = F)
}

trains <- read.csv(newcsvpath, header = F)
trains %>% str
trains %>% summary
trains %>% plot
ggplot(data=trains, mapping=aes(V1, V2)) + geom_point() + scale_y_log10() + geom_smooth()
