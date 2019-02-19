# These are group by time, so size 4096.
# Adding new features compared to FindGroups.R,
# the same that were in FindGroups2.R and sd of sd.

folderpath <- "./Earthquake/"

nlines <- 629145481
filemap <- apply(expand.grid(letters, letters[1:3])[,c(2,1)], 1, function(x) paste0(x,collapse=''))[1:63]


# numperfile <- 1000*100
# newcsvpath <- paste0(folderpath,"trainsmall2.csv")
# if (file.exists(newcsvpath)) {stop("file exists 87987")}
eps <- .0000001
for (i in 1:63) {
  print(i)
  # skipi <- ceiling(runif(1)*(nlines-10))
  # tmpdf <- data.table::fread(paste0(folderpath,"train.csv"), nrows = chunksize, skip = skipi, header = F)
  traini <- data.table::fread(paste0(folderpath,"train/trainsplit", filemap[i],".csv"), header = (i==1))
  colnames(traini) <- c("acoustic_data", "time_to_failure")
  # indsi <- sample(1:nrow(tmpdfall), numperfile, replace=F)
  # tmpdf <- tmpdfall[indsi,]
  # print(str(tmpdf))
  # write.table(x=tmpdf,file=newcsvpath,append=TRUE, row.names = FALSE, col.names=FALSE, sep=",")
  # read.csv(newcsvpath,header = F)
  startrow <- 1#4096 # 1
  tmin <- traini$time[startrow]
  tmax <- traini$time[startrow]
  i <- startrow + 1
  n <- 1
  while(traini$time[i] >= tmin-eps && traini$time[i] <= tmax+eps) {
    tmin <- min(tmin, traini$time[i])
    tmax <- max(tmax, traini$time[i])
    i <- i+1
    n <- n+1
  }
  stop()
}


# for aa, 1:4095 is first group
plot(traini$acoustic_data[1:4095])
plot(traini$time_to_failure[1:4095])
plot(traini$acoustic_data[1:4095+4095])
plot(traini$time_to_failure[1:4095+4095])
plot(traini$acoustic_data[1:4095+4095*2+1])
plot(traini$time_to_failure[1:4095+4095*2+1])







# Find all groups
eps <- .0000001
i.file <- 1
i <- 1
traini <- NULL
groupnum <- 1
savegroupinfofilepath <- paste0(folderpath,"GroupInfo3.csv")

while(TRUE) { # Loop over groups
  # print(i)
  # Load file if needed
  if (is.null(traini)) {
    traini <- data.table::fread(paste0(folderpath,"train/trainsplit", filemap[i.file],".csv"), header = (i.file==1))
    colnames(traini) <- c("acoustic_data", "time_to_failure")
  }
  startrow <- i#4096 # 1
  tmin <- traini$time[startrow]
  tmax <- traini$time[startrow]
  i <- startrow + 1
  n <- 1
  # This will split up groups between files
  while(i <= nrow(traini) && traini$time[i] >= tmin-eps && traini$time[i] <= tmax+eps) {
    tmin <- min(tmin, traini$time[i])
    tmax <- max(tmax, traini$time[i])
    i <- i+1
    n <- n+1
  }
  endrow <- i-1
  if (i > nrow(traini)) {
    traini <- NULL
    i.file <- i.file + 1
    i <- 1
    next()
  }
  
  if (F) {# slower
    plot(traini$acoustic_data[startrow:(i-1)], main=round(mean(traini$time_to_failure[startrow:(i-1)]),4))
  }
  cat("finished ", i.file, i, n, '\n')
  # Next group
  groupnum <- groupnum + 1
  if (endrow > startrow) {
    traini_chunk <- traini[startrow:endrow]
    mean_acoustic <- mean(traini_chunk$acoustic_data)
    sd_acoustic <- sd(  traini_chunk$acoustic_data)
    absz <- abs(traini_chunk$acoustic_data - mean_acoustic) / sd_acoustic
    cat(groupnum, i.file, n, startrow, endrow,
        mean(traini_chunk$time_to_failure),
        min( traini_chunk$time_to_failure),
        max( traini_chunk$time_to_failure),
        mean_acoustic, #mean(traini_chunk$acoustic_data),
        min( traini_chunk$acoustic_data),
        max( traini_chunk$acoustic_data),
        sd_acoustic, #sd(  traini_chunk$acoustic_data)
        quantile(absz, c(.25,.5,.75,.9,.95,.99,.999, 1)),
        sum(absz>1)/n,sum(absz>3)/n,sum(absz>10)/n,
        sd(sapply(split(traini_chunk$acoustic_data[1:min(4096,n)], rep(1:40, each=128)[1:n]), sd)),
        file=savegroupinfofilepath, sep=',', append=T)
    cat("\n", file=savegroupinfofilepath, append=T)
  } else { # single point in group
    cat(paste("single point in group", groupnum, startrow, endrow, i.file))
  }
  # stop()
}

# Looks like groups come in sizes of 4096