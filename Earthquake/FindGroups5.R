# FindGroups5.R
# These are groups of size 150000, the size of testing.
# Added new features of FindGroups4

# Also doing Groups6 here. It uses Nskip to get 10x data.

folderpath <- "./Earthquake/"

nlines <- 629145481
filemap <- apply(expand.grid(letters, letters[1:3])[,c(2,1)], 1, function(x) paste0(x,collapse=''))[1:63]


# numperfile <- 1000*100
# newcsvpath <- paste0(folderpath,"trainsmall2.csv")
# if (file.exists(newcsvpath)) {stop("file exists 87987")}
# eps <- .0000001

extract_chunk_acoustic_features <- function(x) {
  if (length(x) != 150000) {stop("x is wrong length")}
  
  # All good so calculate stats
  mean_acoustic <- mean(x)
  sd_acoustic <- sd(  x)
  absz <- abs(x - mean_acoustic) / sd_acoustic
  N <- length(x)
  c(
    mean_acoustic, 
    min( x),
    max( x),
    sd_acoustic, 
    quantile(x, c(.001,.01,.05,.25,.5,.75,.9,.95,.99,.999)),
    quantile(absz, c(.25,.5,.75,.9,.95,.99,.999, 1)),
    sum(absz>1)/N,sum(absz>3)/N,sum(absz>5)/N,
    mean(x[1:1000]),mean(x[1:10000]),mean(x[1:50000]),
    mean(x[149001:150000]),mean(x[140000:150000]),mean(x[100001:150000]),
    sd(x[1:1000]),sd(x[1:10000]),sd(x[1:50000]),
    sd(x[149001:150000]),sd(x[140000:150000]),sd(x[100001:150000]),
    sd(sapply(split(x, rep(1:10, each=150000/10)),mean)),
    sd(sapply(split(x, rep(1:100, each=150000/100)),mean)),
    sd(sapply(split(x, rep(1:10, each=150000/10)),sd)),
    sd(sapply(split(x, rep(1:100, each=150000/100)),sd)),
    mean(sapply(split(x, rep(1:10, each=150000/10)),sd)),
    mean(sapply(split(x, rep(1:100, each=150000/100)),sd)),
    quantile(RcppRoll::roll_mean(x, n=1000), c(.01,.1,.5,.9,.99)),
    quantile(RcppRoll::roll_sd(  x, n=1000), c(.01,.1,.5,.9,.99)),
    lm(yy~xx, data.frame(xx=1:150000, yy=x))$coeff,
    lm(yy~xx, data.frame(xx=1:150000, yy=abs(x)))$coeff,
    lm(yy~xx, data.frame(xx=120000:150000, yy=x[120000:150000]))$coeff,
    lm(yy~xx, data.frame(xx=120000:150000, yy=abs(x[120000:150000])))$coeff
    # Should do mean of sd
  )
}

N <- 150000
Nskip <- 150000/10 # Do overlapping groups
savegroupinfofilepath <- paste0(folderpath,"GroupInfo5???.csv")
if (file.exists(savegroupinfofilepath)) {stop("file exists")}
for (i.file in 1:63) {
  # print(i)
  # skipi <- ceiling(runif(1)*(nlines-10))
  # tmpdf <- data.table::fread(paste0(folderpath,"train.csv"), nrows = chunksize, skip = skipi, header = F)
  traini <- data.table::fread(paste0(folderpath,"train/trainsplit", filemap[i.file],".csv"), header = (i.file==1))
  colnames(traini) <- c("acoustic_data", "time_to_failure")
  # indsi <- sample(1:nrow(tmpdfall), numperfile, replace=F)
  # tmpdf <- tmpdfall[indsi,]
  # print(str(tmpdf))
  # write.table(x=tmpdf,file=newcsvpath,append=TRUE, row.names = FALSE, col.names=FALSE, sep=",")
  # read.csv(newcsvpath,header = F)
  startrow <- 1#4096 # 1
  endrow <- startrow+N-1
  while(endrow < nrow(traini)) {
    print(c(i.file, startrow))
    traini_chunk <- traini[startrow:endrow]
    # Make sure all from same group
    # Don't do this since testing will have same thing
    # if (any(abs(diff(traini_chunk$time_to_failure)) > 1)) {#browser()
    #   print("going to next since chunk moves too much, summary is")
    #   print(summary)
    #   # Increment for next section
    #   startrow <- endrow+1
    #   endrow <- startrow + N-1
    #   next()
    # }
    chunkstats <- c(
      i.file, startrow, endrow,
      mean(traini_chunk$time_to_failure),
      min( traini_chunk$time_to_failure),
      max( traini_chunk$time_to_failure),
      extract_chunk_acoustic_features(traini_chunk$acoustic_data)
    )
    # Write to file
    cat(chunkstats,
        file=savegroupinfofilepath, sep=',', append=T)
    cat("\n", file=savegroupinfofilepath, append=T)
    
    # Increment for next section, NEED this before next above too!
    # startrow <- endrow+1
    # endrow <- startrow + N-1
    startrow <- startrow + Nskip
    end <- startrow + N - 1
  }
  # n <- 1
  # while(traini$time[i] >= tmin-eps && traini$time[i] <= tmax+eps) {
  #   tmin <- min(tmin, traini$time[i])
  #   tmax <- max(tmax, traini$time[i])
  #   i <- i+1
  #   n <- n+1
  # }
  # stop()
  i.file
}

# 
# # for aa, 1:4095 is first group
# plot(traini$acoustic_data[1:4095])
# plot(traini$time_to_failure[1:4095])
# plot(traini$acoustic_data[1:4095+4095])
# plot(traini$time_to_failure[1:4095+4095])
# plot(traini$acoustic_data[1:4095+4095*2+1])
# plot(traini$time_to_failure[1:4095+4095*2+1])
# 
# 
# 
# 
# 
# 
# 
# # Find all groups
# eps <- .0000001
# i.file <- 1
# i <- 1
# traini <- NULL
# groupnum <- 1
# savegroupinfofilepath <- paste0(folderpath,"GroupInfo.csv")
# 
# while(TRUE) { # Loop over groups
#   print(i)
#   # Load file if needed
#   if (is.null(traini)) {
#     traini <- data.table::fread(paste0(folderpath,"train/trainsplit", filemap[i.file],".csv"), header = (i.file==1))
#     colnames(traini) <- c("acoustic_data", "time_to_failure")
#   }
#   startrow <- i#4096 # 1
#   tmin <- traini$time[startrow]
#   tmax <- traini$time[startrow]
#   i <- startrow + 1
#   n <- 1
#   # This will split up groups between files
#   while(i <= nrow(traini) && traini$time[i] >= tmin-eps && traini$time[i] <= tmax+eps) {
#     tmin <- min(tmin, traini$time[i])
#     tmax <- max(tmax, traini$time[i])
#     i <- i+1
#     n <- n+1
#   }
#   endrow <- i-1
#   if (i > nrow(traini)) {
#     traini <- NULL
#     i.file <- i.file + 1
#     i <- 1
#   }
#   
#   if (F) {# slower
#     plot(traini$acoustic_data[startrow:(i-1)], main=round(mean(traini$time_to_failure[startrow:(i-1)]),4))
#   }
#   cat("finished ", i.file, i, n, '\n')
#   # Next group
#   groupnum <- groupnum + 1
#   if (endrow > startrow) {
#     cat(groupnum, i.file, startrow, endrow,
#         mean(traini$time_to_failure[startrow:endrow]),
#         min( traini$time_to_failure[startrow:endrow]),
#         max( traini$time_to_failure[startrow:endrow]),
#         mean(traini$acoustic_data[startrow:endrow]),
#         min( traini$acoustic_data[startrow:endrow]),
#         max( traini$acoustic_data[startrow:endrow]),
#         sd(  traini$acoustic_data[startrow:endrow]),
#         file=savegroupinfofilepath, sep=',', append=T)
#     cat("\n", file=savegroupinfofilepath, append=T)
#   } else { # single point in group
#     cat(paste("single point in group", groupnum, startrow, endrow, i.file))
#   }
#   # stop()
# }
# 
# # Looks like groups come in sizes of 4096
