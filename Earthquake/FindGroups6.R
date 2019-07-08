# FindGroups6.R.
# Groups of size 150,000
# Uses Nskip to get 10x data.
# Want to run in parallel

# folderpath <- "./Earthquake/"
folderpath <- "~/scratch/earthquake/"

library(foreach)
library(doParallel)

registerDoParallel(32)

# nlines <- 629145481

# if (file.exists(savegroupinfofilepath)) {stop("file exists")}
foreach (i.file = 1:63) %do% {
  
  filemap <- apply(expand.grid(letters, letters[1:3])[,c(2,1)], 1, function(x) paste0(x,collapse=''))[1:63]
  
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
  
  # They need to write to separate files so they don't interrupt each other
  savegroupinfofilepath <- paste0(folderpath,"GroupInfo6sep/GroupInfo6-",i.file,".csv")
  
  
  
  
  
  
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
      tail(traini_chunk$time_to_failure, 1), # Need time to next failure, won't be min anymore
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
    endrow <- startrow + N - 1
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

