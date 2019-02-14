folderpath <- "./Earthquake/"
testfiles <- list.files(paste0(folderpath,"test/"))
testfiles_nocsv <- strsplit(testfiles, ".csv") %>% unlist
preds <- rep(0, length(testfiles))
pdf <- data.frame(seg_id=testfiles_nocsv, time_to_failure=preds)
pdf %>% head()
write.csv(x = pdf, file = paste0(folderpath, "Sub1-zeros.csv"), row.names = FALSE)
pdf %>% str

# Not working for me, has quotes
subfilepath <- paste0(folderpath, "Sub1-zeros.csv")
cat(file = subfilepath, "seg_id,time_to_failure\n")
for (testfile in testfiles) {
  testfile_nocsv <- strsplit(testfile, ".csv")[[1]]
  cat(file=subfilepath, testfile_nocsv, ",0\n", append = TRUE, sep = "")
}


# Predict using the mean
# Since csv is so big, get sample of rows
# There are about 633 million rows, so need to use a sample method
# lf <- LaF::laf_open_csv(LaF::detect_dm_csv(filename=paste0(folderpath,"train.csv"), sep=",", header=TE, factor_fraction = -1))
# sampdf <- LaF::read_lines(lf, sample(1:nrow(lf), 100))
# sampdf <- DMwR2::sampleCSV(file=paste0(folderpath,"train.csv"), nrLines = 100, header = TRUE)

library(LaF)

sample1 <- function(file, n) {
  lf <- laf_open(detect_dm_csv(file, sep = ",", header = TRUE, factor_fraction = -1))
  return(read_lines(lf, sample(1:nrow(lf), n)))
}

sampdf <- sample1(paste0(folderpath,"train.csv"), 1e4)
sampdf %>% str
sampdf %>% summary

mean_value <- mean(sampdf$time_to_failure)
mean_value # should be about 5.4541
mean_value <- 5.4541

subfilepath <- paste0(folderpath, "Sub1-mean.csv")
cat(file = subfilepath, "seg_id,time_to_failure\n")
for (testfile in testfiles) {
  testfile_nocsv <- strsplit(testfile, ".csv")[[1]]
  cat(file=subfilepath, testfile_nocsv, ",", mean_value,"\n", append = TRUE, sep = "")
}



# Try to read and skip a lot or rows, is it slow?
system.time(tmpdf <- read.csv(paste0(folderpath,"train.csv"), skip = 10000, nrows = 100, header=FALSE))
system.time(tmpdf <- read.csv(paste0(folderpath,"train.csv"), skip = 100000, nrows = 100, header=FALSE))
system.time(tmpdf <- read.csv(paste0(folderpath,"train.csv"), skip = 1000000, nrows = 100, header=FALSE))
system.time(tmpdf <- read.csv(paste0(folderpath,"train.csv"), skip = 10000000, nrows = 100, header=FALSE))
system.time(tmpdf <- readr::read_csv(paste0(folderpath,"train.csv"), skip = 1e7, n_max = 100))
system.time(tmpdf2 <- data.table::fread(paste0(folderpath,"train.csv"), nrows = 100, skip = 1e7, header = F))
system.time(tmpdf <- readr::read_csv(paste0(folderpath,"train.csv"), skip = 1e8, n_max = 100))
system.time(tmpdf2 <- data.table::fread(paste0(folderpath,"train.csv"), skip = 1e8, nrows=100, header = F))
