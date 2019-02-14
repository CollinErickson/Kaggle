folderpath <- "./Earthquake/"
trainpath <- paste0(folderpath,"trainsmall2.csv")

train <- data.table::fread(file=trainpath, header=F)
train %>% str
train %>% head

train %>% plot
gam1 <- mgcv::gam(V2 ~ V1, data=train)
curve(predict(gam1, data.frame(V1=x)), from=-6000, to=6000, col=2, add=T)
gam2 <- mgcv::gam(log(V2) ~ V1, data=train)
curve(exp(predict(gam2, data.frame(V1=x))), from=-6000, to=6000, col=3, add=T)


testfolderpath <- paste0(folderpath, "test/")
testsubmissionpath <- paste0(folderpath,"Sub3-gam.csv")
cat(file=testsubmissionpath, "seg_id,time_to_failure\n")
i <- 0
for (testfile in list.files(testfolderpath)) {
  i <- i+1; cat(i, testfile, "\n")
  testdf <- read.csv(paste0(testfolderpath, testfile))
  names(testdf) <- "V1"
  testpred <- predict(gam1, testdf)
  testfile_nocsv <- strsplit(testfile, ".csv")[[1]]
  cat(paste0(testfile_nocsv, ",", mean(testpred), "\n"), file=testsubmissionpath, sep = "",append=TRUE)
  # stop()
}
