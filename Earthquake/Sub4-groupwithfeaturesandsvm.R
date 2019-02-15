folderpath <- "./Earthquake/"

filemap <- apply(expand.grid(letters, letters[1:3])[,c(2,1)], 1, function(x) paste0(x,collapse=''))[1:63]
groupinfofilepath <- paste0(folderpath,"GroupInfo.csv")
gdf <- read.csv(groupinfofilepath, header = F)
gdf %>% head
colnames(gdf) <- c("groupnum", "i.file", "startrow", "endrow",
                   "meantime","mintime","maxtime",
                   "meanacoustic","minacoustic","maxacoustic","sdacoustic")
gdf$nrow <- gdf$endrow - gdf$startrow
gdf %>% head
gdf %>% str()
gdf %>% summary
plot(gdf$meanacoustic, gdf$meantime)
plot(gdf$sdacoustic, gdf$meantime)
summary(lm(meantime ~ meanacoustic, gdf))$r.sq
summary(lm(meantime ~ meanacoustic + sdacoustic, gdf))$r.sq
summary(lm(meantime ~ sdacoustic, gdf))$r.sq

gdf$minacoustic %>% is.infinite() %>% table
gdf$maxacoustic %>% is.infinite() %>% table
gdf[(gdf$maxacoustic %>% is.infinite()),]

gdf2 <- gdf[!(gdf$maxacoustic %>% is.infinite()),]
summary(lm(meantime ~ meanacoustic + sdacoustic, gdf2))$r.sq
gdf2$rangeacoustic <- gdf2$maxacoustic - gdf2$minacoustic
plot(gdf2$rangeacoustic, gdf2$meantime)
plot(gdf2$sdacoustic / gdf2$rangeacoustic, gdf2$meantime)
summary(lm(meantime ~ meanacoustic + sdacoustic + rangeacoustic, gdf2))$r.sq


lm3 <- lm(meantime ~ meanacoustic + sdacoustic + rangeacoustic, gdf2)
SGGP::valstats(predict(lm3, gdf2), rep(0,nrow(gdf2)), gdf2$meantime)


tr.inds <- sample(1:nrow(gdf2), floor(.7*nrow(gdf2)))
tr1 <- gdf2[tr.inds,]
te1 <- gdf2[-tr.inds,]

lm1 <- lm(meantime ~ meanacoustic + sdacoustic + rangeacoustic, tr1)
summary(lm1)$r.sq
pr1tr <- predict(lm1, tr1, se=T)
SGGP::valstats(pr1tr$fit, pr1tr$se.fit, tr1$meantime)
pr1te <- predict(lm1, te1, se=T)
SGGP::valstats(pr1te$fit, pr1te$se.fit, te1$meantime)

svm1 <- e1071::svm(meantime ~ meanacoustic + sdacoustic + rangeacoustic, data=tr1)
pr.svm1tr <- predict(svm1, tr1, se=T)
SGGP::valstats(pr.svm1tr, rep(1,nrow(tr1)), tr1$meantime)
pr.svm1te <- predict(svm1, te1, se=T)
SGGP::valstats(pr.svm1te, rep(1,nrow(te1)), te1$meantime)
plot(te1$meantime, pr.svm1te)

svm2 <- e1071::svm(meantime ~ meanacoustic + sdacoustic + rangeacoustic + I(sdacoustic/rangeacoustic), data=tr1)
pr.svm2tr <- predict(svm2, tr1, se=T)
SGGP::valstats(pr.svm2tr, rep(1,nrow(tr1)), tr1$meantime)
pr.svm2te <- predict(svm2, te1, se=T)
SGGP::valstats(pr.svm2te, rep(1,nrow(te1)), te1$meantime)
plot(te1$meantime, pr.svm2te);abline(a=0,b=1,col=2)


svm3 <- e1071::svm(meantime ~ meanacoustic + sdacoustic + rangeacoustic + I(sdacoustic/rangeacoustic) + I(meanacoustic/sdacoustic), data=tr1)
pr.svm3tr <- predict(svm3, tr1, se=T)
SGGP::valstats(pr.svm3tr, rep(1,nrow(tr1)), tr1$meantime)
pr.svm3te <- predict(svm3, te1, se=T)
SGGP::valstats(pr.svm3te, rep(1,nrow(te1)), te1$meantime)
plot(te1$meantime, pr.svm3te);abline(a=0,b=1,col=2)

getDmatrix1 <- function(x) {
  xgboost::xgb.DMatrix(cbind(x$meanacoustic, x$sdacoustic, x$rangeacoustic, x$sdacoustic/x$rangeacoustic + x$meanacoustic/x$sdacoustic), label=x$meantime)
}
xgb1 <- xgboost::xgb.train(data=getDmatrix1(tr1), nrounds = 300)
pr.xgb1tr <- predict(xgb1, getDmatrix1(tr1), se=T)
SGGP::valstats(pr.xgb1tr, rep(1,nrow(tr1)), tr1$meantime)
pr.xgb1te <- predict(xgb1, getDmatrix1(te1), se=T)
SGGP::valstats(pr.xgb1te, rep(1,nrow(te1)), te1$meantime)
plot(te1$meantime, pr.xgb1te);abline(a=0,b=1,col=2)


# Make predictions for submission
if (F) {
  testfolderpath <- paste0(folderpath, "test/")
  testsubmissionpath <- paste0(folderpath,"Sub4-svm.csv")
  cat(file=testsubmissionpath, "seg_id,time_to_failure\n")
  i <- 0
  for (testfile in list.files(testfolderpath)) {
    i <- i+1; cat(i, testfile, "\n")
    testdf <- read.csv(paste0(testfolderpath, testfile))
    names(testdf) <- "V1"
    testpred <- predict(svm3, data.frame(meanacoustic=mean(testdf$V1),
                                                     rangeacoustic=max(testdf$V1)-min(testdf$V1),
                                                     sdacoustic=sd(testdf$V1)))
    # testpred <- predict(svm3, getDmatrix1(data.frame(meanacoustic=mean(testdf$V1),
    #                                                  rangeacoustic=max(testdf$V1)-min(testdf$V1),
    #                                                  sdacoustic=sd(testdf$V1),
    #                                                  meantime=NaN)))
    testfile_nocsv <- strsplit(testfile, ".csv")[[1]]
    cat(paste0(testfile_nocsv, ",", (testpred), "\n"), file=testsubmissionpath, sep = "",append=TRUE)
    # stop()
  }
}