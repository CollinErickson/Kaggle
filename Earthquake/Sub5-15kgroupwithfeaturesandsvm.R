folderpath <- "./Earthquake/"

filemap <- apply(expand.grid(letters, letters[1:3])[,c(2,1)], 1, function(x) paste0(x,collapse=''))[1:63]
groupinfofilepath <- paste0(folderpath,"GroupInfo2.csv")
gdf <- read.csv(groupinfofilepath, header = F)
gdf %>% head
colnames(gdf) <- c("i.file", "startrow", "endrow",
                   "meantime","mintime","maxtime",
                   "meanacoustic","minacoustic","maxacoustic","sdacoustic",
                   "q25", "q50","q75", "q90", "q95","q99","q999","q1",
                   "p1","p3","p10")
# gdf$nrow <- gdf$endrow - gdf$startrow
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

# Remove bad rows, remove unneeded columns
gdf2 <- gdf[!(gdf$maxacoustic %>% is.infinite()), -c(1,2,3,5,6)]
summary(lm(meantime ~ meanacoustic + sdacoustic, gdf2))$r.sq
gdf2$rangeacoustic <- gdf2$maxacoustic - gdf2$minacoustic
plot(gdf2$rangeacoustic, gdf2$meantime)
plot(gdf2$sdacoustic / gdf2$rangeacoustic, gdf2$meantime)
summary(lm(meantime ~ meanacoustic + sdacoustic + rangeacoustic, gdf2))$r.sq
gdf2.colmeans <- colMeans(gdf2)
gdf2.colsd <- apply(gdf2, 2, sd)
gdf2.norm <- cbind(meantime=gdf2[,1], # Don't normalize first column
                   sweep(sweep(gdf2[,-1],2,gdf2.colmeans[-1]), 2, gdf2.colsd[-1], `/`)
)
gdf2.norm %>% summary

lm3 <- lm(meantime ~ meanacoustic + sdacoustic + rangeacoustic, gdf2)
SGGP::valstats(predict(lm3, gdf2), rep(0,nrow(gdf2)), gdf2$meantime)


tr.inds <- sample(1:nrow(gdf2), floor(.7*nrow(gdf2)))
tr1 <- gdf2.norm[tr.inds,]
te1 <- gdf2.norm[-tr.inds,]

lm0 <- lm(meantime ~ 1, tr1)
summary(lm0)$r.sq
pr0tr <- predict(lm0, tr1, se=T)
SGGP::valstats(pr0tr$fit, pr0tr$se.fit, tr1$meantime)
pr0te <- predict(lm0, te1, se=T)
SGGP::valstats(pr0te$fit, pr0te$se.fit, te1$meantime)

lm1 <- lm(meantime ~ meanacoustic + sdacoustic + rangeacoustic, tr1)
summary(lm1)$r.sq
pr1tr <- predict(lm1, tr1, se=T)
SGGP::valstats(pr1tr$fit, pr1tr$se.fit, tr1$meantime)
pr1te <- predict(lm1, te1, se=T)
SGGP::valstats(pr1te$fit, pr1te$se.fit, te1$meantime)

lm2 <- lm(meantime ~ ., tr1)
summary(lm2)$r.sq
pr2tr <- predict(lm2, tr1, se=T)
SGGP::valstats(pr2tr$fit, pr2tr$se.fit, tr1$meantime)
pr2te <- predict(lm2, te1, se=T)
SGGP::valstats(pr2te$fit, pr2te$se.fit, te1$meantime)


svm1 <- e1071::svm(meantime ~ meanacoustic + sdacoustic + rangeacoustic, data=tr1)
pr.svm1tr <- predict(svm1, tr1, se=T)
SGGP::valstats(pr.svm1tr, rep(1,nrow(tr1)), tr1$meantime)
pr.svm1te <- predict(svm1, te1, se=T)
SGGP::valstats(pr.svm1te, rep(1,nrow(te1)), te1$meantime)
plot(te1$meantime, pr.svm1te)
#  RMSE    score CRPscore  coverage      corr        R2
#1 3.260681 10.63204 2.120923 0.4707146 0.4659226 0.2089533


svm0 <- e1071::svm(meantime ~ ., data=tr1, cost=3.3)
pr.svm0tr <- predict(svm0, tr1, se=T)
SGGP::valstats(pr.svm0tr, rep(1,nrow(tr1)), tr1$meantime)
pr.svm0te <- predict(svm0, te1, se=T)
SGGP::valstats(pr.svm0te, rep(1,nrow(te1)), te1$meantime)
plot(te1$meantime, pr.svm0te);abline(a=0,b=1,col=2)
# RMSE    score CRPscore  coverage      corr        R2
# 3.066802 9.405273 1.959223 0.5071622 0.5466267 0.2898688
# After normalizing it's the same
# Linear kernel is bad (radial is default)
# Polynomial is terrible, as is sigmoid
# Check cost with radial
# cost=.01 3.246116 10.53727 2.118972 0.469123  0.4724401 0.2107227
# cost=.1  3.141428 9.868571 2.019545 0.4952252 0.5224083 0.2608105
# cost=.33 3.102509 9.625562 1.985088 0.5033424 0.5390941 0.2790128
# Cost=3.3 3.073417 9.445889 1.956679 0.5103454 0.5513384 0.2924708
# Use other gamma


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
  # xgboost::xgb.DMatrix(cbind(x$meanacoustic, x$sdacoustic, x$rangeacoustic, x$sdacoustic/x$rangeacoustic + x$meanacoustic/x$sdacoustic), label=x$meantime)
  xgboost::xgb.DMatrix(as.matrix(x[,-1]), label=x$meantime)
}
xgb1 <- xgboost::xgb.train(data=getDmatrix1(tr1), nrounds = 3000)
pr.xgb1tr <- predict(xgb1, getDmatrix1(tr1), se=T)
SGGP::valstats(pr.xgb1tr, rep(1,nrow(tr1)), tr1$meantime)
pr.xgb1te <- predict(xgb1, getDmatrix1(te1), se=T)
SGGP::valstats(pr.xgb1te, rep(1,nrow(te1)), te1$meantime)
plot(te1$meantime, pr.xgb1te);abline(a=0,b=1,col=2)


rf1 <- randomForest::randomForest(meantime ~ ., data=tr1)
pr.rf1tr <- predict(rf1, tr1, se=T)
SGGP::valstats(pr.rf1tr, rep(1,nrow(tr1)), tr1$meantime)
pr.rf1te <- predict(rf1, te1, se=T)
SGGP::valstats(pr.rf1te, rep(1,nrow(te1)), te1$meantime)
plot(te1$meantime, pr.rf1te);abline(a=0,b=1,col=2)
# 3.068152 9.413554 1.995902 0.478036 0.543456 0.2948929
# Combine models, get minor improvement
SGGP::valstats((pr.rf1te+pr.svm0te)/2, rep(1,nrow(te1)), te1$meantime)
# 3.026291 9.158437 1.952196 0.4882222 0.5722433 0.3145773

# Look at worst predictions. Go back for ideas on new features
te1[te1$meantime<1 & pr.rf1te > 11,]



# Make predictions for submission
# Sub5-svm was made with svm0, all features from file
if (F) {
  testfolderpath <- paste0(folderpath, "test/")
  testsubmissionpath <- paste0(folderpath,"Sub6-svmrf.csv")
  if (file.exists(testsubmissionpath)) {stop("File already exists")}
  cat(file=testsubmissionpath, "seg_id,time_to_failure\n")
  i <- 0
  for (testfile in list.files(testfolderpath)) {
    i <- i+1; #cat(i, testfile, "\n")
    testdf <- read.csv(paste0(testfolderpath, testfile))
    names(testdf) <- "V1"
    
    test.absz <- abs(testdf$V1 - mean(testdf$V1)) / sd(testdf$V1)
    # N <- nrow(testdf)
    testdf.features <- data.frame(meanacoustic=mean(testdf$V1),
                                  minacoustic=min(testdf$V1),
                                  maxacoustic=max(testdf$V1),
                                  sdacoustic=sd(testdf$V1),
                                  q25=quantile(test.absz, .25),
                                  q50=quantile(test.absz, .5),
                                  q75=quantile(test.absz, .75),
                                  q90=quantile(test.absz, .90),
                                  q95=quantile(test.absz, .95),
                                  q99=quantile(test.absz, .99),
                                  q999=quantile(test.absz, .999),
                                  q1=quantile(test.absz, 1),
                                  p1=sum(test.absz>1)/nrow(testdf),
                                  p3=sum(test.absz>3)/nrow(testdf),
                                  p10=sum(test.absz>10)/nrow(testdf),
                                  rangeacoustic=max(testdf$V1)-min(testdf$V1)
    )
    # Normalize if needed
    if (TRUE) {
      if (any(colnames(testdf.features) != colnames(gdf2.norm)[-1])) {stop("Names don't match")}
      testdf.features <- sweep(sweep(testdf.features,2,gdf2.colmeans[-1]), 2, gdf2.colsd[-1], `/`)
    }
    testpred.rf1 <- predict(rf1, testdf.features)
    testpred.svm0 <- predict(svm0, testdf.features)
    # Some preds are negative, make them zero. Highest is 16.1
    testpred.svm0 <- pmax( 0.05, testpred.svm0)
    testpred.svm0 <- pmin(16, testpred.svm0)
    testpred <- (testpred.svm0 + testpred.rf1) / 2
    testfile_nocsv <- strsplit(testfile, ".csv")[[1]]
    cat(i, testfile, round(c(testpred.svm0, testpred.rf1, testpred),2), "\n")
    cat(paste0(testfile_nocsv, ",", (testpred), "\n"), file=testsubmissionpath, sep = "",append=TRUE)
    # stop()
  }
}