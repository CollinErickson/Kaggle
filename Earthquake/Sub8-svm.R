# Using data from FindGroups3.R, GroupInfo3.csv
# Data was made from groups of size 4096.
# Will split these 150k testing groups into
# smaller grouops then average prediction


folderpath <- "./Earthquake/"

filemap <- apply(expand.grid(letters, letters[1:3])[,c(2,1)], 1, function(x) paste0(x,collapse=''))[1:63]
groupinfofilepath <- paste0(folderpath,"GroupInfo3.csv")
gdf <- read.csv(groupinfofilepath, header = F)
gdf %>% head
colnames(gdf) <- c("groupnum", "i.file", "n", "startrow", "endrow",
                   "meantime","mintime","maxtime",
                   "meanacoustic","minacoustic","maxacoustic","sdacoustic",
                   "q25", "q50","q75", "q90", "q95","q99","q999","q1",
                   "p1","p3","p10",
                   "sd_of_sd")
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
gdf2 <- gdf[!(gdf$maxacoustic %>% is.infinite()) & gdf$n>4000, -c(1,2,3,4,5,7,8)]
gdf2$rangeacoustic <- gdf2$maxacoustic - gdf2$minacoustic

summary(lm(meantime ~ meanacoustic + sdacoustic, gdf2))$r.sq
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
tr1 <- gdf2[tr.inds,]
te1 <- gdf2[-tr.inds,]
# tr1 <- gdf2.norm[tr.inds,]
# te1 <- gdf2.norm[-tr.inds,]

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
# 3.601297 377.2254 2.874454 0.07921543 0.1945212 0.03779928

lm2 <- lm(meantime ~ ., tr1)
summary(lm2)$r.sq
pr2tr <- predict(lm2, tr1, se=T)
SGGP::valstats(pr2tr$fit, pr2tr$se.fit, tr1$meantime)
pr2te <- predict(lm2, te1, se=T)
SGGP::valstats(pr2te$fit, pr2te$se.fit, te1$meantime)
# 3.476074 156.6094 2.693818 0.1301474 0.3224003 0.1035505

svm1 <- e1071::svm(meantime ~ meanacoustic + sdacoustic + rangeacoustic, data=tr1)
pr.svm1tr <- predict(svm1, tr1, se=T)
SGGP::valstats(pr.svm1tr, rep(1,nrow(tr1)), tr1$meantime)
pr.svm1te <- predict(svm1, te1, se=T)
SGGP::valstats(pr.svm1te, rep(1,nrow(te1)), te1$meantime)
plot(te1$meantime, pr.svm1te)
#  RMSE    score CRPscore  coverage      corr        R2
#  3.370359 11.35932 2.277363 0.4262029 0.4008426 0.1572477


svm0 <- e1071::svm(meantime ~ ., data=tr1, cost=1)
pr.svm0tr <- predict(svm0, tr1, se=T)
SGGP::valstats(pr.svm0tr, rep(1,nrow(tr1)), tr1$meantime)
pr.svm0te <- predict(svm0, te1, se=T)
SGGP::valstats(pr.svm0te, rep(1,nrow(te1)), te1$meantime)
plot(te1$meantime, pr.svm0te);abline(a=0,b=1,col=2)
# 3.289407 10.8202 2.205364 0.4311877 0.4504119 0.1972451


 
# getDmatrix1 <- function(x) {
#   # xgboost::xgb.DMatrix(cbind(x$meanacoustic, x$sdacoustic, x$rangeacoustic, x$sdacoustic/x$rangeacoustic + x$meanacoustic/x$sdacoustic), label=x$meantime)
#   xgboost::xgb.DMatrix(as.matrix(x[,-1]), label=x$meantime)
# }
# xgb1 <- xgboost::xgb.train(data=getDmatrix1(tr1), nrounds = 3000)
# pr.xgb1tr <- predict(xgb1, getDmatrix1(tr1), se=T)
# SGGP::valstats(pr.xgb1tr, rep(1,nrow(tr1)), tr1$meantime)
# pr.xgb1te <- predict(xgb1, getDmatrix1(te1), se=T)
# SGGP::valstats(pr.xgb1te, rep(1,nrow(te1)), te1$meantime)
# plot(te1$meantime, pr.xgb1te);abline(a=0,b=1,col=2)


rf1 <- randomForest::randomForest(meantime ~ ., data=tr1)
pr.rf1tr <- predict(rf1, tr1, se=T)
SGGP::valstats(pr.rf1tr, rep(1,nrow(tr1)), tr1$meantime)
pr.rf1te <- predict(rf1, te1, se=T)
SGGP::valstats(pr.rf1te, rep(1,nrow(te1)), te1$meantime)
plot(te1$meantime, pr.rf1te);abline(a=0,b=1,col=2)
# 3.25688 10.60726 2.202914 0.4190507 0.4628654 0.2130428

# Combine models, get minor improvement
plot(pr.rf1te, pr.svm0te)
SGGP::valstats((pr.rf1te+pr.svm0te)/2, rep(1,nrow(te1)), te1$meantime)


# Look at worst predictions. Go back for ideas on new features
te1[te1$meantime<1 & pr.rf1te > 11,]






# Do predictions again with this
# Make predictions for submission
# Sub5-svm was made with svm0, all features from file
if (F) {
  testfolderpath <- paste0(folderpath, "test/")
  testsubmissionpath <- paste0(folderpath,"Sub8-svmrf.csv")
  if (file.exists(testsubmissionpath)) {stop("File already exists")}
  cat(file=testsubmissionpath, "seg_id,time_to_failure\n")
  i <- 0
  # testfile <- list.files(testfolderpath)[1]
  for (testfile in list.files(testfolderpath)) {
    i <- i+1; #cat(i, testfile, "\n")
    testdf <- read.csv(paste0(testfolderpath, testfile))
    names(testdf) <- "V1"
    
    # I did it wrong, training size was 15k, testing are 150k.
    # What if I predict ten times on testing?
    
    ddd <- 1
    npreds <- floor(nrow(testdf)/4096-1)*ddd
    allpreds.svm <- rep(0,npreds)
    allpreds.rf  <- rep(0,npreds)
    for (iii in 1:npreds) {
      testdfi <- testdf[1:4096 + 4096/ddd*(iii-1),, drop=F]
      test.abszi <- abs(testdfi$V1 - mean(testdfi$V1)) / sd(testdfi$V1)
      testdf.featuresi <- data.frame(meanacoustic=mean(testdfi$V1),
                                     minacoustic=min(testdfi$V1),
                                     maxacoustic=max(testdfi$V1),
                                     sdacoustic=sd(testdfi$V1),
                                     q25=quantile(test.abszi, .25),
                                     q50=quantile(test.abszi, .5),
                                     q75=quantile(test.abszi, .75),
                                     q90=quantile(test.abszi, .90),
                                     q95=quantile(test.abszi, .95),
                                     q99=quantile(test.abszi, .99),
                                     q999=quantile(test.abszi, .999),
                                     q1=quantile(test.abszi, 1),
                                     p1=sum(test.abszi>1)/nrow(testdfi),
                                     p3=sum(test.abszi>3)/nrow(testdfi),
                                     p10=sum(test.abszi>10)/nrow(testdfi),
                                     rangeacoustic=max(testdfi$V1)-min(testdfi$V1),
                                     sd_of_sd=sd(sapply(split(testdfi$V1[1:4096], rep(1:40, each=128)[1:4096]), sd))
      )
      
      
      # Normalize if needed
      if (FALSE) {
        if (any(colnames(testdf.featuresi) != colnames(gdf2.norm)[-1])) {stop("Names don't match")}
        testdf.featuresi <- sweep(sweep(testdf.featuresi,2,gdf2.colmeans[-1]), 2, gdf2.colsd[-1], `/`)
      }
      prediii.svm <- predict(svm0, testdf.featuresi)
      prediii.svm <- pmax( 0.05, prediii.svm)
      prediii.svm <- pmin(16, prediii.svm)
      allpreds.svm[iii] <- prediii.svm
      # prediii.rf <- predict(rf1, testdf.featuresi)
      # prediii.rf <- pmax( 0.05, prediii.rf)
      # prediii.rf <- pmin(16, prediii.rf)
      # allpreds.rf[iii] <- prediii.rf
    }
    # plot(allpreds.svm);points(allpreds.rf, col=2)
    testpred.svm0 <- mean(allpreds.svm)
    # testpred.rf1  <- mean(allpreds.rf)
    # Some preds are negative, make them zero. Highest is 16.1
    # testpred.svm0 <- pmax( 0.05, testpred.svm0)
    # testpred.svm0 <- pmin(16, testpred.svm0)
    # testpred <- (testpred.svm0 + testpred.rf1) / 2
    
    
    # # Get old prediction using all data to check the difference
    # test.absz <- abs(testdf$V1 - mean(testdf$V1)) / sd(testdf$V1)
    # testdf.features <- data.frame(meanacoustic=mean(testdf$V1),
    #                               minacoustic=min(testdf$V1),
    #                               maxacoustic=max(testdf$V1),
    #                               sdacoustic=sd(testdf$V1),
    #                               q25=quantile(test.absz, .25),
    #                               q50=quantile(test.absz, .5),
    #                               q75=quantile(test.absz, .75),
    #                               q90=quantile(test.absz, .90),
    #                               q95=quantile(test.absz, .95),
    #                               q99=quantile(test.absz, .99),
    #                               q999=quantile(test.absz, .999),
    #                               q1=quantile(test.absz, 1),
    #                               p1=sum(test.absz>1)/nrow(testdf),
    #                               p3=sum(test.absz>3)/nrow(testdf),
    #                               p10=sum(test.absz>10)/nrow(testdf),
    #                               rangeacoustic=max(testdf$V1)-min(testdf$V1)
    # )
    # Normalize if needed
    if (FALSE) {
      if (any(colnames(testdf.features) != colnames(gdf2.norm)[-1])) {stop("Names don't match")}
      testdf.features <- sweep(sweep(testdf.features,2,gdf2.colmeans[-1]), 2, gdf2.colsd[-1], `/`)
    }
    # testpred.svm0BAD <- predict(svm0, testdf.features)
    testfile_nocsv <- strsplit(testfile, ".csv")[[1]]
    cat(i, testfile, round(c(mean(allpreds.svm),sd(allpreds.svm)),2), "\n")
    cat(paste0(testfile_nocsv, ",", (testpred.svm0), "\n"), file=testsubmissionpath, sep = "",append=TRUE)
    # stop()
  }
}

