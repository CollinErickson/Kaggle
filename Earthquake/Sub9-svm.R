# Using data from FindGroups4.R, GroupInfo4.csv
# Data was made from groups of size 4096.
# Will split these 150k testing groups into
# smaller grouops then average prediction


folderpath <- "./Earthquake/"

filemap <- apply(expand.grid(letters, letters[1:3])[,c(2,1)], 1, function(x) paste0(x,collapse=''))[1:63]
groupinfofilepath <- paste0(folderpath,"GroupInfo4.csv")
gdf <- read.csv(groupinfofilepath, header = F)
gdf %>% head
colnames(gdf) <- c("i.file", "startrow", "endrow",
                   "meantime","mintime","maxtime",
                   "meanacoustic","minacoustic","maxacoustic","sdacoustic",
                   "xq001","xq01","xq05","xq25", "xq50","xq75", "xq90", "xq95","xq99","xq999",
                   "q25", "q50","q75", "q90", "q95","q99","q999","q1",
                   "p1","p3","p6",
                   "m1","m2","m3","m4","m5","m6",
                   "sd1","sd2","sd3","sd4","sd5","sd6",
                   "sd_of_m1","sd_of_m2","sd_of_sd1", "sd_of_sd2")
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

# Now need to break up by top and bottom since they overlap.
# tr.inds <- sample(1:nrow(gdf2), floor(.7*nrow(gdf2)))
tr.inds <- 1:floor(.7*nrow(gdf2))#sample(1:nrow(gdf2), floor(.7*nrow(gdf2)))
# tr1 <- gdf2[tr.inds,]
# te1 <- gdf2[-tr.inds,]
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
#  2.895254 8.382498 1.813076 0.5560032 0.5926002 0.3471446


svm0 <- e1071::svm(meantime ~ ., data=tr1, cost=.1)
pr.svm0tr <- predict(svm0, tr1, se=T)
SGGP::valstats(pr.svm0tr, rep(1,nrow(tr1)), tr1$meantime)
pr.svm0te <- predict(svm0, te1, se=T)
SGGP::valstats(pr.svm0te, rep(1,nrow(te1)), te1$meantime)
plot(te1$meantime, pr.svm0te);abline(a=0,b=1,col=2)
# cost=.0033 3.147921 9.90941 2.063177 0.4826753 0.568513 0.2282238
# cost=.033 2.825818 7.985248 1.741812 0.5898469 0.6155309 0.3780836
# cost=.1 2.786146 7.762609 1.716849 0.5898469 0.631979 0.3954235
# cost=.33 2.836184 8.043939  1.77001 0.548751 0.6323494 0.3735125
# 2.98438 8.906527 1.890176 0.5132957 0.6200233 0.3063315
# cost=10 3.249401 10.55861 2.088079 0.4899275 0.5798192 0.1776622

# Refit svm with all data using best parameters
svm0 <- e1071::svm(meantime ~ ., data=gdf2.norm, cost=.1)


getDmatrix1 <- function(x) {
  xgboost::xgb.DMatrix(as.matrix(x[,-1]), label=x$meantime)
}
xgb1 <- xgboost::xgb.train(data=getDmatrix1(tr1), nrounds = 1000)
pr.xgb1tr <- predict(xgb1, getDmatrix1(tr1), se=T)
SGGP::valstats(pr.xgb1tr, rep(1,nrow(tr1)), tr1$meantime)
pr.xgb1te <- predict(xgb1, getDmatrix1(te1), se=T)
SGGP::valstats(pr.xgb1te, rep(1,nrow(te1)), te1$meantime)
plot(te1$meantime, pr.xgb1te);abline(a=0,b=1,col=2)
# nrounds=100  3.116251 9.71102 1.987122 0.4979855 0.5789453 0.2436751
# nrounds=300  3.120151 9.735343 1.990623 0.5012087 0.5783889 0.2417807
# nrounds=1000 3.120184 9.735545  1.99066 0.5004029 0.5783675 0.2417649

rf1 <- randomForest::randomForest(meantime ~ ., data=tr1)
pr.rf1tr <- predict(rf1, tr1, se=T)
SGGP::valstats(pr.rf1tr, rep(1,nrow(tr1)), tr1$meantime)
pr.rf1te <- predict(rf1, te1, se=T)
SGGP::valstats(pr.rf1te, rep(1,nrow(te1)), te1$meantime)
plot(te1$meantime, pr.rf1te);abline(a=0,b=1,col=2)
# 2.86537 8.210343 1.822476 0.5358582 0.6304441 0.3605525

# Combine models, get minor improvement
plot(pr.rf1te, pr.svm0te)
SGGP::valstats((pr.rf1te+pr.svm0te)/2, rep(1,nrow(te1)), te1$meantime)
# 2.816308 7.931592 1.771666 0.5439162 0.6383652 0.3822625

# Look at worst predictions. Go back for ideas on new features
te1[te1$meantime<1 & pr.rf1te > 11,]






# Do predictions again with this
# Make predictions for submission
# Sub5-svm was made with svm0, all features from file
if (F) {
  testfolderpath <- paste0(folderpath, "test/")
  testsubmissionpath <- paste0(folderpath,"Sub9-svm150k.csv")
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
    # allpreds.rf  <- rep(0,npreds)
    # for (iii in 1:npreds) {
    #   testdfi <- testdf[1:4096 + 4096/ddd*(iii-1),, drop=F]
    #   test.abszi <- abs(testdfi$V1 - mean(testdfi$V1)) / sd(testdfi$V1)
    #   testdf.featuresi <- data.frame(meanacoustic=mean(testdfi$V1),
    #                                  minacoustic=min(testdfi$V1),
    #                                  maxacoustic=max(testdfi$V1),
    #                                  sdacoustic=sd(testdfi$V1),
    #                                  q25=quantile(test.abszi, .25),
    #                                  q50=quantile(test.abszi, .5),
    #                                  q75=quantile(test.abszi, .75),
    #                                  q90=quantile(test.abszi, .90),
    #                                  q95=quantile(test.abszi, .95),
    #                                  q99=quantile(test.abszi, .99),
    #                                  q999=quantile(test.abszi, .999),
    #                                  q1=quantile(test.abszi, 1),
    #                                  p1=sum(test.abszi>1)/nrow(testdfi),
    #                                  p3=sum(test.abszi>3)/nrow(testdfi),
    #                                  p10=sum(test.abszi>10)/nrow(testdfi),
    #                                  rangeacoustic=max(testdfi$V1)-min(testdfi$V1),
    #                                  sd_of_sd=sd(sapply(split(testdfi$V1[1:4096], rep(1:40, each=128)[1:4096]), sd))
    #   )
    #   
    #   testdf.featuresi <- extract_chunk_acoustic_features(testdfi$V1)
    #   names(testdf.featuresi) <- names(gdf2.norm)
    #   
    #   # Normalize if needed
    #   if (TRUE) {
    #     if (any(colnames(testdf.featuresi) != colnames(gdf2.norm)[-1])) {stop("Names don't match")}
    #     testdf.featuresi <- sweep(sweep(testdf.featuresi,2,gdf2.colmeans[-1]), 2, gdf2.colsd[-1], `/`)
    #   }
    #   prediii.svm <- predict(svm0, testdf.featuresi)
    #   prediii.svm <- pmax( 0.05, prediii.svm)
    #   prediii.svm <- pmin(16, prediii.svm)
    #   allpreds.svm[iii] <- prediii.svm
    #   # prediii.rf <- predict(rf1, testdf.featuresi)
    #   # prediii.rf <- pmax( 0.05, prediii.rf)
    #   # prediii.rf <- pmin(16, prediii.rf)
    #   # allpreds.rf[iii] <- prediii.rf
    # }
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
    testdf.features <- extract(testdf$V1)
    names(testdf.features) <- names(gdf2.norm)
    # Normalize if needed
    if (TRUE) {
      if (any(colnames(testdf.features) != colnames(gdf2.norm)[-1])) {stop("Names don't match")}
      testdf.features <- sweep(sweep(testdf.features,2,gdf2.colmeans[-1]), 2, gdf2.colsd[-1], `/`)
    }
    testpred.svm0 <- predict(svm0, testdf.features)
    testfile_nocsv <- strsplit(testfile, ".csv")[[1]]
    # cat(i, testfile, round(c(mean(allpreds.svm),sd(allpreds.svm)),2), "\n")
    cat(i, testfile, round(testpred.svm0,2), "\n", sep="\t")
    cat(paste0(testfile_nocsv, ",", (testpred.svm0), "\n"), file=testsubmissionpath, sep = "",append=TRUE)
    # stop()
  }
}

