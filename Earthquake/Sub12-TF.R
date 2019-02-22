library(tensorflow)
# sess <- tf$InteractiveSession()
x <- tf$placeholder(tf$float32, shape(NULL, as.integer(ncol(tr1)-1)))
y_ <- tf$placeholder(tf$float32, shape(NULL, 1L))
dense1 <- tf$layers$dense(x, 10, activation = tf$nn$selu)
dense2 <- tf$layers$dense(dense1, 5, activation = tf$nn$selu) # tf$sigmoid
dense3 <- tf$layers$dense(dense2, 3, activation = tf$nn$selu)
dense4 <- tf$layers$dense(dense3, 3, activation = tf$nn$selu)
Out <- tf$layers$dense(dense4, 1)
mse <- tf$reduce_mean(tf$square(y_ - Out))
mae <- tf$reduce_mean(tf$abs(y_ - Out))
train_step <- tf$train$AdamOptimizer(1e-3)$minimize(mae) #mse)
sess <-  tf$Session()
sess$run(tf$global_variables_initializer())

N <- 10000
mses <- numeric(N)
for (i in 1:N) {
  tr.rows <- sample(1:nrow(tr1), 128) #2895 +0*128*4*2*4)
  batchx <- as.matrix(tr1[tr.rows,-1]) + 
  batchy <- as.matrix(tr1[tr.rows,1, drop=F])
  if (i %% 50 == 0) {
    train_mse <- mse$eval(feed_dict = dict(
      x = batchx, y_=batchy), session = sess)
    train_mae <- mae$eval(feed_dict = dict(
      x = batchx, y_=batchy), session = sess)
    mses[i] <- train_mse
    cat(sprintf("step %d, training MSE %g, MAE %g\n", i, train_mse, train_mae))
  }
  train_step$run(feed_dict = dict(x = batchx, y_=batchy), session = sess)
}
pr.tf1tr <- Out$eval(feed_dict = dict(x=as.matrix(tr1[,-1])), session=sess)
SGGP::valstats(pr.tf1tr, rep(1,nrow(tr1)), tr1$mintime, metrics=list(MAE=function(a,b,c) {mean(abs(a-c))}))
pr.tf1te <- Out$eval(feed_dict = dict(x=as.matrix(te1[,-1])), session=sess)
SGGP::valstats(pr.tf1te, rep(1,nrow(te1)), te1$mintime, metrics=list(MAE=function(a,b,c) {mean(abs(a-c))}))
plot(tr1$mintime, pr.tf1tr);abline(a=0,b=1,col=2)
plot(te1$mintime, pr.tf1te);abline(a=0,b=1,col=2)




















# Do predictions again with this
# Make predictions for submission
if (F) {
  testfolderpath <- paste0(folderpath, "test/")
  testsubmissionpath <- paste0(folderpath,"Sub12-TFsvm.csv")
  if (file.exists(testsubmissionpath)) {stop("File already exists")}
  cat(file=testsubmissionpath, "seg_id,time_to_failure\n")
  # testfile <- list.files(testfolderpath)[1]
  i <- 0
  for (testfile in list.files(testfolderpath)) {
    i <- i+1; #cat(i, testfile, "\n")
    testdf <- read.csv(paste0(testfolderpath, testfile))
    names(testdf) <- "V1"
    
    testdf.features <- extract_chunk_acoustic_features(testdf$V1)
    names(testdf.features) <- names(gdf2.norm[c(-1,-ncol(gdf2.norm))])
    testdf.features['rangeacoustic'] <- testdf.features[4] - testdf.features[3]
    testdf.features <- as.data.frame(t(testdf.features))
    # Normalize if needed
    if (TRUE) {
      if (is.null(colnames(testdf.features)) || any(colnames(testdf.features) != colnames(gdf2.norm)[-1])) {stop("Names don't match")}
      testdf.features <- sweep(sweep(testdf.features,2,gdf2.colmeans[-1]), 2, gdf2.colsd[-1], `/`)
    }
    testpred.svm0 <- predict(svm0all, testdf.features)
    testpred.TF1 <- Out$eval(feed_dict = dict(x=as.matrix(testdf.features)), session=sess)
    # testpred.rf1 <- predict(rf1all, testdf.features)
    testfile_nocsv <- strsplit(testfile, ".csv")[[1]]
    # cat(i, testfile, round(c(mean(allpreds.svm),sd(allpreds.svm)),2), "\n")
    testpred.final <- (testpred.svm0 + testpred.TF1) / 2 #(testpred.svm0 + testpred.rf1) / 2
    cat(i, testfile, round(c(testpred.svm0, testpred.TF1,  testpred.final),2), "\n", sep="\t")
    cat(paste0(testfile_nocsv, ",", (testpred.final), "\n"), file=testsubmissionpath, sep = "",append=TRUE)
    # stop()
  }
}

