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
