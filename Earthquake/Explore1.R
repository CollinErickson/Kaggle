folderpath <- "./Earthquake/"
library(magrittr)

sampsub <- read.csv(paste0(folderpath, "sample_submission.csv"))
sampsub %>% str
sampsub %>% head

train1 <- read.csv(paste0(folderpath, "train.csv"), nrows = 10000)
train1 %>% str
train1 %>% head
plot(train1)
train1 %>% summary
train1$time_to_failure %>% unique %>% sort
(train1$time_to_failure - mean(train1$time_to_failure)) %>% sort
