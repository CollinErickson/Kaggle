library(magrittr); library(dplyr)
dipole_moments <- read.csv('./ScalarCoupling/data/dipole_moments.csv', stringsAsFactors = F)
dipole_moments %>% str
dipole_moments %>% summary
dipole_moments[,2:4] %>% pairs
dipole_moments[,2:4] %>% cor

magnetic <- read.csv('./ScalarCoupling/data/magnetic_shielding_tensors.csv')
magnetic %>% str
