library(tidyverse)
nba_shooting_small <- read_csv(file="data/nba_shooting_small.csv")
nba_shooting_small <- mutate(nba_shooting_small, FGP = FGM/FGA,
                             TTP = TPM/TPA,
                             FTP = FTM/FTA)

