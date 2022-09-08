library(tidyverse)

nba_shooting <- read_csv("data/nba_shooting.csv")
nba_shooting <- mutate(nba_shooting, FGP = FGM/FGA, TTP = TPM/TPA, FTP = FTM/FTA)

nba_shooting <- mutate(nba_shooting, eFGP = ((FGM + 0.5 * TPM) / FGA))
nba_shooting <- mutate(nba_shooting, PTS = (FTM +2 * FGM + TPM))
nba_shooting <- mutate(nba_shooting, TSP = PTS / (2 * (FGA + 0.44 * FTA)))
nba_shooting <- arrange(nba_shooting, desc(TSP))