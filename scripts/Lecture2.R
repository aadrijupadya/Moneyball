library(tidyverse)
# Read in data

raw_shooting <- read_csv(file = "/Users/aadrijupadya/Desktop/Moneyball/data/nba_shooting.csv")

raw_shooting <- mutate(raw_shooting,
         FGP = FGM / FGA,
         TPP = TPM / TPA,
         FTP = FTM / FTA,
         eFGP = (FGM + 0.5 * TPM) / (FGA),
         PTS = FTM + 2 * FGM + TPM,
         TSP = PTS/(2 * (FGA + 0.44 * FTA)))

# Sort by the TSP in descending order
raw_shooting <- arrange(raw_shooting, desc(TSP))

# ggplot(data = raw_shooting) + 
#   geom_histogram(mapping = aes(x = FGP), bins = 5)
# 
# 
# ggplot(data = raw_shooting) +
#   geom_histogram(mapping = aes(x = FGP), bins = 50)
# 
# ggplot(data = raw_shooting) +
#   geom_histogram(mapping = aes(x = FTP, y = ..density..), bins = 50)
# ggplot(data = raw_shooting) +
#   geom_histogram(mapping = aes(x = TPP, y = ..density..), bins = 50)
# ggplot(data = raw_shooting) +
#   geom_point(mapping = aes(x = FGP, y = FTP))
# 
# ggplot(data = raw_shooting) +
#   geom_point(mapping = aes(x = FGP, y = FTP), alpha = 0.1)
# ggplot(data = raw_shooting) +
#   geom_bin2d(aes(x = FGP, y = FTP))
# ggplot(data = raw_shooting) + 
#   geom_bin2d(mapping = aes(x = FGP, y = FTP), bins = 100)
# ggplot(data = raw_shooting) +
#   geom_point(mapping = aes(x = FGA, y = FGP), alpha = 0.1)
# ggplot(data = raw_shooting) +
#   geom_point(aes(x = FGA, y = FGP)) + 
#   xlim(0, 200)
# filter(raw_shooting, (FGA >= 100 & TPA >= 50) | (FGP >= 0.45 & FGP <= 0.5))
# nba_shooting <- 
#   filter(raw_shooting, 
#          (FGA >= 100 & 
#             FTA >= 100 & 
#             TPA >= 50 & 
#             !SEASON %in% c(1999, 2012)))
# nba_shooting
nba_shooting <- mutate(nba_shooting,
                       Classification = case_when(
                         TPP < 0.2 ~  "Hopeless",
                         0.2 <= TPP & TPP < 0.3 ~ "Below Average",
                         0.3 <= TPP & TPP < 0.35 ~  "Average",
                         0.35 <= TPP & TPP < 0.4 ~ "Above Average",
                         0.4 <= TPP ~ "Elite"))
nba_shooting_2016 <- filter(nba_shooting, SEASON == 2016)
summarize(nba_shooting_2016, FGP = mean(FGP))
summarize(nba_shooting_2016, FGP = mean(FGP), TPP = mean(TPP), FTP = mean(FTP))
select(nba_shooting, PLAYER, SEASON, FGP, TPP, FTP, eFGP, PTS, TSP)
save(nba_shooting, file = "/Users/aadrijupadya/Desktop/Moneyball/data/nba_shooting.RData")
