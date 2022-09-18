library(tidyverse)
# relative_payroll <- read_csv(file="/Users/aadrijupadya/Desktop/Moneyball/data/mlb_relative_payrolls.csv")
# ggplot(data = relative_payroll) +
#   geom_histogram(mapping = aes(x = Winning_Percentage, y = ..density..), bins = 50)
# #Histogram mapping distribution and relative density of winning percentage
# ggplot(data = relative_payroll) +
#   geom_histogram(mapping = aes(x = Relative_Payroll, y = ..density..), bins = 50)
# #Histogram mapping distribution and relative density of relative payroll
# ggplot(data = relative_payroll) +
#   geom_point(mapping = aes(x = Relative_Payroll, y = Winning_Percentage), alpha = 0.1)
# #Scatter plot comparing payroll and percentage
# ggplot(data = relative_payroll) + 
#   geom_point(mapping = aes(x = Year, y = Team_Payroll))
# 
# ggplot(data = relative_payroll) + 
#   geom_point(mapping = aes(x = Year, y = Relative_Payroll))

hitting_qualified <- read_csv(file="/Users/aadrijupadya/Desktop/Moneyball/data/hitting_qualified.csv")
# hitting_qualified <- arrange(hitting_qualified,desc(yearID))
# hitting_qualified <- arrange(hitting_qualified,(yearID))
# hitting_qualified <- summarise(hitting_qualified,yearID)
hitting_qualified <- mutate(hitting_qualified,
                            IBB = as.integer(IBB),
                            HBP = as.integer(HBP),
                            SH = as.integer(SH),
                            SF = as.integer(SF),
                            GIDP = as.integer(GIDP)
)
select(hitting_qualified, playerID, yearID, AB, IBB, HBP, SH, SF, GIDP)
hitting_qualified <- replace_na(hitting_qualified, 
                                list(IBB = 0, HBP = 0, SH = 0, SF = 0, GIDP = 0))

hitting_qualified <- mutate(hitting_qualified,
                            X1B = (H-X2B-X3B-HR))


hitting_qualified <- mutate(hitting_qualified,
                            uBB = (BB-IBB))

hitting_qualified <- mutate(hitting_qualified,
                            BBP = (BB/PA),
                            KP = SO/PA,
                            OBP = ((H+BB+HBP)/(AB+BB+HBP+SF)),
                            SLG = ((X1B + 2 * X2B + 3 * X3B + 4 * HR)/(AB)),
                            OPS = OBP+SLG,
                            wOBA = ((0.687*uBB + 0.718 * HBP + 0.881 * X1B + 1.256 * X2B + 1.594 * X3B + 2.065 * HR)/(AB+uBB+SF+HBP))
                                   )
hitting_qualified <- mutate(hitting_qualified, 
                            BBP_rating = case_when(BBP >= .15 ~ "Excellent",
                                                   BBP < .15 & BBP >= .125 ~ "Great",
                                                   ),
                            OBP_rating = case_when(BBP >= 0.390 ~ "Excellent",
                                                   BBP < 0.390 & BBP >= 0.370 ~ "Great",
                                                   BBP < 0.370 & BBP >= 0.340 ~ "Above Average",
                                                   BBP < 0.340 & BBP >= 0.320 ~ "Average",
                                                   BBP < 0.320 & BBP >= 0.310 ~ "Below Average",
                                                   BBP < 0.310 & BBP >= 0.300 ~ "Poor",
                                                   BBP < 0.300 & BBP >= 0.290 ~ "Awful"
                            )
                            
                            )
tmp_batting <- filter(hitting_qualified, (2000 <= yearID & yearID >= 2015 )) 
batting_recent <- select(tmp_batting, playerID, yearID, teamID, lgID, BBP, KP, OBP, SLG, OPS, wOBA, BBP_rating, OBP_rating) 

ggplot(data = batting_recent) + 
  geom_point(mapping = aes(x = BBP, y = SLG))
ggplot(data = relative_payroll) +
 geom_histogram(mapping = aes(x = SLG, y = ..density..), bins = 50)
batting_recent