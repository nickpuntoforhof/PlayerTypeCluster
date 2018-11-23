
### Packages Used ##############################################################
library(Lahman)
library(dplyr)
################################################################################


summary <- Batting %>% group_by(halfdecade=cut(yearID, breaks= seq(1870, 2018, by = 5))) %>% 
  summarize(mean(AB),median(AB),ibb_na_count = sum(is.na(HBP)))

summary <- Batting %>% group_by(yearID) %>% 
  summarize(mean(AB),median(AB),HBP_na_pct = sum(is.na(HBP))/n(),
            SH_na_pct = sum(is.na(SH))/n(),
            GIDP_na_pct = sum(is.na(GIDP))/n(),
            SF_na_pct = sum(is.na(SF))/n(),
            CS_na_pct = sum(is.na(CS))/n(),
            IBB_na_pc = sum(is.na(IBB))/n())

data(Batting)
data(Master)
master <- Master[c(1,14,15)]
batting <- subset(Batting, yearID >= 1955)

batting <- batting %>% group_by(playerID, yearID) %>% summarize(G = sum(G),
                                                                AB = sum(AB),
                                                                R = sum(R),
                                                                H = sum(H),
                                                                X2B = sum(X2B),
                                                                X3B = sum(X3B),
                                                                HR = sum(HR),
                                                                RBI = sum(RBI),
                                                                SB = sum(SB),
                                                                CS = sum(CS),
                                                                BB = sum(BB),
                                                                SO = sum(SO),
                                                                IBB = sum(IBB),
                                                                HBP = sum(HBP),
                                                                SH = sum(SH),
                                                                SF = sum(SF),
                                                                GIDP = sum(GIDP))

batting <- batting %>% mutate(PA = AB + BB + HBP + SH + SF + GIDP,
                              X1B = H - X2B - X3B - HR,
                              BB = ifelse(BB > 0, BB , BB + 1),
                              AVG = H/AB,
                              SLG = (X1B + 2*X2B + 3*X3B + 4*HR)/AB,
                              OBP = (H + BB + HBP) / (AB + BB + HBP + SF),
                              OPS = AVG + SLG,
                              SB_PCT = ifelse((SB + CS) > 0 ,SB / (SB + CS), 0),
                              SB_FREQ = SB / (H + BB + HBP),
                              SO_PCT = SO/PA,
                              HR_PCT = HR/PA,
                              HR_H = HR/H,
                              RBI_H = RBI/H,
                              SO_BB = SO/BB,
                              X2B_PCT = X2B/PA,
                              X3B_PCT = X3B/PA,
                              SH_PCT = SH/PA,
                              SF_PCT = SF/PA,
                              GIDP_PCT = GIDP/PA)

batting <- subset(batting, PA >=200)
master$name <- paste(master$nameFirst, master$nameLast, sep = " ")
batting <- merge(master,batting,  all.y = TRUE)
batting$name <- paste(batting$nameFirst, batting$nameLast, sep = "")

ktest <- kmeans(batting[24:39],centers = 12)

ktest$centers

