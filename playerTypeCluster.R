
### Packages Used ##############################################################
library(Lahman)
library(dplyr)
library(ggplot2)
library(NbClust)


################################################################################

### this section investigates which data is present for each year in the lahman database
summary <- Batting %>% group_by(halfdecade=cut(yearID, breaks= seq(1870, 2018, by = 5))) %>% 
  summarize(mean(AB),median(AB),ibb_na_count = sum(is.na(HBP)))

summary <- Batting %>% group_by(yearID) %>% 
  summarize(mean(AB),median(AB),HBP_na_pct = sum(is.na(HBP))/n(),
            SH_na_pct = sum(is.na(SH))/n(),
            GIDP_na_pct = sum(is.na(GIDP))/n(),
            SF_na_pct = sum(is.na(SF))/n(),
            CS_na_pct = sum(is.na(CS))/n(),
            IBB_na_pc = sum(is.na(IBB))/n())

### load in Batting and Master from Lahman package
data(Batting)
data(Master)

### get columns of master needed for the analysis
master <- Master[c(1,14,15)]

### getall hitter seasons from 1955 onward
batting <- subset(Batting, yearID >= 1955)

### players that were traded within a year have their data broken up into
### sints.  Sum across all of these stints to create a single observation for each
### batter-year.
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

### drive various batting statistics from the basic lahman counting statistics.
batting <- batting %>% mutate(PA = AB + BB + HBP + SH + SF + GIDP,
                              X1B = H - X2B - X3B - HR,
                              BB = ifelse(BB > 0, BB , BB + 1), ### give a single walk
                              ### to players who never walked in a season, avoiding NAs in the
                              ### calculation of some rate statistics.
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

### keep only batterseasons with 200 or more PAs
batting <- subset(batting, PA >= 200)

### join master and batting tables, to get the full names of each player
master$name <- paste(master$nameFirst, master$nameLast, sep = " ")
batting <- merge(master,batting,  all.y = TRUE)
batting$name <- paste(batting$nameFirst, batting$nameLast, sep = " ")

### selecting only rate statistics, standardize the data.  This
### puts each variable on the same scale, helping the k-means algorithm.
batting_std <- scale(batting[25:40])

### plot the within sum of squares for possible clusterings from k=2 to k=50
wss <- (nrow(batting_std)-1)*sum(apply(batting_std,2,var))
for (i in 2:50) wss[i] <- sum(kmeans(batting_std, 
                                     centers = i, iter.max = 20)$withinss)
plot(1:50, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
### from the straight exponential decay of the plot, there are no real
### jumps, and no leveling off in the plot.  A good choice of cluster
### then, is very subjective.  I choose 12, as it is a convenient number for
### analysis, but not too small as to lead to a very high sum o squares

### set seed
set.seed(12)

### run k-means w ith k = 12 on the standardized data
k12_clust <- kmeans(batting_std, centers = 12, iter.max = 50)

### add the cluster assignments for each batter-season to the batting dataframe
batting$cluster_assn <- k12_clust$cluster

### bucket each batter-season by half decade
batting$halfdecade <- batting$yearID - batting$yearID %% 5

### for each batter-season, calculate the euclidean distance between it and its cluster
vector <- numeric(nrow(batting_std))
for (i in 1:nrow(batting_std)) {
  vector[i] <- sqrt(sum((batting_std[i,] - k12_clust$centers[k12_clust$cluster[i],])^2))
}
### add this value as a column in batting
batting$clust_dist <- vector


by_decade_sum <- batting %>% group_by(cluster_assn) %>% mutate(play_per_clust = n()) %>%
  ungroup() %>% group_by(halfdecade) %>% mutate(play_per_dec = n()) %>% ungroup() %>%
  group_by(halfdecade, cluster_assn) %>% summarize(n = n(), 
                                               ppd = mean(play_per_dec), 
                                               ppc = mean(play_per_clust),
                                               cr = n/ppc,
                                               dr = n/ppd,
                                               or = (n/ppd)/(n/ppc))



ggplot(by_decade_sum,aes(x=halfdecade, y = dr)) + 
  labs(x="Half Decade",y="% of League") +
  ggtitle("% of League in Each Cluster, by Half Decade") +
  facet_wrap(~cluster_assn) +
  geom_bar(stat = 'identity', fill ='#356598') +
  theme_light()

ggplot(by_decade_sum, aes(cluster_assn, halfdecade)) + geom_tile(aes(fill = dr),
                                       colour = "white") + scale_fill_gradient(low = "white",
                                                                             high = "steelblue")

for (i in sort(unique(batting$halfdecade))) {
  print(i)
  print(head((batting %>% filter(cluster_assn == 12, halfdecade == i) %>% arrange(halfdecade,clust_dist,yearID)), 5))
}

cntrs <- k12_clust$centers
