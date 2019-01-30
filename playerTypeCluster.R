
### Packages Used ##############################################################
library(Lahman)
library(dplyr)
library(ggplot2)
library(NbClust)
library(DMwR)

################################################################################
### Set Working Directory ######################################################

setwd("~/GithubRepositories/PlayerTypeCluster")

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
#data(Batting)
Batting <- read.csv("batting.csv", stringsAsFactors = FALSE)
#data(Master)
Master <- read.csv("master.csv", stringsAsFactors = FALSE)

### get columns of master needed for the analysis
master <- Master[c(1,2,14,15)]

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
                              OBP = (H + BB + HBP) / (AB + BB + HBP + SF),
                              SLG = (X1B + 2*X2B + 3*X3B + 4*HR)/AB,
                              OPS = OBP + SLG,
                              IS0 = SLG - AVG,
                              X2B_PCT = X2B/PA,
                              X3B_PCT = X3B/PA,
                              HR_PCT = HR/PA,
                              HR_H = HR/H,
                              RBI_PCT = RBI/PA,
                              RBI_H = RBI/H,
                              R_PA = R/PA,
                              R_OB = R / (H + BB + HBP),
                              SO_BB = SO/BB,
                              SO_PCT = SO/PA,
                              SB_PCT = ifelse((SB + CS) > 0 ,SB / (SB + CS), 0),
                              SB_FREQ = SB / (H + BB + HBP),
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
batting_std <- scale(batting[26:45])

### plot the within sum of squares for possible clusterings from k=2 to k=50
wss <- (nrow(batting_std)-1)*sum(apply(batting_std,2,var))
for (i in 2:50) wss[i] <- sum(kmeans(batting_std, 
                                     centers = i, iter.max = 30)$withinss)
plot(1:50, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

### from the straight exponential decay of the plot, there are no real
### jumps, and no leveling off in the plot.  A good choice of cluster
### then, is very subjective.  I choose 12, as it is a convenient number for
### analysis, but not too small as to lead to a very high sum o squares

### set seed
set.seed(12)

### run k-means w ith k = 12 on the standardized data
k12_clust <- kmeans(batting_std, centers = 12, iter.max = 30)

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


### Plot Each Cluster's % of Players in League by Half Decade

ggplot(by_decade_sum,aes(x=halfdecade, y = dr)) + 
  labs(x="Half Decade",y="% of League") +
  ggtitle("% of League in Each Cluster, by Half Decade") +
  facet_wrap(~cluster_assn) +
  geom_bar(stat = 'identity', fill ='#356598') +
  theme_light()

### Plot subsets of the previous plot
ggplot(subset(by_decade_sum, cluster_assn %in% c(8,10,11)),aes(x=halfdecade, y = dr)) + 
  labs(x="Half Decade",y="% of League") +
  ggtitle("% of League in Each Cluster, by Half Decade") +
  facet_wrap(~cluster_assn) +
  geom_bar(stat = 'identity', fill ='#356598') +
  theme_light()

ggplot(subset(by_decade_sum, cluster_assn %in% c(5,6,12)),aes(x=halfdecade, y = dr)) + 
  labs(x="Half Decade",y="% of League") +
  ggtitle("% of League in Each Cluster, by Half Decade") +
  facet_wrap(~cluster_assn) +
  geom_bar(stat = 'identity', fill ='#356598') +
  theme_light()

ggplot(subset(by_decade_sum, cluster_assn %in% c(2,3,7)),aes(x=halfdecade, y = dr)) + 
  labs(x="Half Decade",y="% of League") +
  ggtitle("% of League in Each Cluster, by Half Decade") +
  facet_wrap(~cluster_assn) +
  geom_bar(stat = 'identity', fill ='#356598') +
  theme_light()

### Save Each Cluster's Bar Chart Individually
for (i in 1:12){
  
  temp_plot <- ggplot(subset(by_decade_sum, cluster_assn == i),aes(x=halfdecade, y = dr)) + 
    labs(x="Half Decade",y="% of League") +
    ggtitle(paste0("% of League in Cluster ", i)) +
    geom_bar(stat = 'identity', fill ='#356598') +
    theme_light()
  
  ggsave(temp_plot, file=paste0("pct_league_in_cluster_", i,".png"))
  
}

### heatmap of same data
ggplot(by_decade_sum, aes(cluster_assn, halfdecade)) + geom_tile(aes(fill = dr),
                                       colour = "white") + 
  scale_fill_gradient(low = "white",
                      high = "steelblue")


### get top 5 representative batter-seasons in dataframe in 2018 and overall
rep_players_out <- data.frame()
for (i in 1:12) {
rep_players_out <- rbind(rep_players_out,
                         c(clusterid = paste("Cluster ", i)),
                         head(batting %>% filter(cluster_assn == i) %>% arrange(clust_dist), 5),
                  head(batting %>% filter(cluster_assn == i, yearID == 2018) %>% arrange(clust_dist), 5))
}

### write representative and centers data to file
write.csv(rep_players_out, "representative_players_per_cluster.csv",row.names = FALSE)
write.csv(unscaled_centers, "cluster_center_statistics.csv")


############## code not used for article ########################################
### unscale centers
#unscaled_centers <- data.frame(unscale(k12_clust$centers, batting_std))


### create cluster transition networks

#batting$age <- batting$yearID - batting$birthYear

#batting <- batting %>% arrange(playerID, yearID) %>% group_by(playerID) %>% mutate(next_ops = lead(OPS),
#                                                                                   prev_ops = lag(OPS),
#                                                                                   next_clust = lead(cluster_assn),
#                                                                                   prev_clust = lag(cluster_assn))

#batting %>% filter(age <= 30) %>% 
#  mutate(ops_pct_delta = (next_ops - prev_ops) / prev_ops) %>% 
#  na.omit() %>%
#  group_by(cluster_assn) %>% 
#  summarize(mean(ops_pct_delta), var(ops_pct_delta))

#batting <- batting %>% filter(age <= 30) %>% 
#  mutate(ops_pct_delta = (next_ops - prev_ops) / prev_ops)

#m <- subset(batting, cluster_assn %in% c(2, 12)) %>% na.omit()
#ggplot(m, aes(x=ops_pct_delta, fill=factor(cluster_assn))) + geom_histogram(alpha = 0.5)