# Hot Hand Fallacy test - do shooters improve after making a shot?

library(readr)
library(ggplot2)
shot_logs <- read_csv("~/Documents/Hot Hand/shot.csv")

# Adding columns
shot_logs$Date <- substring(shot_logs$MATCHUP,0,12)
shot_logs$Team <- substring(shot_logs$MATCHUP,16,18)
shot_logs$Opponent <- substring(shot_logs$MATCHUP,nchar(shot_logs$MATCHUP)-3)
shot_logs$prev_shot_result <- NA
shot_logs$consecutive <- 0
shot_logs$prev_dist <- NA
shot_logs$prev_defender_dist <- NA

players <- unique(shot_logs$playerName)
player_data <- data.frame(name = players) # Deal with player stats
row.names(player_data) <- players # Makes it easier to index by player names

shots <- shot_logs$X1 # Shot IDs corresponding to row number, so I can iterate over all of them
for (shot in shots) {
  shot_num <- shot_logs[shot, "SHOT_NUMBER"]
  shot_logs[shot, "consecutive"] <- 0
  if (shot_num[1,1] == 1) {
    shot_logs[shot,"prev_shot_result"] <- NA # If shot is player's first in the game, no previous shot result exists
  } else {
    shot_logs[shot,"prev_shot_result"] <- shot_logs[shot-1,"SHOT_RESULT"]
    shot_logs[shot,"prev_dist"] <- shot_logs[shot-1,"SHOT_DIST"]
    shot_logs[shot,"prev_defender_dist"] <- shot_logs[shot-1,"CLOSE_DEF_DIST"]
    if (shot_logs[shot,"prev_shot_result"] == "made") {
      shot_logs[shot,"consecutive"] <- shot_logs[shot-1,"consecutive"] + 1 # If shooter made the previous shot, consecutive streak increases by 1
    }                                                                      # Otherwise, consecutive stays at 0
  }
}

shot_logs$difference <- shot_logs$SHOT_DIST - shot_logs$prev_dist

for (player in players) { # Building out player_data
  individual_shots <- shot_logs[shot_logs$playerName==player,]
  player_data[player,"fg_pct"] <- mean(individual_shots$FGM)
  shots_after_miss <- individual_shots[which(individual_shots$prev_shot_result == "missed"),]
  shots_after_made <- individual_shots[which(individual_shots$prev_shot_result == "made"),]
  player_data[player,"fg_pct_miss"] <- mean(shots_after_miss$FGM) # FG% after missing
  player_data[player,"fg_pct_made"] <- mean(shots_after_made$FGM) # FG% after scoring
  player_data[player,"fg_att"] <- length(individual_shots$X1) # Total FG attempts
  player_data[player,"fg_att_miss"] <- length(shots_after_miss$X1) # FG attempts after missing
  player_data[player,"fg_att_made"] <- length(shots_after_made$X1) # FG attempts after scoring
  player_data[player,"gp"] <- length(unique(individual_shots$MATCHUP)) # Total games played
  player_data[player,"shots_per_gp"] <- player_data[player,"fg_att"] / player_data[player,"gp"]
  
  # Consecutive shots
  shots_after_1 <- individual_shots[which(individual_shots$consecutive == 1),]
  shots_after_2 <- individual_shots[which(individual_shots$consecutive == 2),]
  shots_after_3 <- individual_shots[which(individual_shots$consecutive == 3),]
  shots_after_more <- individual_shots[which(individual_shots$consecutive > 3),]
  player_data[player,"fg_pct_1"] <- mean(shots_after_1$FGM)
  player_data[player,"fg_pct_2"] <- mean(shots_after_2$FGM)
  player_data[player,"fg_pct_3"] <- mean(shots_after_3$FGM)
  player_data[player,"fg_pct_more"] <- mean(shots_after_more$FGM)
  
  # Average amount of movement after missing or scoring (positive is away)
  player_data[player,"avg_dist"] <- mean(individual_shots$SHOT_DIST)
  player_data[player,"avg_diff_miss"] <- mean(shots_after_miss$difference)
  player_data[player,"avg_diff_made"] <- mean(shots_after_made$difference)
}

player_data <- player_data[which(player_data$fg_att >= 100),] # Remove anyone who didn't shoot enough to get "hot"

# Help determine who gets hot (or doesn't) and who moves a lot (or not at all)
player_data$fg_difference <- player_data$fg_pct_made - player_data$fg_pct_miss
player_data$dist_difference <- player_data$avg_diff_made - player_data$avg_diff_miss

# League wide FG% numbers, overall, after miss, and after score
overall_fg_pct <- sum(shot_logs$FGM) / length(shot_logs$X1)
overall_fg_scored <- sum(shot_logs$FGM)
all_shots_after_miss <- shot_logs[which(shot_logs$prev_shot_result == "missed"),]
all_shots_after_made <- shot_logs[which(shot_logs$prev_shot_result == "made"),]
fg_pct_miss <- sum(all_shots_after_miss$FGM) / length(all_shots_after_miss$X1)
fg_pct_made <- sum(all_shots_after_made$FGM) / length(all_shots_after_made$X1)

# Average distance moved over entire league after scoring and after missing
overall_diff_miss <- mean(all_shots_after_miss$difference)
overall_diff_made <- mean(all_shots_after_made$difference)

# FG% for consecutive shots (0, 1, 2, 3, 4+)
fg_after_0 <- shot_logs[which(shot_logs$consecutive %in% c(0, NA)),"FGM"]
fg_pct_0 <- mean(fg_after_0$FGM)
fg_after_1 <- shot_logs[which(shot_logs$consecutive == 1),"FGM"]
fg_pct_1 <- mean(fg_after_1$FGM)
fg_after_2 <- shot_logs[which(shot_logs$consecutive == 2),"FGM"]
fg_pct_2 <- mean(fg_after_2$FGM)
fg_after_3 <- shot_logs[which(shot_logs$consecutive == 3),"FGM"]
fg_pct_3 <- mean(fg_after_3$FGM)
fg_after_more <- shot_logs[which(shot_logs$consecutive > 3),"FGM"]
fg_pct_more <- mean(fg_after_more$FGM)

# Player average distances overall, after scoring, after missing
player_data$avg_dist <- round(player_data$avg_dist, digits=1)
player_data$avg_dist_made <- round(player_data$avg_dist + player_data$avg_diff_made, digits=1)
player_data$avg_dist_miss <- round(player_data$avg_dist + player_data$avg_diff_miss, digits=1)

scored_shots <- shot_logs[which(shot_logs$FGM == 1),]

# Distance data for plot #1
distances <- sort(unique(shot_logs$SHOT_DIST))
distance_data <- data.frame(distance = distances)
row.names(distance_data) <- distances
for (dist in distances) {
  shots_from_dist <- shot_logs[which(shot_logs$SHOT_DIST == dist),] # Every shot from dist
  distance_data[toString(dist),"num_shots"] <- length(shots_from_dist$X1)
  distance_data[toString(dist),"fg_pct"] <- mean(shots_from_dist$FGM)
  distance_data[toString(dist),"fg_scored"] <- sum(shots_from_dist$FGM)
}

# Cut out half-court shots - limits range to 0.2-27.7 ft
distance_data <- distance_data[which(distance_data$num_shots >= 100),]

# Do the same for all shots after missing and all shots after scoring
after_miss_distances <- sort(unique(all_shots_after_miss$SHOT_DIST))
after_miss_distance_data <- data.frame(distance = after_miss_distances)
row.names(after_miss_distance_data) <- after_miss_distances
for (dist in after_miss_distances) {
  shots_from_dist <- all_shots_after_miss[which(all_shots_after_miss$SHOT_DIST == dist),]
  after_miss_distance_data[toString(dist),"num_shots"] <- length(shots_from_dist$X1)
  after_miss_distance_data[toString(dist),"fg_pct"] <- mean(shots_from_dist$FGM)
  after_miss_distance_data[toString(dist),"fg_scored"] <- sum(shots_from_dist$FGM)
}

after_miss_distance_data <- after_miss_distance_data[which(after_miss_distance_data$num_shots >= 50),]

after_made_distances <- sort(unique(all_shots_after_made$SHOT_DIST))
after_made_distance_data <- data.frame(distance = after_made_distances)
row.names(after_made_distance_data) <- after_made_distances
for (dist in after_made_distances) {
  shots_from_dist <- all_shots_after_made[which(all_shots_after_made$SHOT_DIST == dist),]
  after_made_distance_data[toString(dist),"num_shots"] <- length(shots_from_dist$X1)
  after_made_distance_data[toString(dist),"fg_pct"] <- mean(shots_from_dist$FGM)
  after_made_distance_data[toString(dist),"fg_scored"] <- sum(shots_from_dist$FGM)
}

after_made_distance_data <- after_made_distance_data[which(after_made_distance_data$num_shots >= 50),]

# East vs West

east_teams <- c("ATL","BOS","BKN","CHA","CHI","CLE","DET","IND","MIA","MIL","NYK","ORL","PHI","TOR","WAS")
west_teams <- c("DAL","DEN","GSW","HOU","LAC","LAL","MEM","MIN","NOP","OKC","PHX","POR","SAC","SAS","UTA")

east_shots <- shot_logs[which(shot_logs$Team %in% east_teams),]
west_shots <- shot_logs[which(shot_logs$Team %in% west_teams),]

east_distances <- sort(unique(east_shots$SHOT_DIST))
east_distance_data <- data.frame(distance = east_distances)
row.names(east_distance_data) <- east_distances
for (dist in east_distances) {
  shots_from_dist <- east_shots[which(east_shots$SHOT_DIST == dist),]
  east_distance_data[toString(dist),"num_shots"] <- length(shots_from_dist$X1)
  east_distance_data[toString(dist),"fg_pct"] <- mean(shots_from_dist$FGM)
  east_distance_data[toString(dist),"fg_scored"] <- sum(shots_from_dist$FGM)
}

west_distances <- sort(unique(west_shots$SHOT_DIST))
west_distance_data <- data.frame(distance = west_distances)
row.names(west_distance_data) <- west_distances
for (dist in west_distances) {
  shots_from_dist <- west_shots[which(west_shots$SHOT_DIST == dist),]
  west_distance_data[toString(dist),"num_shots"] <- length(shots_from_dist$X1)
  west_distance_data[toString(dist),"fg_pct"] <- mean(shots_from_dist$FGM)
  west_distance_data[toString(dist),"fg_scored"] <- sum(shots_from_dist$FGM)
}

east_distance_data <- east_distance_data[which(east_distance_data$num_shots >= 50),]
west_distance_data <- west_distance_data[which(west_distance_data$num_shots >= 50),]

# Consecutive Shots for line plot
consecutive_pct <- c(fg_pct_0,fg_pct_1,fg_pct_2,fg_pct_3,fg_pct_more)
consecutive_data <- data.frame(consecutive_scored <- c("0","1","2","3","4+"),fg_pct <- consecutive_pct)

# Hot Hand players: Hansbrough, Dorsey, Varejao, Lopez, Nurkic (added Len, Mills, Sefolosha, McGary, Durant)
# Not Hot Hand players; Allen, Korver, Ibaka, Harris, Hardaway (added Thomas, Smart, Gordon, Cole, Bledsoe)

# Note: 10 players still not enough to see true trends. Possibly bucket shot results by half or full ft instead of 0.1 ft
# Tried full foot, still doesn't look right. Going by 10 ft (3 categories)
hot_hand_players <- c("Tyler Hansbrough", "Joey Dorsey", "Anderson Varejao", "Robin Lopez", "Jusuf Nurkic",
                      "Alex Len", "Patty Mills", "Thabo Sefolosha", "Mitch McGary", "Kevin Durant")

hot_hand_shots <- shot_logs[which(shot_logs$playerName %in% hot_hand_players),]

hot_hand_distances <- c(5,15,25)
low <- c(0,10,23.8)
high <- c(10,23.8,50)
hot_hand_distance_data <- data.frame(distance = hot_hand_distances, low = low, high = high)
row.names(hot_hand_distance_data) <- hot_hand_distances
for (distance in hot_hand_distances) {
  shots_from_dist <- hot_hand_shots[which(hot_hand_shots$SHOT_DIST >= hot_hand_distance_data[toString(distance),"low"] & hot_hand_shots$SHOT_DIST < hot_hand_distance_data[toString(distance),"high"]),] # Every shot in range  hot_hand_distance_data[toString(distance),"num_shots"] <- length(shots_from_dist$X1)
  shots_after_scoring <- shots_from_dist[which(shots_from_dist$prev_shot_result == "made"),]
  shots_after_missing <- shots_from_dist[which(shots_from_dist$prev_shot_result == "missed"),]
  hot_hand_distance_data[toString(distance),"num_shots"] <- length(shots_from_dist$X1)
  hot_hand_distance_data[toString(distance),"fg_pct"] <- mean(shots_from_dist$FGM)
  hot_hand_distance_data[toString(distance),"fg_scored"] <- sum(shots_from_dist$FGM)
  hot_hand_distance_data[toString(distance),"fg_pct_made"] <- mean(shots_after_scoring$FGM)
  hot_hand_distance_data[toString(distance),"fg_pct_miss"] <- mean(shots_after_missing$FGM)
}

hot_hand_distance_data$category <- factor(c("Short Range", "Mid Range", "3 Point"),levels = c("Short Range", "Mid Range", "3 Point"))

hot_hand_distance_data2 <- data.frame(distance = rep(hot_hand_distance_data$category, times=2), fg_pct = c(hot_hand_distance_data$fg_pct_made, hot_hand_distance_data$fg_pct_miss), type = c(rep("After Scoring", times=3), rep("After Missing", times=3)))

not_hot_hand_players <- c("Tony Allen", "Kyle Korver", "Serge Ibaka", "Devin Harris", "Tim Hardaway Jr.", 
                          "Isaiah Thomas", "Marcus Smart", "Aaron Gordon", "Norris Cole", "Eric Bledsoe")

not_hot_hand_shots <- shot_logs[which(shot_logs$playerName %in% not_hot_hand_players),]

not_hot_hand_distance_data <- data.frame(distance = hot_hand_distances, low = low, high = high)
row.names(not_hot_hand_distance_data) <- hot_hand_distances
for (distance in hot_hand_distances) {
  shots_from_dist <- not_hot_hand_shots[which(not_hot_hand_shots$SHOT_DIST >= not_hot_hand_distance_data[toString(distance),"low"] & not_hot_hand_shots$SHOT_DIST < not_hot_hand_distance_data[toString(distance),"high"]),] # Every shot in range
  shots_after_scoring <- shots_from_dist[which(shots_from_dist$prev_shot_result == "made"),]
  shots_after_missing <- shots_from_dist[which(shots_from_dist$prev_shot_result == "missed"),]
  not_hot_hand_distance_data[toString(distance),"num_shots"] <- length(shots_from_dist$X1)
  not_hot_hand_distance_data[toString(distance),"fg_pct"] <- mean(shots_from_dist$FGM)
  not_hot_hand_distance_data[toString(distance),"fg_scored"] <- sum(shots_from_dist$FGM)
  not_hot_hand_distance_data[toString(distance),"fg_pct_made"] <- mean(shots_after_scoring$FGM)
  not_hot_hand_distance_data[toString(distance),"fg_pct_miss"] <- mean(shots_after_missing$FGM)
}

not_hot_hand_distance_data$category <- factor(c("Short Range", "Mid Range", "3 Point"),levels = c("Short Range", "Mid Range", "3 Point"))

not_hot_hand_distance_data2 <- data.frame(distance = rep(not_hot_hand_distance_data$category, times=2), fg_pct = c(not_hot_hand_distance_data$fg_pct_made, not_hot_hand_distance_data$fg_pct_miss), type = c(rep("After Scoring", times=3), rep("After Missing", times=3)))

overall_hot_hand_distance_data <- data.frame(distance = rep(hot_hand_distance_data$category, times=2), fg_pct = c(hot_hand_distance_data$fg_pct, not_hot_hand_distance_data$fg_pct), type=c(rep("Streaky",times=3),rep("Consistent",times=3)))

# Plots

# 1. All shots, distance on x, FG% on y
ggplot(distance_data, aes(x = distance, y = fg_pct)) + geom_point() + labs(x = "Shot Distance", y = "FG%") + geom_vline(xintercept=23.8) + geom_vline(xintercept = 10) + ggtitle("League-Wide FG% by Distance")

# 1a. All shots after scoring, distance on x, FG% on y
ggplot(after_made_distance_data, aes(x = distance, y = fg_pct)) + geom_point() + labs(x = "Shot Distance", y = "FG%") + geom_vline(xintercept=23.8) + geom_vline(xintercept = 10) + ggtitle("League-Wide FG% by Distance after Scoring")

# 1b. All shots after missing, distance on x, FG% on y
ggplot(after_miss_distance_data, aes(x = distance, y = fg_pct)) + geom_point() + labs(x = "Shot Distance", y = "FG%") + geom_vline(xintercept=23.8) + geom_vline(xintercept = 10) + ggtitle("League-Wide FG% by Distance after Missing")

# 1c. All shots both after scoring and missing, distance on x, FG% on y
ggplot() + geom_point(data=after_made_distance_data, aes(x = distance, y = fg_pct, color="Scoring")) + geom_point(data = after_miss_distance_data, aes(x = distance, y = fg_pct, color="Missing")) + theme(legend.title=element_blank()) + 
  scale_color_manual(name="Legend", values = c("red", "green")) + ggtitle("After Missing vs After Scoring FG% by Distance") + geom_vline(xintercept=23.8) + labs(x = "Shot Distance", y = "FG%") + geom_vline(xintercept = 10)

# 2. Histogram of FG made by distance
qplot(scored_shots$SHOT_DIST,geom="histogram", xlab="Shot Distance", binwidth=0.1) + geom_vline(xintercept=23.8) + geom_vline(xintercept = 10) + xlim(0,30) + ggtitle("Histogram of Shots Scored by Distance")

# 3. Hot hand player shots, category (short, mid, 3 pt) on x, FG% on y
ggplot(hot_hand_distance_data, aes(x = category, y = fg_pct)) + geom_bar(stat="identity") + labs(x = "Shot Distance", y = "FG%") + ggtitle("Hot Hand Shooters FG% By Distance")

# 3a. 3 but split by previous shot result
ggplot(hot_hand_distance_data2, aes(x = distance, y = fg_pct, fill = type)) + geom_bar(stat="identity", position = "dodge") + labs(x = "Shot Distance", y = "FG%") + scale_fill_manual("Legend", values = c("After Missing" = "Red", "After Scoring" = "green"))

# 4. Not hot hand player shots, category (short, mid, 3 pt) on x, FG% on y
ggplot(not_hot_hand_distance_data, aes(x = category, y = fg_pct)) + geom_bar(stat="identity") + labs(x = "Shot Distance", y = "FG%") + ggtitle("Non-Hot Hand Shooters FG% By Distance")

# 4a. 4 but split by previous shot result
ggplot(not_hot_hand_distance_data2, aes(x = distance, y = fg_pct, fill = type)) + geom_bar(stat="identity", position = "dodge") + labs(x = "Shot Distance", y = "FG%") + scale_fill_manual("Legend", values = c("After Missing" = "Red", "After Scoring" = "green"))

# 5. Hot hand and not hot hand player shots
ggplot(overall_hot_hand_distance_data,aes(x = distance, y = fg_pct, fill = type)) + geom_bar(stat="identity", position = "dodge") + labs(x = "Shot Distance", y = "FG%") + ggtitle("Streaky and Consistent Shooters FG% By Distance")

# 6. East player shots, distance on x, FG% on y
ggplot(east_distance_data, aes(x = distance, y = fg_pct)) + geom_point() + labs(x = "Shot Distance", y = "FG%") + geom_vline(xintercept=23.8) + geom_vline(xintercept=10) + ggtitle("Eastern Conference FG% by Distance")

# 7. West player shots, distance on x, FG% on y
ggplot(west_distance_data, aes(x = distance, y = fg_pct)) + geom_point() + labs(x = "Shot Distance", y = "FG%") + geom_vline(xintercept=23.8) + geom_vline(xintercept=10) + ggtitle("Western Conference FG% by Distance")

# 8. Consecutive shots line chart
ggplot(consecutive_data,aes(x = consecutive_scored, y = fg_pct, group = 1)) + geom_line() + geom_point() + scale_y_continuous(limits=c(0.3,0.6)) + labs(x = "Consecutive Shots Scored", y = "FG%") + ggtitle("FG% by Consecutive Shots Scored")

# 9. East vs west player shots, distance on x, FG% on y, colored by conference
ggplot() + geom_point(data = east_distance_data, aes(x = distance, y = fg_pct, color = "blue")) + geom_point(data = west_distance_data, aes(x = distance, y = fg_pct, color = "red")) + theme(legend.title=element_blank()) + scale_color_hue(labels = c("East", "West")) + ggtitle("Eastern vs Western Conference FG% by Distance")

