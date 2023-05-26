library(lubridate)
library(dplyr)
resilience_table <- data.frame(matrix(ncol = 3, nrow = 0))
setwd("/Users/tim17/Desktop/CS_parsed")
df <- read.csv("CS_parsed_.csv")
df <- data.frame(lapply(df, function(x) gsub("'", "", x)))
df$rank_1 <- as.numeric(df$rank_1)
df$rank_2 <- as.numeric(df$rank_2)
df$score_t1 <- as.numeric(df$score_t1)
df$score_t2 <- as.numeric(df$score_t2)
players_table$age <- as.numeric(players_table$age)
#читаем таблицу шоков
shocks <- read.csv("map_changes2.csv")
for (i in 1:nrow(shocks)){
  #итерируемся по шокам
 map <- shocks[i,]
 map_name <- map[1][[1]]
 #Даст по разному называется в таблице и датафрейме - поэтому переназываю его
 if(map_name == "Dust II"){
   map_name <- "Dust2"
 }
 type <- map[3]
 if(type == "added"){
   shock_type <- 1
 }else{
   shock_type <- 0
 }
 date1 <- map[2][[1]]
 date_removed <- mdy(date1)
 if(!is.na(date_removed)){
   cat(map_name, date_removed, shock_type, "\n")
   month1_before <- date_removed - months(1)
   #коостыль для 29 фераля (которого не было в 2019)
   if(date_removed == "2019-03-29"){
     month1_before <- date_removed - days(1)- months(1)
   }
   #тут устанавливаем границы для срезов датафрейма
   month1_after <- date_removed + months(1)
   month3_before <- date_removed - months(3)
   month3_after <- date_removed + months(3)
   month6_before <- date_removed - months(6)
   month6_after <- date_removed + months(6)
   month2_before <- date_removed - months(2)
   month2_after <- date_removed + months(2)
   month4_before <- date_removed - months(4)
   month4_after <- date_removed + months(4)
   month5_before <- date_removed - months(5)
   month5_after <- date_removed + months(5)
  
   #производительность должна быть типа нумерик, чтобы мы ее могли обработать
   df$performance.m1_t1_player1_2.0 <- as.numeric(df$performance.m1_t1_player1_2.0)
   df$performance.m1_t1_player2_2.0 <- as.numeric(df$performance.m1_t1_player2_2.0)
   df$performance.m1_t1_player3_2.0 <- as.numeric(df$performance.m1_t1_player3_2.0)
   df$performance.m1_t1_player4_2.0 <- as.numeric(df$performance.m1_t1_player4_2.0)
   df$performance.m1_t1_player5_2.0 <- as.numeric(df$performance.m1_t1_player5_2.0)
   
   df$average2.0_t1 <- rowMeans(df[, c("performance.m1_t1_player1_2.0", "performance.m1_t1_player2_2.0", "performance.m1_t1_player3_2.0", "performance.m1_t1_player4_2.0", "performance.m1_t1_player5_2.0")])
   
   df$performance.m1_t2_player1_2.0 <- as.numeric(df$performance.m1_t2_player1_2.0)
   df$performance.m1_t2_player2_2.0 <- as.numeric(df$performance.m1_t2_player2_2.0)
   df$performance.m1_t2_player3_2.0 <- as.numeric(df$performance.m1_t2_player3_2.0)
   df$performance.m1_t2_player4_2.0 <- as.numeric(df$performance.m1_t2_player4_2.0)
   df$performance.m1_t2_player5_2.0 <- as.numeric(df$performance.m1_t2_player5_2.0)
   
   df$average2.0_t2 <- rowMeans(df[, c("performance.m1_t2_player1_2.0", "performance.m1_t2_player2_2.0", "performance.m1_t2_player3_2.0", "performance.m1_t2_player4_2.0", "performance.m1_t2_player5_2.0")])
   #далее считаю средний КД для команды в конкретном матче
   df$averageKD_t1 <- apply(df, 1, function(row) {
     K <- as.numeric(strsplit(row["performance.m1_t1_player1_K_D"], "-")[[1]][1]) + as.numeric(strsplit(row["performance.m1_t1_player2_K_D"], "-")[[1]][1]) + as.numeric(strsplit(row["performance.m1_t1_player3_K_D"], "-")[[1]][1]) + as.numeric(strsplit(row["performance.m1_t1_player4_K_D"], "-")[[1]][1]) + as.numeric(strsplit(row["performance.m1_t1_player5_K_D"], "-")[[1]][1])
     D <- as.numeric(strsplit(row["performance.m1_t1_player1_K_D"], "-")[[1]][2]) + as.numeric(strsplit(row["performance.m1_t1_player2_K_D"], "-")[[1]][2]) + as.numeric(strsplit(row["performance.m1_t1_player3_K_D"], "-")[[1]][2]) + as.numeric(strsplit(row["performance.m1_t1_player4_K_D"], "-")[[1]][2]) + as.numeric(strsplit(row["performance.m1_t1_player5_K_D"], "-")[[1]][2])
     averageKD_t1 <- K/D
     return(averageKD_t1)
   })
   
   df$averageKD_t2 <- apply(df, 1, function(row) {
     K <- as.numeric(strsplit(row["performance.m1_t2_player1_K_D"], "-")[[1]][1]) + as.numeric(strsplit(row["performance.m1_t2_player2_K_D"], "-")[[1]][1]) + as.numeric(strsplit(row["performance.m1_t2_player3_K_D"], "-")[[1]][1]) + as.numeric(strsplit(row["performance.m1_t2_player4_K_D"], "-")[[1]][1]) + as.numeric(strsplit(row["performance.m1_t2_player5_K_D"], "-")[[1]][1])
     D <- as.numeric(strsplit(row["performance.m1_t2_player1_K_D"], "-")[[1]][2]) + as.numeric(strsplit(row["performance.m1_t2_player2_K_D"], "-")[[1]][2]) + as.numeric(strsplit(row["performance.m1_t2_player3_K_D"], "-")[[1]][2]) + as.numeric(strsplit(row["performance.m1_t2_player4_K_D"], "-")[[1]][2]) + as.numeric(strsplit(row["performance.m1_t2_player5_K_D"], "-")[[1]][2])
     averageKD_t2 <- K/D
     return(averageKD_t2)
   })
   #срезы датафрейма
   before1_df <- df[ymd(df$date) < date_removed & ymd(df$date) > month1_before, ]
   after1_df <- df[ymd(df$date) > date_removed & ymd(df$date) < month1_after, ]
   before3_df <- df[ymd(df$date) < date_removed & ymd(df$date) > month3_before, ]
   after3_df <- df[ymd(df$date) > date_removed & ymd(df$date) < month3_after, ]
   before6_df <- df[ymd(df$date) < date_removed & ymd(df$date) > month6_before, ]
   after6_df <- df[ymd(df$date) > date_removed & ymd(df$date) < month6_after, ]
   before2_df <- df[ymd(df$date) < date_removed & ymd(df$date) > month2_before, ]
   after2_df <- df[ymd(df$date) > date_removed & ymd(df$date) < month2_after, ]
   before4_df <- df[ymd(df$date) < date_removed & ymd(df$date) > month4_before, ]
   after4_df <- df[ymd(df$date) > date_removed & ymd(df$date) < month4_after, ]
   before5_df <- df[ymd(df$date) < date_removed & ymd(df$date) > month5_before, ]
   after5_df <- df[ymd(df$date) > date_removed & ymd(df$date) < month5_after, ]
   

   #resilience
   teams1 <- unique(c(before1_df["team_1"][[1]], before1_df["team_2"][[1]]))
   teams3 <- unique(c(before3_df["team_1"][[1]], before3_df["team_2"][[1]]))
   teams6 <- unique(c(before6_df["team_1"][[1]], before6_df["team_2"][[1]]))
   teams2 <- unique(c(before2_df["team_1"][[1]], before2_df["team_2"][[1]]))
   teams4 <- unique(c(before4_df["team_1"][[1]], before4_df["team_2"][[1]]))
   teams5 <- unique(c(before5_df["team_1"][[1]], before5_df["team_2"][[1]]))
   teams_perf_after1 <- data.frame(matrix(ncol = 3, nrow = 0))
   teams_perf_before1 <- data.frame(matrix(ncol = 3, nrow = 0))
   teams_perf_after3 <- data.frame(matrix(ncol = 3, nrow = 0))
   teams_perf_before3 <- data.frame(matrix(ncol = 3, nrow = 0))
   teams_perf_after6 <- data.frame(matrix(ncol = 3, nrow = 0))
   teams_perf_before6 <- data.frame(matrix(ncol = 3, nrow = 0))
   teams_perf_after2 <- data.frame(matrix(ncol = 3, nrow = 0))
   teams_perf_before2 <- data.frame(matrix(ncol = 3, nrow = 0))
   teams_perf_after4 <- data.frame(matrix(ncol = 3, nrow = 0))
   teams_perf_before4 <- data.frame(matrix(ncol = 3, nrow = 0))
   teams_perf_after5 <- data.frame(matrix(ncol = 3, nrow = 0))
   teams_perf_before5 <- data.frame(matrix(ncol = 3, nrow = 0))
   map_stat_after1 <- data.frame(matrix(ncol = 3, nrow = 0))
   map_stat_before1 <- data.frame(matrix(ncol = 3, nrow = 0))
   map_stat_after3 <- data.frame(matrix(ncol = 3, nrow = 0))
   map_stat_before3 <- data.frame(matrix(ncol = 3, nrow = 0))
   map_stat_after6 <- data.frame(matrix(ncol = 3, nrow = 0))
   map_stat_before6 <- data.frame(matrix(ncol = 3, nrow = 0))
   #тут считаем производительность для 2 4 5 месяцев после шока, подробнее далее (в окнах равных 1 месяцу)
   for(i in 1:length(teams2)){
     team <- teams2[i]
     maps <- (unique(after2_df["map"])[[1]])
     kd1 <- mean(after2_df$averageKD_t1[after2_df$team_1 == team], na.rm = TRUE)
     kd2 <- mean(after2_df$averageKD_t2[after2_df$team_2 == team], na.rm = TRUE)
     count1 <- sum(after2_df$team_1 == team, na.rm = TRUE)
     count2 <- sum(after2_df$team_2 == team, na.rm = TRUE)
     if (is.na(count1)) count1 <- 0
     if (is.na(count2)) count2 <- 0
     rating2.0_1 <- mean(after2_df$average2.0_t1[after2_df$team_1 == team], na.rm = TRUE)
     rating2.0_2 <- mean(after2_df$average2.0_t2[after2_df$team_2 == team], na.rm = TRUE)
     rounds_1 <- mean(after2_df$score_t1[after2_df$team_1 == team], na.rm = TRUE)
     rounds_2 <- mean(after2_df$score_t2[after2_df$team_2 == team], na.rm = TRUE)
     if(count1 > 0 & count2 > 0){
       teams_perf_after2 <- rbind(teams_perf_after2, data.frame(team = team, kd = (kd1*(count1/(count1+count2)) + kd2*(count2/(count1+count2))), rating2.0 = (rating2.0_1*(count1/(count1+count2)) + rating2.0_2*(count2/(count1+count2))), rounds = (rounds_1*(count1/(count1+count2)) + rounds_2*(count2/(count1+count2)))))
     }else if(count1 > 0 & count2 == 0){
       teams_perf_after2 <- rbind(teams_perf_after2, data.frame(team = team, kd = kd1, rating2.0 = rating2.0_1, rounds = rounds_1))
     }else if(count2 > 0 & count1 == 0){
       teams_perf_after2 <- rbind(teams_perf_after2, data.frame(team = team, kd = kd2, rating2.0 = rating2.0_2, rounds = rounds_2))
     }
   }
   
   for(i in 1:length(teams4)){
     team <- teams4[i]
     maps <- (unique(after4_df["map"])[[1]])
     kd1 <- mean(after4_df$averageKD_t1[after4_df$team_1 == team], na.rm = TRUE)
     kd2 <- mean(after4_df$averageKD_t2[after4_df$team_2 == team], na.rm = TRUE)
     count1 <- sum(after4_df$team_1 == team, na.rm = TRUE)
     count2 <- sum(after4_df$team_2 == team, na.rm = TRUE)
     if (is.na(count1)) count1 <- 0
     if (is.na(count2)) count2 <- 0
     rating2.0_1 <- mean(after4_df$average2.0_t1[after4_df$team_1 == team], na.rm = TRUE)
     rating2.0_2 <- mean(after4_df$average2.0_t2[after4_df$team_2 == team], na.rm = TRUE)
     rounds_1 <- mean(after4_df$score_t1[after4_df$team_1 == team], na.rm = TRUE)
     rounds_2 <- mean(after4_df$score_t2[after4_df$team_2 == team], na.rm = TRUE)
     if(count1 > 0 & count2 > 0){
       teams_perf_after4 <- rbind(teams_perf_after4, data.frame(team = team, kd = (kd1*(count1/(count1+count2)) + kd2*(count2/(count1+count2))), rating2.0 = (rating2.0_1*(count1/(count1+count2)) + rating2.0_2*(count2/(count1+count2))), rounds = (rounds_1*(count1/(count1+count2)) + rounds_2*(count2/(count1+count2)))))
     }else if(count1 > 0 & count2 == 0){
       teams_perf_after4 <- rbind(teams_perf_after4, data.frame(team = team, kd = kd1, rating2.0 = rating2.0_1, rounds = rounds_1))
     }else if(count2 > 0 & count1 == 0){
       teams_perf_after4 <- rbind(teams_perf_after4, data.frame(team = team, kd = kd2, rating2.0 = rating2.0_2, rounds = rounds_2))
     }
   }
   
   for(i in 1:length(teams5)){
     team <- teams5[i]
     maps <- (unique(after5_df["map"])[[1]])
     kd1 <- mean(after5_df$averageKD_t1[after5_df$team_1 == team], na.rm = TRUE)
     kd2 <- mean(after5_df$averageKD_t2[after5_df$team_2 == team], na.rm = TRUE)
     count1 <- sum(after5_df$team_1 == team, na.rm = TRUE)
     count2 <- sum(after5_df$team_2 == team, na.rm = TRUE)
     if (is.na(count1)) count1 <- 0
     if (is.na(count2)) count2 <- 0
     rating2.0_1 <- mean(after5_df$average2.0_t1[after5_df$team_1 == team], na.rm = TRUE)
     rating2.0_2 <- mean(after5_df$average2.0_t2[after5_df$team_2 == team], na.rm = TRUE)
     rounds_1 <- mean(after5_df$score_t1[after5_df$team_1 == team], na.rm = TRUE)
     rounds_2 <- mean(after5_df$score_t2[after5_df$team_2 == team], na.rm = TRUE)
     if(count1 > 0 & count2 > 0){
       teams_perf_after5 <- rbind(teams_perf_after5, data.frame(team = team, kd = (kd1*(count1/(count1+count2)) + kd2*(count2/(count1+count2))), rating2.0 = (rating2.0_1*(count1/(count1+count2)) + rating2.0_2*(count2/(count1+count2))), rounds = (rounds_1*(count1/(count1+count2)) + rounds_2*(count2/(count1+count2)))))
     }else if(count1 > 0 & count2 == 0){
       teams_perf_after5 <- rbind(teams_perf_after5, data.frame(team = team, kd = kd1, rating2.0 = rating2.0_1, rounds = rounds_1))
     }else if(count2 > 0 & count1 == 0){
       teams_perf_after5 <- rbind(teams_perf_after5, data.frame(team = team, kd = kd2, rating2.0 = rating2.0_2, rounds = rounds_2))
     }
   }
   
   for(i in 1:length(teams1)){
     team <- teams1[i]
     #after1
     maps <- (unique(after1_df["map"])[[1]])
     #считаем нужные метрики по данному окну
     kd1 <- mean(after1_df$averageKD_t1[after1_df$team_1 == team], na.rm = TRUE)
     kd2 <- mean(after1_df$averageKD_t2[after1_df$team_2 == team], na.rm = TRUE)
     kd1_opponent <- mean(after1_df$averageKD_t2[after1_df$team_1 == team], na.rm = TRUE)
     kd2_opponent <- mean(after1_df$averageKD_t1[after1_df$team_2 == team], na.rm = TRUE)
     count1 <- sum(after1_df$team_1 == team, na.rm = TRUE)
     count2 <- sum(after1_df$team_2 == team, na.rm = TRUE)
     if (is.na(count1)) count1 <- 0
     if (is.na(count2)) count2 <- 0
     rating2.0_1 <- mean(after1_df$average2.0_t1[after1_df$team_1 == team], na.rm = TRUE)
     rating2.0_2 <- mean(after1_df$average2.0_t2[after1_df$team_2 == team], na.rm = TRUE)
     rounds_1 <- mean(after1_df$score_t1[after1_df$team_1 == team], na.rm = TRUE)
     rounds_2 <- mean(after1_df$score_t2[after1_df$team_2 == team], na.rm = TRUE)
     rating2.0_1_opponent <- mean(after1_df$average2.0_t2[after1_df$team_1 == team], na.rm = TRUE)
     rating2.0_2_opponent <- mean(after1_df$average2.0_t1[after1_df$team_2 == team], na.rm = TRUE)
     #далее все if-ы нужны, чтобы было меньше пропусков (поскольку если одна из переменных в выражении NA или есть деление на 0, получаем пропус)
     if(count1 > 0 & count2 > 0){
       teams_perf_after1 <- rbind(teams_perf_after1, data.frame(team = team, kd = (kd1*(count1/(count1+count2)) + kd2*(count2/(count1+count2))), kd_opponent = (kd1_opponent*(count1/(count1+count2)) + kd2_opponent*(count2/(count1+count2))), rating2.0 = (rating2.0_1*(count1/(count1+count2)) + rating2.0_2*(count2/(count1+count2))), rating2.0_opponent = (rating2.0_1_opponent*(count1/(count1+count2)) + rating2.0_2_opponent*(count2/(count1+count2))), rounds = (rounds_1*(count1/(count1+count2)) + rounds_2*(count2/(count1+count2)))))
     }else if(count1 > 0 & count2 == 0){
       teams_perf_after1 <- rbind(teams_perf_after1, data.frame(team = team, kd = kd1, kd_opponent = kd1_opponent, rating2.0 = rating2.0_1, rating2.0_opponent = rating2.0_1_opponent, rounds = rounds_1))
     }else if(count2 > 0 & count1 == 0){
       teams_perf_after1 <- rbind(teams_perf_after1, data.frame(team = team, kd = kd2, kd_opponent = kd2_opponent, rating2.0 = rating2.0_2, rating2.0_opponent = rating2.0_2_opponent, rounds = rounds_2))
     }
     maps_played_1 <- count1+count2
     played_on_map <- as.numeric(sum(after1_df$team_1[after1_df$map == map_name] == team, na.rm = TRUE)) + as.numeric(sum(after1_df$team_2[after1_df$map == map_name] == team, na.rm = TRUE))
     maps_vector <- numeric(length = length(maps))
     #тут считаем доли для каждой карты
     for (l in 1:length(maps)){
       map <- maps[l]
       if(!is.na(map)){
         maps_vector[l] <- (as.numeric(sum(after1_df$team_1[after1_df$map == map] == team, na.rm = TRUE)) + as.numeric(sum(after1_df$team_2[after1_df$map == map] == team, na.rm = TRUE)))/maps_played_1
       }
     }
     hhi <- 0
     for(var in maps_vector){
       hhi <- hhi + var**2
     }
     map_stat_after1 <- rbind(map_stat_after1, data.frame(team = team, played_on_map_after1 = played_on_map, proportion_after1 = played_on_map/maps_played_1, hhi_after1 = hhi, num_maps_played_after1 = maps_played_1))
     #before1
     maps <- (unique(before1_df["map"])[[1]])
     kd1 <- mean(before1_df$averageKD_t1[before1_df$team_1 == team], na.rm = TRUE)
     kd2 <- mean(before1_df$averageKD_t2[before1_df$team_2 == team], na.rm = TRUE)
     kd1_opponent <- mean(before1_df$averageKD_t2[before1_df$team_1 == team], na.rm = TRUE)
     kd2_opponent <- mean(before1_df$averageKD_t1[before1_df$team_2 == team], na.rm = TRUE)
     count1 <- sum(before1_df$team_1 == team, na.rm = TRUE)
     count2 <- sum(before1_df$team_2 == team, na.rm = TRUE)
     if (is.na(count1)) count1 <- 0
     if (is.na(count2)) count2 <- 0
     rating2.0_1 <- mean(before1_df$average2.0_t1[before1_df$team_1 == team], na.rm = TRUE)
     rating2.0_2 <- mean(before1_df$average2.0_t2[before1_df$team_2 == team], na.rm = TRUE)
     rounds_1 <- mean(before1_df$score_t1[before1_df$team_1 == team], na.rm = TRUE)
     rounds_2 <- mean(before1_df$score_t2[before1_df$team_2 == team], na.rm = TRUE)
     rating2.0_1_opponent <- mean(before1_df$average2.0_t2[before1_df$team_1 == team], na.rm = TRUE)
     rating2.0_2_opponent <- mean(before1_df$average2.0_t1[before1_df$team_2 == team], na.rm = TRUE)
     if(count1 > 0 & count2 > 0){
       teams_perf_before1 <- rbind(teams_perf_before1, data.frame(team = team, kd = (kd1*(count1/(count1+count2)) + kd2*(count2/(count1+count2))), kd_opponent = (kd1_opponent*(count1/(count1+count2)) + kd2_opponent*(count2/(count1+count2))), rating2.0 = (rating2.0_1*(count1/(count1+count2)) + rating2.0_2*(count2/(count1+count2))), rating2.0_opponent = (rating2.0_1_opponent*(count1/(count1+count2)) + rating2.0_2_opponent*(count2/(count1+count2))), rounds = (rounds_1*(count1/(count1+count2)) + rounds_2*(count2/(count1+count2)))))
     }else if(count1 > 0 & count2 == 0){
       teams_perf_before1 <- rbind(teams_perf_before1, data.frame(team = team, kd = kd1, kd_opponent = kd1_opponent, rating2.0 = rating2.0_1, rating2.0_opponent = rating2.0_1_opponent, rounds = rounds_1))
     }else if(count2 > 0 & count1 == 0){
       teams_perf_before1 <- rbind(teams_perf_before1, data.frame(team = team, kd = kd2, kd_opponent = kd2_opponent, rating2.0 = rating2.0_2, rating2.0_opponent = rating2.0_2_opponent, rounds = rounds_2))
     }
     maps_played_1 <- count1+count2
     played_on_map <- as.numeric(sum(before1_df$team_1[before1_df$map == map_name] == team, na.rm = TRUE)) + as.numeric(sum(before1_df$team_2[before1_df$map == map_name] == team, na.rm = TRUE))
     maps_vector <- numeric(length = length(maps))
     for (l in 1:length(maps)){
       map <- maps[l]
       if(!is.na(map)){
         maps_vector[l] <- (as.numeric(sum(before1_df$team_1[before1_df$map == map] == team, na.rm = TRUE)) + as.numeric(sum(before1_df$team_2[before1_df$map == map] == team, na.rm = TRUE)))/maps_played_1
       }
     }
     hhi <- 0
     for(var in maps_vector){
       hhi <- hhi + var**2
     }
     map_stat_before1 <- rbind(map_stat_before1, data.frame(team = team, played_on_map_before1 = played_on_map, proportion_before1 = played_on_map/maps_played_1, hhi_before1 = hhi, num_maps_played_before1 = maps_played_1))
   }
   cat("Stat for 1-month period is counted \n")
   
   for(i in 1:length(teams3)){
     team <- teams3[i]
     #after3
     maps <- (unique(after3_df["map"])[[1]])
     kd1 <- mean(after3_df$averageKD_t1[after3_df$team_1 == team], na.rm = TRUE)
     kd2 <- mean(after3_df$averageKD_t2[after3_df$team_2 == team], na.rm = TRUE)
     kd1_opponent <- mean(after3_df$averageKD_t2[after3_df$team_1 == team], na.rm = TRUE)
     kd2_opponent <- mean(after3_df$averageKD_t1[after3_df$team_2 == team], na.rm = TRUE)
     count1 <- sum(after3_df$team_1 == team, na.rm = TRUE)
     count2 <- sum(after3_df$team_2 == team, na.rm = TRUE)
     if (is.na(count1)) count1 <- 0
     if (is.na(count2)) count2 <- 0
     rating2.0_1 <- mean(after3_df$average2.0_t1[after3_df$team_1 == team], na.rm = TRUE)
     rating2.0_2 <- mean(after3_df$average2.0_t2[after3_df$team_2 == team], na.rm = TRUE)
     rounds_1 <- mean(after3_df$score_t1[after3_df$team_1 == team], na.rm = TRUE)
     rounds_2 <- mean(after3_df$score_t2[after3_df$team_2 == team], na.rm = TRUE)
     rating2.0_1_opponent <- mean(after3_df$average2.0_t2[after3_df$team_1 == team], na.rm = TRUE)
     rating2.0_2_opponent <- mean(after3_df$average2.0_t1[after3_df$team_2 == team], na.rm = TRUE)
     if(count1 > 0 & count2 > 0){
       teams_perf_after3 <- rbind(teams_perf_after3, data.frame(team = team, kd = (kd1*(count1/(count1+count2)) + kd2*(count2/(count1+count2))), kd_opponent = (kd1_opponent*(count1/(count1+count2)) + kd2_opponent*(count2/(count1+count2))), rating2.0 = (rating2.0_1*(count1/(count1+count2)) + rating2.0_2*(count2/(count1+count2))), rating2.0_opponent = (rating2.0_1_opponent*(count1/(count1+count2)) + rating2.0_2_opponent*(count2/(count1+count2))), rounds = (rounds_1*(count1/(count1+count2)) + rounds_2*(count2/(count1+count2)))))
     }else if(count1 > 0 & count2 == 0){
       teams_perf_after3 <- rbind(teams_perf_after3, data.frame(team = team, kd = kd1, kd_opponent = kd1_opponent, rating2.0 = rating2.0_1, rating2.0_opponent = rating2.0_1_opponent, rounds = rounds_1))
     }else if(count2 > 0 & count1 == 0){
       teams_perf_after3 <- rbind(teams_perf_after3, data.frame(team = team, kd = kd2, kd_opponent = kd2_opponent, rating2.0 = rating2.0_2, rating2.0_opponent = rating2.0_2_opponent, rounds = rounds_2))
     }
     maps_played_1 <- count1+count2
     played_on_map <- as.numeric(sum(after3_df$team_1[after3_df$map == map_name] == team, na.rm = TRUE)) + as.numeric(sum(after3_df$team_2[after3_df$map == map_name] == team, na.rm = TRUE))
     maps_vector <- numeric(length = length(maps))
     for (l in 1:length(maps)){
       map <- maps[l]
       if(!is.na(map)){
         maps_vector[l] <- (as.numeric(sum(after3_df$team_1[after3_df$map == map] == team, na.rm = TRUE)) + as.numeric(sum(after3_df$team_2[after3_df$map == map] == team, na.rm = TRUE)))/maps_played_1
       }
     }
     hhi <- 0
     for(var in maps_vector){
       hhi <- hhi + var**2
     }
     map_stat_after3 <- rbind(map_stat_after3, data.frame(team = team, played_on_map_after3 = played_on_map, proportion_after3 = played_on_map/maps_played_1, hhi_after3 = hhi, num_maps_played_after3 = maps_played_1))
     #before3
     maps <- (unique(before3_df["map"])[[1]])
     kd1 <- mean(before3_df$averageKD_t1[before3_df$team_1 == team], na.rm = TRUE)
     kd2 <- mean(before3_df$averageKD_t2[before3_df$team_2 == team], na.rm = TRUE)
     kd1_opponent <- mean(before3_df$averageKD_t2[before3_df$team_1 == team], na.rm = TRUE)
     kd2_opponent <- mean(before3_df$averageKD_t1[before3_df$team_2 == team], na.rm = TRUE)
     count1 <- sum(before3_df$team_1 == team, na.rm = TRUE)
     count2 <- sum(before3_df$team_2 == team, na.rm = TRUE)
     if (is.na(count1)) count1 <- 0
     if (is.na(count2)) count2 <- 0
     rating2.0_1 <- mean(before3_df$average2.0_t1[before3_df$team_1 == team], na.rm = TRUE)
     rating2.0_2 <- mean(before3_df$average2.0_t2[before3_df$team_2 == team], na.rm = TRUE)
     rounds_1 <- mean(before3_df$score_t1[before3_df$team_1 == team], na.rm = TRUE)
     rounds_2 <- mean(before3_df$score_t2[before3_df$team_2 == team], na.rm = TRUE)
     rating2.0_1_opponent <- mean(before3_df$average2.0_t2[before3_df$team_1 == team], na.rm = TRUE)
     rating2.0_2_opponent <- mean(before3_df$average2.0_t1[before3_df$team_2 == team], na.rm = TRUE)
     if(count1 > 0 & count2 > 0){
       teams_perf_before3 <- rbind(teams_perf_before3, data.frame(team = team, kd = (kd1*(count1/(count1+count2)) + kd2*(count2/(count1+count2))), kd_opponent = (kd1_opponent*(count1/(count1+count2)) + kd2_opponent*(count2/(count1+count2))), rating2.0 = (rating2.0_1*(count1/(count1+count2)) + rating2.0_2*(count2/(count1+count2))), rating2.0_opponent = (rating2.0_1_opponent*(count1/(count1+count2)) + rating2.0_2_opponent*(count2/(count1+count2))), rounds = (rounds_1*(count1/(count1+count2)) + rounds_2*(count2/(count1+count2)))))
     }else if(count1 > 0 & count2 == 0){
       teams_perf_before3 <- rbind(teams_perf_before3, data.frame(team = team, kd = kd1, kd_opponent = kd1_opponent, rating2.0 = rating2.0_1, rating2.0_opponent = rating2.0_1_opponent, rounds = rounds_1))
     }else if(count2 > 0 & count1 == 0){
       teams_perf_before3 <- rbind(teams_perf_before3, data.frame(team = team, kd = kd2, kd_opponent = kd2_opponent, rating2.0 = rating2.0_2, rating2.0_opponent = rating2.0_2_opponent, rounds = rounds_2))
     }
     maps_played_1 <- count1+count2
     played_on_map <- as.numeric(sum(before3_df$team_1[before3_df$map == map_name] == team, na.rm = TRUE)) + as.numeric(sum(before3_df$team_2[before3_df$map == map_name] == team, na.rm = TRUE))
     maps_vector <- numeric(length = length(maps))
     for (l in 1:length(maps)){
       map <- maps[l]
       if(!is.na(map)){
         maps_vector[l] <- (as.numeric(sum(before3_df$team_1[before3_df$map == map] == team, na.rm = TRUE)) + as.numeric(sum(before3_df$team_2[before3_df$map == map] == team, na.rm = TRUE)))/maps_played_1
       }
     }
     hhi <- 0
     for(var in maps_vector){
       hhi <- hhi + var**2
     }
     map_stat_before3 <- rbind(map_stat_before3, data.frame(team = team, played_on_map_before3 = played_on_map, proportion_before3 = played_on_map/maps_played_1, hhi_before3 = hhi, num_maps_played_before3 = maps_played_1))
   }
   cat("Stat for 3-months period is counted \n")
   for(i in 1:length(teams6)){
     team <- teams6[i]
     #after6
     maps <- (unique(after6_df["map"])[[1]])
     kd1 <- mean(after6_df$averageKD_t1[after6_df$team_1 == team], na.rm = TRUE)
     kd2 <- mean(after6_df$averageKD_t2[after6_df$team_2 == team], na.rm = TRUE)
     kd1_opponent <- mean(after6_df$averageKD_t2[after6_df$team_1 == team], na.rm = TRUE)
     kd2_opponent <- mean(after6_df$averageKD_t1[after6_df$team_2 == team], na.rm = TRUE)
     count1 <- sum(after6_df$team_1 == team, na.rm = TRUE)
     count2 <- sum(after6_df$team_2 == team, na.rm = TRUE)
     if (is.na(count1)) count1 <- 0
     if (is.na(count2)) count2 <- 0
     rating2.0_1 <- mean(after6_df$average2.0_t1[after6_df$team_1 == team], na.rm = TRUE)
     rating2.0_2 <- mean(after6_df$average2.0_t2[after6_df$team_2 == team], na.rm = TRUE)
     rounds_1 <- mean(after6_df$score_t1[after6_df$team_1 == team], na.rm = TRUE)
     rounds_2 <- mean(after6_df$score_t2[after6_df$team_2 == team], na.rm = TRUE)
     rating2.0_1_opponent <- mean(after6_df$average2.0_t2[after6_df$team_1 == team], na.rm = TRUE)
     rating2.0_2_opponent <- mean(after6_df$average2.0_t1[after6_df$team_2 == team], na.rm = TRUE)
     if(count1 > 0 & count2 > 0){
       teams_perf_after6 <- rbind(teams_perf_after6, data.frame(team = team, kd = (kd1*(count1/(count1+count2)) + kd2*(count2/(count1+count2))), kd_opponent = (kd1_opponent*(count1/(count1+count2)) + kd2_opponent*(count2/(count1+count2))), rating2.0 = (rating2.0_1*(count1/(count1+count2)) + rating2.0_2*(count2/(count1+count2))), rating2.0_opponent = (rating2.0_1_opponent*(count1/(count1+count2)) + rating2.0_2_opponent*(count2/(count1+count2))), rounds = (rounds_1*(count1/(count1+count2)) + rounds_2*(count2/(count1+count2)))))
     }else if(count1 > 0 & count2 == 0){
       teams_perf_after6 <- rbind(teams_perf_after6, data.frame(team = team, kd = kd1, kd_opponent = kd1_opponent, rating2.0 = rating2.0_1, rating2.0_opponent = rating2.0_1_opponent, rounds = rounds_1))
     }else if(count2 > 0 & count1 == 0){
       teams_perf_after6 <- rbind(teams_perf_after6, data.frame(team = team, kd = kd2, kd_opponent = kd2_opponent, rating2.0 = rating2.0_2, rating2.0_opponent = rating2.0_2_opponent, rounds = rounds_2))
     }
     maps_played_1 <- count1+count2
     played_on_map <- as.numeric(sum(after6_df$team_1[after6_df$map == map_name] == team, na.rm = TRUE)) + as.numeric(sum(after6_df$team_2[after6_df$map == map_name] == team, na.rm = TRUE))
     maps_vector <- numeric(length = length(maps))
     for (l in 1:length(maps)){
       map <- maps[l]
       if(!is.na(map)){
         maps_vector[l] <- (as.numeric(sum(after6_df$team_1[after6_df$map == map] == team, na.rm = TRUE)) + as.numeric(sum(after6_df$team_2[after6_df$map == map] == team, na.rm = TRUE)))/maps_played_1
       }
     }
     hhi <- 0
     for(var in maps_vector){
       hhi <- hhi + var**2
     }
     map_stat_after6 <- rbind(map_stat_after6, data.frame(team = team, played_on_map_after6 = played_on_map, proportion_after6 = played_on_map/maps_played_1, hhi_after6 = hhi, num_maps_played_after6 = maps_played_1))
     #before6
     maps <- (unique(before6_df["map"])[[1]])
     kd1 <- mean(before6_df$averageKD_t1[before6_df$team_1 == team], na.rm = TRUE)
     kd2 <- mean(before6_df$averageKD_t2[before6_df$team_2 == team], na.rm = TRUE)
     kd1_opponent <- mean(after1_df$averageKD_t2[before6_df$team_1 == team], na.rm = TRUE)
     kd2_opponent <- mean(after1_df$averageKD_t1[before6_df$team_2 == team], na.rm = TRUE)
     count1 <- sum(before6_df$team_1 == team, na.rm = TRUE)
     count2 <- sum(before6_df$team_2 == team, na.rm = TRUE)
     if (is.na(count1)) count1 <- 0
     if (is.na(count2)) count2 <- 0
     average_rank1 <- mean(before6_df$rank_2[before6_df$team_1 == team & before6_df$rank_2 != "-"], na.rm = TRUE)
     average_rank2 <- mean(before6_df$rank_1[before6_df$team_2 == team & before6_df$rank_1 != "-"], na.rm = TRUE)
     average_rank1_t <- mean(before6_df$rank_1[before6_df$team_1 == team & before6_df$rank_1 != "-"], na.rm = TRUE)
     average_rank2_t <- mean(before6_df$rank_2[before6_df$team_2 == team & before6_df$rank_2 != "-"], na.rm = TRUE)
     rating2.0_1 <- mean(before6_df$average2.0_t1[before6_df$team_1 == team], na.rm = TRUE)
     rating2.0_2 <- mean(before6_df$average2.0_t2[before6_df$team_2 == team], na.rm = TRUE)
     rounds_1 <- mean(before6_df$score_t1[before6_df$team_1 == team], na.rm = TRUE)
     rounds_2 <- mean(before6_df$score_t2[before6_df$team_2 == team], na.rm = TRUE)
     rating2.0_1_opponent <- mean(before6_df$average2.0_t2[before6_df$team_1 == team], na.rm = TRUE)
     rating2.0_2_opponent <- mean(before6_df$average2.0_t1[before6_df$team_2 == team], na.rm = TRUE)
     if(count1 > 0 & count2 > 0){
       if(!is.na(average_rank1) & !is.na(average_rank2)){
         teams_perf_before6 <- rbind(teams_perf_before6, data.frame(team = team, kd = (kd1*(count1/(count1+count2)) + kd2*(count2/(count1+count2))), average_rank = (average_rank1*(count1/(count1+count2)) + average_rank2*(count2/(count1+count2))), team_rank = (average_rank1_t*(count1/(count1+count2)) + average_rank2_t*(count2/(count1+count2))), kd_opponent = (kd1_opponent*(count1/(count1+count2)) + kd2_opponent*(count2/(count1+count2))), rating2.0 = (rating2.0_1*(count1/(count1+count2)) + rating2.0_2*(count2/(count1+count2))), rating2.0_opponent = (rating2.0_1_opponent*(count1/(count1+count2)) + rating2.0_2_opponent*(count2/(count1+count2))), rounds = (rounds_1*(count1/(count1+count2)) + rounds_2*(count2/(count1+count2)))))
       }else if(is.na(average_rank1)){
         if(is.na(average_rank1_t)){
           teams_perf_before6 <- rbind(teams_perf_before6, data.frame(team = team, kd = (kd1*(count1/(count1+count2)) + kd2*(count2/(count1+count2))), average_rank = average_rank2, team_rank = average_rank2_t, kd_opponent = (kd1_opponent*(count1/(count1+count2)) + kd2_opponent*(count2/(count1+count2))), rating2.0 = (rating2.0_1*(count1/(count1+count2)) + rating2.0_2*(count2/(count1+count2))), rating2.0_opponent = (rating2.0_1_opponent*(count1/(count1+count2)) + rating2.0_2_opponent*(count2/(count1+count2))), rounds = (rounds_1*(count1/(count1+count2)) + rounds_2*(count2/(count1+count2)))))
         }else if(is.na(average_rank2_t)){
           teams_perf_before6 <- rbind(teams_perf_before6, data.frame(team = team, kd = (kd1*(count1/(count1+count2)) + kd2*(count2/(count1+count2))), average_rank = average_rank2, team_rank = average_rank1_t, kd_opponent = (kd1_opponent*(count1/(count1+count2)) + kd2_opponent*(count2/(count1+count2))), rating2.0 = (rating2.0_1*(count1/(count1+count2)) + rating2.0_2*(count2/(count1+count2))), rating2.0_opponent = (rating2.0_1_opponent*(count1/(count1+count2)) + rating2.0_2_opponent*(count2/(count1+count2))), rounds = (rounds_1*(count1/(count1+count2)) + rounds_2*(count2/(count1+count2)))))
         }else {
           teams_perf_before6 <- rbind(teams_perf_before6, data.frame(team = team, kd = (kd1*(count1/(count1+count2)) + kd2*(count2/(count1+count2))), average_rank = average_rank2, team_rank = (average_rank1_t*(count1/(count1+count2)) + average_rank2_t*(count2/(count1+count2))), kd_opponent = (kd1_opponent*(count1/(count1+count2)) + kd2_opponent*(count2/(count1+count2))), rating2.0 = (rating2.0_1*(count1/(count1+count2)) + rating2.0_2*(count2/(count1+count2))), rating2.0_opponent = (rating2.0_1_opponent*(count1/(count1+count2)) + rating2.0_2_opponent*(count2/(count1+count2))), rounds = (rounds_1*(count1/(count1+count2)) + rounds_2*(count2/(count1+count2)))))
         }
       }else{
         if(is.na(average_rank1_t)){
           teams_perf_before6 <- rbind(teams_perf_before6, data.frame(team = team, kd = (kd1*(count1/(count1+count2)) + kd2*(count2/(count1+count2))), average_rank = average_rank1, team_rank = average_rank2_t, kd_opponent = (kd1_opponent*(count1/(count1+count2)) + kd2_opponent*(count2/(count1+count2))), rating2.0 = (rating2.0_1*(count1/(count1+count2)) + rating2.0_2*(count2/(count1+count2))), rating2.0_opponent = (rating2.0_1_opponent*(count1/(count1+count2)) + rating2.0_2_opponent*(count2/(count1+count2))), rounds = (rounds_1*(count1/(count1+count2)) + rounds_2*(count2/(count1+count2)))))
         }else if(is.na(average_rank2_t)){
           teams_perf_before6 <- rbind(teams_perf_before6, data.frame(team = team, kd = (kd1*(count1/(count1+count2)) + kd2*(count2/(count1+count2))), average_rank = average_rank1, team_rank = average_rank1_t, kd_opponent = (kd1_opponent*(count1/(count1+count2)) + kd2_opponent*(count2/(count1+count2))), rating2.0 = (rating2.0_1*(count1/(count1+count2)) + rating2.0_2*(count2/(count1+count2))), rating2.0_opponent = (rating2.0_1_opponent*(count1/(count1+count2)) + rating2.0_2_opponent*(count2/(count1+count2))), rounds = (rounds_1*(count1/(count1+count2)) + rounds_2*(count2/(count1+count2)))))
         }else {
           teams_perf_before6 <- rbind(teams_perf_before6, data.frame(team = team, kd = (kd1*(count1/(count1+count2)) + kd2*(count2/(count1+count2))), average_rank = average_rank1, team_rank = (average_rank1_t*(count1/(count1+count2)) + average_rank2_t*(count2/(count1+count2))), kd_opponent = (kd1_opponent*(count1/(count1+count2)) + kd2_opponent*(count2/(count1+count2))), rating2.0 = (rating2.0_1*(count1/(count1+count2)) + rating2.0_2*(count2/(count1+count2))), rating2.0_opponent = (rating2.0_1_opponent*(count1/(count1+count2)) + rating2.0_2_opponent*(count2/(count1+count2))), rounds = (rounds_1*(count1/(count1+count2)) + rounds_2*(count2/(count1+count2)))))
         }
       }
     }else if(count1 > 0 & count2 == 0){
       teams_perf_before6 <- rbind(teams_perf_before6, data.frame(team = team, kd = kd1, kd_opponent = kd1_opponent, rating2.0 = rating2.0_1, rating2.0_opponent = rating2.0_1_opponent, average_rank = average_rank1, team_rank = average_rank1_t, rounds = rounds_1))
     }else if(count2 > 0 & count1 == 0){
       teams_perf_before6 <- rbind(teams_perf_before6, data.frame(team = team, kd = kd2, kd_opponent = kd2_opponent, rating2.0 = rating2.0_2, rating2.0_opponent = rating2.0_2_opponent, average_rank = average_rank2, team_rank = average_rank2_t, rounds = rounds_2))
     }
     maps_played_1 <- count1+count2
     played_on_map <- as.numeric(sum(before6_df$team_1[before6_df$map == map_name] == team, na.rm = TRUE)) + as.numeric(sum(before6_df$team_2[before6_df$map == map_name] == team, na.rm = TRUE))
     if(shock_type == 0){
       not_playing_del_map <- (played_on_map == 0)
     }else{
       not_playing_del_map <- NA
     }
     maps_vector <- numeric(length = length(maps))
     for (l in 1:length(maps)){
       map <- maps[l]
       if(!is.na(map)){
         maps_vector[l] <- (as.numeric(sum(before6_df$team_1[before6_df$map == map] == team, na.rm = TRUE)) + as.numeric(sum(before6_df$team_2[before6_df$map == map] == team, na.rm = TRUE)))/maps_played_1
       }
     }
     hhi <- 0
     for(var in maps_vector){
       hhi <- hhi + var**2
     }
     
     #создаем датафрейм игроков
     new_df <- before6_df[
       before6_df$team_1 == team,
       c("date",
         "performance.m1_t1_player1",
         "performance.m1_t1_player2",
         "performance.m1_t1_player3",
         "performance.m1_t1_player4",
         "performance.m1_t1_player5"
       )
     ][complete.cases(before6_df[
       before6_df$team_1 == team,
       c("date",
         "performance.m1_t1_player1",
         "performance.m1_t1_player2",
         "performance.m1_t1_player3",
         "performance.m1_t1_player4",
         "performance.m1_t1_player5"
       )
     ]),]
     
     new_df2 <- before6_df[
       before6_df$team_2 == team,
       c( "date",
          "performance.m1_t2_player1",
          "performance.m1_t2_player2",
          "performance.m1_t2_player3",
          "performance.m1_t2_player4",
          "performance.m1_t2_player5"
       )
     ][complete.cases(before6_df[
       before6_df$team_2 == team,
       c("date",
         "performance.m1_t2_player1",
         "performance.m1_t2_player2",
         "performance.m1_t2_player3",
         "performance.m1_t2_player4",
         "performance.m1_t2_player5"
       )
     ]),]
     
     colnames(new_df2) <- colnames(new_df)
     new_df <- rbind(new_df, new_df2)
     new_df <- arrange(new_df, new_df$date)
     ctr <- 0
     #заполняем датафрейм игроков, чтобы затем посчитать их количество
     if(nrow(new_df) > 0){
       players_unique <- c(new_df[1, 2:6])
       players_unique <- unique(c(players_unique, new_df[nrow(new_df), 2:6]))
     }
     #вложные циклы сравнивают соседние строки, чтобы посмотреть, сколько изменнеий в составе (считаем shifts)
     if(nrow(new_df) > 1){
       for(k in 1:(nrow(new_df)-1)){
         players_unique <- unique(c(players_unique, new_df[k, 2:6]))
         ctrr <- 0
         for(l in 2:6){
           for(f in 2:6){
             if(new_df[k, l] == new_df[k+1, f]){
               ctrr <- ctrr + 1
             }
           }
         }
         ctr <- ctr + 5 - ctrr 
       }
     }
     tot_players <- 5
     if(nrow(new_df) > 0){
       tot_players <- length(players_unique)
     }
     first <- new_df[1, ]
     players_table_unique <- players_table[!duplicated(players_table$nickname), ]
     first$nationality_counts <- -Inf
     first$max_group_count <- -Inf
     #Далее считаем количество игроков одной национальности и из одной языковой группы + возрастные характеристики
     first %<>%
       rowwise() %>%
       mutate(
         max_nationality_count = {
           players <- c(performance.m1_t1_player1, performance.m1_t1_player2, performance.m1_t1_player3,
                        performance.m1_t1_player4, performance.m1_t1_player5)
           nationality_counts <- table(players_table_unique$nationality[players_table_unique$nickname %in% players])
           max(nationality_counts)
         },
         max_group_count = {
           players <- c(performance.m1_t1_player1, performance.m1_t1_player2, performance.m1_t1_player3,
                        performance.m1_t1_player4, performance.m1_t1_player5)
           players <- players[complete.cases(players)]
           group_counts <- table(players_table_unique$group[players_table_unique$nickname %in% players])
           max(group_counts)
         },
         average_age = {
           players <- c(performance.m1_t1_player1, performance.m1_t1_player2, performance.m1_t1_player3,
                        performance.m1_t1_player4, performance.m1_t1_player5)
           mean_age <- mean(players_table_unique$age[players_table_unique$nickname %in% players], na.rm = TRUE)
           mean_age
         },
         max_age_delta = {
           players <- c(performance.m1_t1_player1, performance.m1_t1_player2, performance.m1_t1_player3,
                        performance.m1_t1_player4, performance.m1_t1_player5)
           players <- players[complete.cases(players)]
           age_vector <- players_table_unique$age[players_table_unique$nickname %in% players]
           max_age_delta <- max(diff(age_vector, na.rm = TRUE))
           max_age_delta
         }
       ) %>%
       ungroup()
     nationality_counts <- first$max_nationality_count
     max_group_count <- first$max_group_count
     
     average_age <- first$average_age
     max_age_delta <- first$max_age_delta
     if(is.nan(average_age)){
       average_age <- NA
     }
     if(!is.finite(max_age_delta)){
       max_age_delta <- NA
     }
     if(max_group_count != -Inf & nationality_counts != -Inf){
       from_one <- max(max_group_count, nationality_counts)
     }else if(max_group_count != -Inf){
       from_one <- max_group_count
     }else if(nationality_counts != -Inf){
       from_one <- nationality_counts
     }else {
       from_one <- 1
     }
     map_stat_before6 <- rbind(map_stat_before6, data.frame(team = team, played_on_map_before6 = played_on_map, proportion_before6 = played_on_map/maps_played_1, hhi_before6 = hhi, num_maps_played_before6 = maps_played_1, from_one = from_one, shifts = ctr, players_ammount = tot_players, average_age = average_age, max_age_delta, not_playing_del_map))
   }
   cat("Stat for 6-months period is counted \n")
   #Далее собираем все полученные датафреймы в один + считаем новые метрики отношений столбцов
   if(nrow(teams_perf_after1) > 0 & nrow(teams_perf_before1) > 0){
     merged_data <- merge(teams_perf_after1, teams_perf_before1, by = "team", all = TRUE)
     
     delta1 <- transform(merged_data, change_kd1 = kd.x / kd.y, kd_after1 = kd.x, kd_before1 = kd.y, rating2.0_after1 = rating2.0.x, rating2.0_before1 = rating2.0.y, change_rating1 = rating2.0.x / rating2.0.y, kd_opponent_after1 = kd_opponent.x, kd_opponent_before1 = kd_opponent.y, rating2.0_opponent_after1 = rating2.0_opponent.x, rating2.0_opponent_before1 = rating2.0_opponent.y, rounds_after1 = rounds.x, rounds_before1 = rounds.y)
     delta1 <- delta1[c("team", "change_kd1", "change_rating1", "kd_after1", "kd_before1", "rating2.0_after1", "rating2.0_before1", "kd_opponent_after1", "kd_opponent_before1", "rating2.0_opponent_after1", "rating2.0_opponent_before1", "rounds_after1", "rounds_before1")]
   }else{
     delta1 <- data.frame(matrix(ncol = 3, nrow = 0))
   }
   
   if(nrow(teams_perf_after3) > 0 & nrow(teams_perf_before3) > 0){
     merged_data <- merge(teams_perf_after3, teams_perf_before3, by = "team", all = TRUE)
     
     delta3 <- transform(merged_data, change_kd3 = kd.x / kd.y, kd_after3 = kd.x, kd_before3 = kd.y, rating2.0_after3 = rating2.0.x, rating2.0_before3 = rating2.0.y, change_rating3 = rating2.0.x / rating2.0.y, kd_opponent_after3 = kd_opponent.x, kd_opponent_before3 = kd_opponent.y, rating2.0_opponent_after3 = rating2.0_opponent.x, rating2.0_opponent_before3 = rating2.0_opponent.y, rounds_after3 = rounds.x, rounds_before3 = rounds.y)
     delta3 <- delta3[c("team", "change_kd3", "change_rating3", "kd_after3", "kd_before3", "rating2.0_after3", "rating2.0_before3", "kd_opponent_after3", "kd_opponent_before3", "rating2.0_opponent_after3", "rating2.0_opponent_before3", "rounds_after3", "rounds_before3")]
     delta3 <- merge(delta3, teams_perf_after2, by = "team", all = TRUE)
     delta3$change_kd2_3 <- delta3$kd / delta3$kd_before3
     delta3$change_rating2.0_2_3 <- delta3$rating2.0 / delta3$rating2.0_before3 
     delta3$change_rounds_2_3 <- delta3$rounds / delta3$rounds_before3
     delta3 <- delta3[c("team", "change_kd3", "change_rating3", "kd_after3", "kd_before3", "rating2.0_after3", "rating2.0_before3",  "kd_opponent_after3", "kd_opponent_before3", "rating2.0_opponent_after3", "rating2.0_opponent_before3", "rounds_after3", "rounds_before3", "change_kd2_3", "change_rating2.0_2_3", "change_rounds_2_3")]
     delta3 <- merge(delta3, teams_perf_after4, by = "team", all = TRUE)
     delta3$change_kd4_3 <- delta3$kd / delta3$kd_before3
     delta3$change_rating2.0_4_3 <- delta3$rating2.0 / delta3$rating2.0_before3 
     delta3$change_rounds_4_3 <- delta3$rounds / delta3$rounds_before3
     delta3 <- delta3[c("team", "change_kd3", "change_rating3", "kd_after3", "kd_before3", "rating2.0_after3", "rating2.0_before3",  "kd_opponent_after3", "kd_opponent_before3", "rating2.0_opponent_after3", "rating2.0_opponent_before3", "rounds_after3", "rounds_before3", "change_kd2_3", "change_rating2.0_2_3", "change_rounds_2_3", "change_kd4_3", "change_rating2.0_4_3", "change_rounds_4_3")]
     delta3 <- merge(delta3, teams_perf_after5, by = "team", all = TRUE)
     delta3$change_kd5_3 <- delta3$kd / delta3$kd_before3
     delta3$change_rating2.0_5_3 <- delta3$rating2.0 / delta3$rating2.0_before3 
     delta3$change_rounds_5_3 <- delta3$rounds / delta3$rounds_before3
     delta3 <- delta3[c("team", "change_kd3", "change_rating3", "kd_after3", "kd_before3", "rating2.0_after3", "rating2.0_before3",  "kd_opponent_after3", "kd_opponent_before3", "rating2.0_opponent_after3", "rating2.0_opponent_before3", "rounds_after3", "rounds_before3", "change_kd2_3", "change_rating2.0_2_3", "change_rounds_2_3",  "change_kd4_3", "change_rating2.0_4_3", "change_rounds_4_3",  "change_kd5_3", "change_rating2.0_5_3", "change_rounds_5_3")]
   }else{
     delta3 <- data.frame(matrix(ncol = 3, nrow = 0))
   }
   
   if(nrow(teams_perf_after6) > 0 & nrow(teams_perf_before6) > 0){
     merged_data <- merge(teams_perf_after6, teams_perf_before6, by = "team", all = TRUE)
     
     delta6 <- transform(merged_data, change_kd6 = kd.x / kd.y, kd_after6 = kd.x, kd_before6 = kd.y, rating2.0_after6 = rating2.0.x, rating2.0_before6 = rating2.0.y, change_rating6 = rating2.0.x / rating2.0.y, kd_opponent_after6 = kd_opponent.x, kd_opponent_before6 = kd_opponent.y, rating2.0_opponent_after6 = rating2.0_opponent.x, rating2.0_opponent_before6 = rating2.0_opponent.y, rounds_after6 = rounds.x, rounds_before6 = rounds.y)
     delta6 <- delta6[c("team", "average_rank", "team_rank", "change_kd6", "change_rating6", "kd_after6", "kd_before6", "rating2.0_after6", "rating2.0_before6",  "kd_opponent_after6", "kd_opponent_before6", "rating2.0_opponent_after6", "rating2.0_opponent_before6", "rounds_after6", "rounds_before6")]
     delta <- merge(merge(merge(merge(merge(merge(merge(merge(delta1, delta3, by = "team", all = TRUE), delta6, by = "team", all = TRUE), map_stat_after1, by = "team", all = TRUE), map_stat_before1, by = "team", all = TRUE), map_stat_after3, by = "team", all = TRUE), map_stat_before3, by = "team", all = TRUE), map_stat_after6, by = "team", all = TRUE), map_stat_before6, by = "team", all = TRUE)
     delta$map <- map_name
     delta$isdeleted <- shock_type
     
     last_col_index <- ncol(delta)
     delta <- delta[, c(last_col_index, 1:(last_col_index-1))]
     delta <- delta[, c(last_col_index, 1:(last_col_index-1))]
     resilience_table <- rbind(resilience_table, delta)
    }
 }
}

