library(rvest)
library(httr)
library(RSelenium)
setwd("/Users/tim17/Desktop/CS_parsed")
df <- data.frame(matrix(ncol = 37, nrow = 0))
colnames(df) <- c("team_1", "team_2", "map", "score_t_1", "score_t_2")

#Параметр till - число страниц с результатами матчей на HLTV, include_player_links - опционально добавляет ссылки на игроков
till <- 774
include_player_links <- FALSE
url <- "https://www.hltv.org/results"
#использовал несколько прокси, чтобы реже падало. сейчас они уже неактуальны, если понадобится запускать код еще раз, в следующей строке необходимо прописать работающие прокси (или просто удалить их, но тогда парситься будет дольше)
proxies <- c("user74496:l03teh@45.85.64.198:2866", "user74496:l03teh@45.89.71.181:2866", "user74496:l03teh@45.88.208.168:2866", "user74496:l03teh@45.88.211.4:2866", "user74496:l03teh@45.88.211.155:2866", "user74496:l03teh@193.111.152.129:8999", "user74496:l03teh@185.161.210.173:8999", "user74496:l03teh@185.20.187.108:8999
")
headers <- c('User-Agent' = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3')
proxy <- sample(proxies, size = 1)
config <- set_config(use_proxy(proxy))


#Параметры парсинга - ссылки и сами матчи
parse_links <- FALSE
parse_matches <- TRUE


if(parse_links){
  links <- new.env()
  html_content <- read_html(url, headers = headers, config = config)
  
  results_table <- html_content %>% html_nodes("div.result-con") 
  
  links <- results_table %>% html_node("a") %>% html_attr("href")
  
  prefix <- "https://www.hltv.org/"
  links <- paste(prefix, links, sep = "")
  links_buff <- ""
  
  for (i in  1:till){
    #считываем все ссылки со страницы и записываем в массив links
    Sys.sleep(0.3 + runif(1)*0.7)
    url <- paste0("https://www.hltv.org/results?offset=", as.character(i*100))
    proxy <- sample(proxies, size = 1)
    
    html_content <- read_html(url, headers = headers, config = config)
    
    results_table <- html_content %>% html_nodes("div.result-con") 
    
    links_buff <- results_table %>% html_node("a") %>% html_attr("href")
    
    prefix <- "https://www.hltv.org/"
    links_buff <- paste(prefix, links_buff, sep = "")
    links <- c(links, links_buff)
    cat("Links parsing in progress - ", i, "/", till, "\n")
  }
  
  links <- links[-1]
}
if(parse_matches){
  #итерируемся по массиву ссылок
  for (i in 1:length(links)) {
    #немного рандомных задержек, чтобы реже падало из-за отказа сервера HLTV
    if(i%%500 == 0){
      Sys.sleep(20 + runif(1)*20)
    }
    if(runif(1) < 0.03){
      Sys.sleep(10 + runif(1)*5 + runif(1) * runif(1) * 5)
    }
    cat("Mathes parsing in progress - ", i, "/", length(links), "\n")
    url <- links[i]
    proxy <- sample(proxies, size = 1)
    config <- set_config(use_proxy(proxy))
    html_content <- read_html(url, headers = headers, config = config)
    Sys.sleep(0.7 + runif(1)*1.1)
    
    teams <- html_content %>% html_nodes("div.teamName") %>% html_text()
    teams <- teams[1:2]
    score <- html_content %>% html_nodes(".results-team-score") %>% html_text()
    if(length(score) > 0){
    maps_played <- html_content %>% html_nodes("div.mapname") %>% html_text()
    num_maps = length(maps_played)
    
    arr <- html_content %>% 
      html_nodes(sprintf("div.lineups div.players td.player a")) %>% 
      html_attr("href")
    team1_players <- arr[1:5]
    team2_players <- arr[11:15]
    prize <- html_content %>% html_nodes("body > div.bgPadding > div.widthControl > div:nth-child(2) > div.leftCol > div > div > div:nth-child(1) > div > div > div > div.matchSidebarEventDataWrapper > div > div.matchSidebarData") %>% html_text()
    LAN <- html_content %>% html_nodes("body > div.bgPadding > div.widthControl > div:nth-child(2) > div.contentCol > div.match-page > div.standard-box.teamsBox > div.timeAndEvent > div.event.text-ellipsis") %>% html_text()
    score_t_1 <- score[seq(from = 1, to = length(score) - 1, by = 2)]
    score_t_2 <- score[seq(from = 2, to = length(score), by = 2)]
    isdefault <- FALSE
    defaultnum <- 0
    plus <- 0
    for (i in 1:num_maps){
      if(maps_played[i] == "Default"){
        defaultnum <- i
        isdefault <- TRUE
        plus <- 1
      }
    }
    #костыль, чтобы не падало на матчах, где у одной команды есть очко по дефолту
    if(num_maps == 5 & length(score_t_1) < 5 + plus){
      num_maps <- num_maps - 1
      maps_played <- maps_played[-5]
    }
    if(num_maps == 4 & length(score_t_1) < 4 + plus){
      num_maps <- num_maps - 1
      maps_played <- maps_played[-4]
    }
    if(num_maps == 3 & length(score_t_1) < 3 + plus){
      num_maps <- num_maps - 1
      maps_played <- maps_played[-3]
    }
    if(isdefault){
      num_maps <- num_maps - 1
      score_t_1 <- score_t_1[-defaultnum]
      score_t_2 <- score_t_2[-defaultnum]
      maps_played <- maps_played[-defaultnum]
    }
    team_links <- html_content %>% html_nodes(".teamName") %>%  html_attr("href")
    team_links <- strsplit(team_links, " ")
    team_links <- unique(team_links)
    link_1 <- team_links[[2]]
    link_2 <- team_links[[3]]
    prefix <- "https://www.hltv.org"
    link_1 <- paste(prefix, link_1, sep = "")
    link_2 <- paste(prefix, link_2, sep = "")
    url_1 <- link_1
    proxy <- sample(proxies, size = 1)
    config <- set_config(use_proxy(proxy))
    Sys.sleep(0.5)
    html_content_1 <- read_html(url_1, headers = headers, config = config)
    sum <- 0
    #отсюда и до следующего комментария бесполезный код
    for(i in 1:5){
      selector <- paste0("#rosterBox > div.playersBox-wrapper > table > tbody > tr:nth-child(", i, ") > td:nth-child(3) > div")
      a <- html_content_1 %>% html_nodes(selector) %>%html_text
      res <- strsplit(a, " ")
      month <- 0
      if(length(a) > 0){
        if(res[[1]][2] == "months" | res[[1]][2] == "month"){
          months <- as.numeric(res[[1]][1])
        }else{
          months <- as.numeric(res[[1]][1]) * 12 + as.numeric(gsub("[^[:digit:]]", "", res[[1]][2]))
        }
        sum <- sum + months
        if(i == 5){
          min_t1 <- months
        }
      }else{
        min_t1 <- 0
      }
    }
    average_roster_t1 <- sum / 5
    
    url_2 <- link_2
    proxy <- sample(proxies, size = 1)
    config <- set_config(use_proxy(proxy))
    Sys.sleep(0.5)
    html_content_2 <- read_html(url_2, headers = headers, config = config)
    sum <- 0
    for(i in 1:5){
      selector <- paste0("#rosterBox > div.playersBox-wrapper > table > tbody > tr:nth-child(", i, ") > td:nth-child(3) > div")
      a <- html_content_2 %>% html_nodes(selector) %>%html_text
      res <- strsplit(a, " ")
      month <- 0
      if(length(a) > 0){
        if(res[[1]][2] == "months" | res[[1]][2] == "month"){
          months <- as.numeric(res[[1]][1])
        }else{
          months <- as.numeric(res[[1]][1]) * 12 + as.numeric(gsub("[^[:digit:]]", "", res[[1]][2]))
        }
        sum <- sum + months
        if(i == 5){
          min_t2 <- months
        }
      }else{
        min_t2 <- 0
      }
    }
    average_roster_t2 <- sum / 5
    #конец бесполезного кода
    #далее работа с таблицей результатов. поскольку извлечь ее получилось только строкой, пришлось разбивать ее на массив по пробелам и далее втсавлять кучу костылей, исправляющих двойные названия команд и двойные фамилии
    res_table_s <- html_content %>% html_nodes("table.totalstats") %>% html_text()
    res_table_s <- gsub("\\s{2,}", " ", res_table_s)
    res_table <- strsplit(res_table_s, " ")
    counter <- length(score_t_1[score_t_1 != "-"])
    skip <- FALSE
    if(length(res_table)/2-1 < counter){
      skip <- TRUE
    }
    #KD pos нужен для того, чтобы код работал для названия команд с любым количеством слов. если их 2 и более все позиции в массиве сдвигаются вправо
    if((length(res_table) > 0) & !skip){
      KD_pos <- which(res_table[[1]] == "K-D", arr.ind = TRUE)
      KD_pos2 <- which(res_table[[2]] == "K-D", arr.ind = TRUE)
      # Решаем проблему с двойными именами и проблему с отсутствием имен
      if(length(res_table) > 3){
        for (i in 3:4){
          if(i%%2 == 1){
            k <- KD_pos
          }else{
            k <- KD_pos2
          }
          #тут мы проверяем, что на нужном нам месте стоит рейтинг 2.0 (при срезе его получаем точку). если нет - значит у игрока двойная фамилия и для корректной обработки в дальнейшем мы ее убираем(точнее убираем ее 2 часть)
          for(j in 1:3){
            if(length(res_table[[i]]) > k+13){
              if(!(substring(res_table[[i]][k+13], 2, 2) == ".")){
                res_table[[i]] <- res_table[[i]][-(k+9)]
              }
            }
          }
          for(j in 1:3){
            if(length(res_table[[i]]) > k+22){
              if(!(substring(res_table[[i]][k+22], 2, 2) == ".")){
                res_table[[i]] <- res_table[[i]][-(k+18)]
              }
            }
          }
          for(j in 1:3){
            if(length(res_table[[i]]) > k+31){
              if(!(substring(res_table[[i]][k+31], 2, 2) == ".")){
                res_table[[i]] <- res_table[[i]][-(k+27)]
              }
            }
          }
          for(j in 1:3){
            if(length(res_table[[i]]) > k+40){
              if(!(substring(res_table[[i]][k+40], 2, 2) == ".")){
                res_table[[i]] <- res_table[[i]][-(k+36)]
              }
            }
          }
          for(j in 1:3){
            if(length(res_table[[i]]) > k+49){
              if(!(substring(res_table[[i]][k+49], 2, 2) == ".")){
                res_table[[i]] <- res_table[[i]][-(k+45)]
              }
            }
          }
        }
        #далее берем нужные нам элементы из массива результатов
        m1_t1_player1 <- res_table[[3]][KD_pos+6]
        m1_t1_player1_2.0 <- res_table[[3]][KD_pos+13]
        m1_t1_player1_K_D <- res_table[[3]][KD_pos+9]
        m1_t1_player2 <- res_table[[3]][KD_pos+17] 
        m1_t1_player2_2.0 <- res_table[[3]][KD_pos+22]
        m1_t1_player2_K_D <- res_table[[3]][KD_pos+18]
        m1_t1_player3 <- res_table[[3]][KD_pos+24] 
        m1_t1_player3_2.0 <- res_table[[3]][KD_pos+31]
        m1_t1_player3_K_D <- res_table[[3]][KD_pos+27]
        m1_t1_player4 <- res_table[[3]][KD_pos+33] 
        m1_t1_player4_2.0 <- res_table[[3]][KD_pos+40]
        m1_t1_player4_K_D <- res_table[[3]][KD_pos+36]
        m1_t1_player5 <- res_table[[3]][KD_pos+42] 
        m1_t1_player5_2.0 <- res_table[[3]][KD_pos+49]
        m1_t1_player5_K_D <- res_table[[3]][KD_pos+45]
        
        m1_t2_player1 <- res_table[[4]][KD_pos2+6]
        m1_t2_player1_2.0 <- res_table[[4]][KD_pos2+13]
        m1_t2_player1_K_D <- res_table[[4]][KD_pos2+9]
        m1_t2_player2 <- res_table[[4]][KD_pos2+17] 
        m1_t2_player2_2.0 <- res_table[[4]][KD_pos2+22]
        m1_t2_player2_K_D <- res_table[[4]][KD_pos2+18]
        m1_t2_player3 <- res_table[[4]][KD_pos2+24] 
        m1_t2_player3_2.0 <- res_table[[4]][KD_pos2+31]
        m1_t2_player3_K_D <- res_table[[4]][KD_pos2+27]
        m1_t2_player4 <- res_table[[4]][KD_pos2+33] 
        m1_t2_player4_2.0 <- res_table[[4]][KD_pos2+40]
        m1_t2_player4_K_D <- res_table[[4]][KD_pos2+36]
        m1_t2_player5 <- res_table[[4]][KD_pos2+42] 
        m1_t2_player5_2.0 <- res_table[[4]][KD_pos2+49]
        m1_t2_player5_K_D <- res_table[[4]][KD_pos2+45]
        
        perf <- data.frame(m1_t1_player1, m1_t1_player1_2.0, m1_t1_player1_K_D, m1_t1_player2, m1_t1_player2_2.0, m1_t1_player2_K_D, m1_t1_player3, m1_t1_player3_2.0, m1_t1_player3_K_D, m1_t1_player4, m1_t1_player4_2.0, m1_t1_player4_K_D, m1_t1_player5, m1_t1_player5_2.0, m1_t1_player5_K_D, m1_t2_player1, m1_t2_player1_2.0, m1_t2_player1_K_D, m1_t2_player2, m1_t2_player2_2.0, m1_t2_player2_K_D, m1_t2_player3, m1_t2_player3_2.0, m1_t2_player3_K_D, m1_t2_player4, m1_t2_player4_2.0, m1_t2_player4_K_D, m1_t2_player5, m1_t2_player5_2.0, m1_t2_player5_K_D)
      }
      if(num_maps > 1){
        if(length(res_table) > 5){
          for (i in 5:6){
            if(i%%2 == 1){
              k = KD_pos
            }else{
              k = KD_pos2
            }
            for(j in 1:3){
              if(length(res_table[[i]]) > k+13){
                if(!(substring(res_table[[i]][k+13], 2, 2) == ".")){
                  res_table[[i]] <- res_table[[i]][-(k+9)]
                }
              }
            }
            for(j in 1:3){
              if(length(res_table[[i]]) > k+22){
                if(!(substring(res_table[[i]][k+22], 2, 2) == ".")){
                  res_table[[i]] <- res_table[[i]][-(k+18)]
                }
              }
            }
            for(j in 1:3){
              if(length(res_table[[i]]) > k+31){
                if(!(substring(res_table[[i]][k+31], 2, 2) == ".")){
                  res_table[[i]] <- res_table[[i]][-(k+27)]
                }
              }
            }
            for(j in 1:3){
              if(length(res_table[[i]]) > k+40){
                if(!(substring(res_table[[i]][k+40], 2, 2) == ".")){
                  res_table[[i]] <- res_table[[i]][-(k+36)]
                }
              }
            }
            for(j in 1:3){
              if(length(res_table[[i]]) > k+49){
                if(!(substring(res_table[[i]][k+49], 2, 2) == ".")){
                  res_table[[i]] <- res_table[[i]][-(k+45)]
                }
              }
            }
          }
          m2_t1_player1 <- res_table[[5]][KD_pos+6]
          m2_t1_player1_2.0 <- res_table[[5]][KD_pos+13]
          m2_t1_player1_K_D <- res_table[[5]][KD_pos+9]
          m2_t1_player2 <- res_table[[5]][KD_pos+17] 
          m2_t1_player2_2.0 <- res_table[[5]][KD_pos+22]
          m2_t1_player2_K_D <- res_table[[5]][KD_pos+18]
          m2_t1_player3 <- res_table[[5]][KD_pos+24] 
          m2_t1_player3_2.0 <- res_table[[5]][KD_pos+31]
          m2_t1_player3_K_D <- res_table[[5]][KD_pos+27]
          m2_t1_player4 <- res_table[[5]][KD_pos+33] 
          m2_t1_player4_2.0 <- res_table[[5]][KD_pos+40]
          m2_t1_player4_K_D <- res_table[[5]][KD_pos+36]
          m2_t1_player5 <- res_table[[5]][KD_pos+42] 
          m2_t1_player5_2.0 <- res_table[[5]][KD_pos+49]
          m2_t1_player5_K_D <- res_table[[5]][KD_pos+45]
          
          m2_t2_player1 <- res_table[[6]][KD_pos2+6]
          m2_t2_player1_2.0 <- res_table[[6]][KD_pos2+13]
          m2_t2_player1_K_D <- res_table[[6]][KD_pos2+9]
          m2_t2_player2 <- res_table[[6]][KD_pos2+17] 
          m2_t2_player2_2.0 <- res_table[[6]][KD_pos2+22]
          m2_t2_player2_K_D <- res_table[[6]][KD_pos2+18]
          m2_t2_player3 <- res_table[[6]][KD_pos2+24] 
          m2_t2_player3_2.0 <- res_table[[6]][KD_pos2+31]
          m2_t2_player3_K_D <- res_table[[6]][KD_pos2+27]
          m2_t2_player4 <- res_table[[6]][KD_pos2+33] 
          m2_t2_player4_2.0 <- res_table[[6]][KD_pos2+40]
          m2_t2_player4_K_D <- res_table[[6]][KD_pos2+36]
          m2_t2_player5 <- res_table[[6]][KD_pos2+42] 
          m2_t2_player5_2.0 <- res_table[[6]][KD_pos2+49]
          m2_t2_player5_K_D <- res_table[[6]][KD_pos2+45]
          
          perf_m2 <- c(m2_t1_player1, m2_t1_player1_2.0, m2_t1_player1_K_D, m2_t1_player2, m2_t1_player2_2.0, m2_t1_player2_K_D, m2_t1_player3, m2_t1_player3_2.0, m2_t1_player3_K_D, m2_t1_player4, m2_t1_player4_2.0, m2_t1_player4_K_D, m2_t1_player5, m2_t1_player5_2.0, m2_t1_player5_K_D, m2_t2_player1, m2_t2_player1_2.0, m2_t2_player1_K_D, m2_t2_player2, m2_t2_player2_2.0, m2_t2_player2_K_D, m2_t2_player3, m2_t2_player3_2.0, m2_t2_player3_K_D, m2_t2_player4, m2_t2_player4_2.0, m2_t2_player4_K_D, m2_t2_player5, m2_t2_player5_2.0, m2_t2_player5_K_D)
          perf <- rbind(perf, perf_m2)
        }
        if(num_maps > 2 & length(score_t_1) >= 3){
          if(length(res_table) > 7){
            if(score_t_1[3] != "-" & !is.na(score_t_1[3]) & defaultnum!=3){
              for (i in 7:8){
                if(i%%2 == 1){
                  k = KD_pos
                }else{
                  k = KD_pos2
                }
                for(j in 1:3){
                  if(length(res_table[[i]]) > k+13){
                    if(!(substring(res_table[[i]][k+13], 2, 2) == ".")){
                      res_table[[i]] <- res_table[[i]][-(k+9)]
                    }
                  }
                }
                for(j in 1:3){
                  if(length(res_table[[i]]) > k+22){
                    if(!(substring(res_table[[i]][k+22], 2, 2) == ".")){
                      res_table[[i]] <- res_table[[i]][-(k+18)]
                    }
                  }
                }
                for(j in 1:3){
                  if(length(res_table[[i]]) > k+31){
                    if(!(substring(res_table[[i]][k+31], 2, 2) == ".")){
                      res_table[[i]] <- res_table[[i]][-(k+27)]
                    }
                  }
                }
                for(j in 1:3){
                  if(length(res_table[[i]]) > k+40){
                    if(!(substring(res_table[[i]][k+40], 2, 2) == ".")){
                      res_table[[i]] <- res_table[[i]][-(k+36)]
                    }
                  }
                }
                for(j in 1:3){
                  if(length(res_table[[i]]) > k+49){
                    if(!(substring(res_table[[i]][k+49], 2, 2) == ".")){
                      res_table[[i]] <- res_table[[i]][-(k+45)]
                    }
                  }
                }
              }
              m3_t1_player1 <- res_table[[7]][KD_pos+6]
              m3_t1_player1_2.0 <- res_table[[7]][KD_pos+13]
              m3_t1_player1_K_D <- res_table[[7]][KD_pos+9]
              m3_t1_player2 <- res_table[[7]][KD_pos+17] 
              m3_t1_player2_2.0 <- res_table[[7]][KD_pos+22]
              m3_t1_player2_K_D <- res_table[[7]][KD_pos+18]
              m3_t1_player3 <- res_table[[7]][KD_pos+24] 
              m3_t1_player3_2.0 <- res_table[[7]][KD_pos+31]
              m3_t1_player3_K_D <- res_table[[7]][KD_pos+27]
              m3_t1_player4 <- res_table[[7]][KD_pos+33] 
              m3_t1_player4_2.0 <- res_table[[7]][KD_pos+40]
              m3_t1_player4_K_D <- res_table[[7]][KD_pos+36]
              m3_t1_player5 <- res_table[[7]][KD_pos+42] 
              m3_t1_player5_2.0 <- res_table[[7]][KD_pos+49]
              m3_t1_player5_K_D <- res_table[[7]][KD_pos+45]
              
              m3_t2_player1 <- res_table[[8]][KD_pos2+6]
              m3_t2_player1_2.0 <- res_table[[8]][KD_pos2+13]
              m3_t2_player1_K_D <- res_table[[8]][KD_pos2+9]
              m3_t2_player2 <- res_table[[8]][KD_pos2+17] 
              m3_t2_player2_2.0 <- res_table[[8]][KD_pos2+22]
              m3_t2_player2_K_D <- res_table[[8]][KD_pos2+18]
              m3_t2_player3 <- res_table[[8]][KD_pos2+24] 
              m3_t2_player3_2.0 <- res_table[[8]][KD_pos2+31]
              m3_t2_player3_K_D <- res_table[[8]][KD_pos2+27]
              m3_t2_player4 <- res_table[[8]][KD_pos2+33] 
              m3_t2_player4_2.0 <- res_table[[8]][KD_pos2+40]
              m3_t2_player4_K_D <- res_table[[8]][KD_pos2+36]
              m3_t2_player5 <- res_table[[8]][KD_pos2+42] 
              m3_t2_player5_2.0 <- res_table[[8]][KD_pos2+49]
              m3_t2_player5_K_D <- res_table[[8]][KD_pos2+45]
              perf_m3 <- c(m3_t1_player1, m3_t1_player1_2.0, m3_t1_player1_K_D, m3_t1_player2, m3_t1_player2_2.0, m3_t1_player2_K_D, m3_t1_player3, m3_t1_player3_2.0, m3_t1_player3_K_D, m3_t1_player4, m3_t1_player4_2.0, m3_t1_player4_K_D, m3_t1_player5, m3_t1_player5_2.0, m3_t1_player5_K_D, m3_t2_player1, m3_t2_player1_2.0, m3_t2_player1_K_D, m3_t2_player2, m3_t2_player2_2.0, m3_t2_player2_K_D, m3_t2_player3, m3_t2_player3_2.0, m3_t2_player3_K_D, m3_t2_player4, m3_t2_player4_2.0, m3_t2_player4_K_D, m3_t2_player5, m3_t2_player5_2.0, m3_t2_player5_K_D)
              perf <- rbind(perf, perf_m3)
            }
          }
          if(defaultnum != 3){
            if(score_t_1[3] == "-"){
              perf_m3 <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
              perf <- rbind(perf, perf_m3)
            }
          }
        }
      }
      if(num_maps > 3){
        if(length(res_table) > 9){
          if(score_t_1[4] != "-" & !is.na(score_t_1[4]) & defaultnum != 4){
            for (i in 9:10){
              if(i%%2 == 1){
                k = KD_pos
              }else{
                k = KD_pos2
              }
              for(j in 1:3){
                if(length(res_table[[i]]) > k+13){
                  if(!(substring(res_table[[i]][k+13], 2, 2) == ".")){
                    res_table[[i]] <- res_table[[i]][-(k+9)]
                  }
                }
              }
              for(j in 1:3){
                if(length(res_table[[i]]) > k+22){
                  if(!(substring(res_table[[i]][k+22], 2, 2) == ".")){
                    res_table[[i]] <- res_table[[i]][-(k+18)]
                  }
                }
              }
              for(j in 1:3){
                if(length(res_table[[i]]) > k+31){
                  if(!(substring(res_table[[i]][k+31], 2, 2) == ".")){
                    res_table[[i]] <- res_table[[i]][-(k+27)]
                  }
                }
              }
              for(j in 1:3){
                if(length(res_table[[i]]) > k+40){
                  if(!(substring(res_table[[i]][k+40], 2, 2) == ".")){
                    res_table[[i]] <- res_table[[i]][-(k+36)]
                  }
                }
              }
              for(j in 1:3){
                if(length(res_table[[i]]) > k+49){
                  if(!(substring(res_table[[i]][k+49], 2, 2) == ".")){
                    res_table[[i]] <- res_table[[i]][-(k+45)]
                  }
                }
              }
            }
            m4_t1_player1 <- res_table[[9]][KD_pos+6]
            m4_t1_player1_2.0 <- res_table[[9]][KD_pos+13]
            m4_t1_player1_K_D <- res_table[[9]][KD_pos+9]
            m4_t1_player2 <- res_table[[9]][KD_pos+17] 
            m4_t1_player2_2.0 <- res_table[[9]][KD_pos+22]
            m4_t1_player2_K_D <- res_table[[9]][KD_pos+18]
            m4_t1_player3 <- res_table[[9]][KD_pos+24] 
            m4_t1_player3_2.0 <- res_table[[9]][KD_pos+31]
            m4_t1_player3_K_D <- res_table[[9]][KD_pos+27]
            m4_t1_player4 <- res_table[[9]][KD_pos+33] 
            m4_t1_player4_2.0 <- res_table[[9]][KD_pos+40]
            m4_t1_player4_K_D <- res_table[[9]][KD_pos+36]
            m4_t1_player5 <- res_table[[9]][KD_pos+42] 
            m4_t1_player5_2.0 <- res_table[[9]][KD_pos+49]
            m4_t1_player5_K_D <- res_table[[9]][KD_pos+45]
            
            m4_t2_player1 <- res_table[[10]][KD_pos2+6]
            m4_t2_player1_2.0 <- res_table[[10]][KD_pos2+13]
            m4_t2_player1_K_D <- res_table[[10]][KD_pos2+9]
            m4_t2_player2 <- res_table[[10]][KD_pos2+17] 
            m4_t2_player2_2.0 <- res_table[[10]][KD_pos2+22]
            m4_t2_player2_K_D <- res_table[[10]][KD_pos2+18]
            m4_t2_player3 <- res_table[[10]][KD_pos2+24] 
            m4_t2_player3_2.0 <- res_table[[10]][KD_pos2+31]
            m4_t2_player3_K_D <- res_table[[10]][KD_pos2+27]
            m4_t2_player4 <- res_table[[10]][KD_pos2+33] 
            m4_t2_player4_2.0 <- res_table[[10]][KD_pos2+40]
            m4_t2_player4_K_D <- res_table[[10]][KD_pos2+36]
            m4_t2_player5 <- res_table[[10]][KD_pos2+42] 
            m4_t2_player5_2.0 <- res_table[[10]][KD_pos2+49]
            m4_t2_player5_K_D <- res_table[[10]][KD_pos2+45]
            
            perf_m4 <- c(m4_t1_player1, m4_t1_player1_2.0, m4_t1_player1_K_D, m4_t1_player2, m4_t1_player2_2.0, m4_t1_player2_K_D, m4_t1_player3, m4_t1_player3_2.0, m4_t1_player3_K_D, m4_t1_player4, m4_t1_player4_2.0, m4_t1_player4_K_D, m4_t1_player5, m4_t1_player5_2.0, m4_t1_player5_K_D, m4_t2_player1, m4_t2_player1_2.0, m4_t2_player1_K_D, m4_t2_player2, m4_t2_player2_2.0, m4_t2_player2_K_D, m4_t2_player3, m4_t2_player3_2.0, m4_t2_player3_K_D, m4_t2_player4, m4_t2_player4_2.0, m4_t2_player4_K_D, m4_t2_player5, m4_t2_player5_2.0, m4_t2_player5_K_D)
            perf <- rbind(perf, perf_m4)
          }
        }
        if(defaultnum != 4){
          if(score_t_1[4] == "-" | is.na(score_t_1[4])){
            perf_m4 <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
            perf <- rbind(perf, perf_m4)
          }
        }
        if(num_maps > 4){
          if(length(res_table) > 11){
            if(score_t_2[5]!="-" & !is.na(score_t_2[5]) & defaultnum!=5){
              for (i in 11:12){
                if(i%%2 == 1){
                  k = KD_pos
                }else{
                  k = KD_pos2
                }
                for(j in 1:3){
                  if(length(res_table[[i]]) > k+13){
                    if(!(substring(res_table[[i]][k+13], 2, 2) == ".")){
                      res_table[[i]] <- res_table[[i]][-(k+9)]
                    }
                  }
                }
                for(j in 1:3){
                  if(length(res_table[[i]]) > k+22){
                    if(!(substring(res_table[[i]][k+22], 2, 2) == ".")){
                      res_table[[i]] <- res_table[[i]][-(k+18)]
                    }
                  }
                }
                for(j in 1:3){
                  if(length(res_table[[i]]) > k+31){
                    if(!(substring(res_table[[i]][k+31], 2, 2) == ".")){
                      res_table[[i]] <- res_table[[i]][-(k+27)]
                    }
                  }
                }
                for(j in 1:3){
                  if(length(res_table[[i]]) > k+40){
                    if(!(substring(res_table[[i]][k+40], 2, 2) == ".")){
                      res_table[[i]] <- res_table[[i]][-(k+36)]
                    }
                  }
                }
                for(j in 1:3){
                  if(length(res_table[[i]]) > k+49){
                    if(!(substring(res_table[[i]][k+49], 2, 2) == ".")){
                      res_table[[i]] <- res_table[[i]][-(k+45)]
                    }
                  }
                }
              }
              m5_t1_player1 <- res_table[[11]][KD_pos+6]
              m5_t1_player1_2.0 <- res_table[[11]][KD_pos+13]
              m5_t1_player1_K_D <- res_table[[11]][KD_pos+9]
              m5_t1_player2 <- res_table[[11]][KD_pos+17] 
              m5_t1_player2_2.0 <- res_table[[11]][KD_pos+22]
              m5_t1_player2_K_D <- res_table[[11]][KD_pos+18]
              m5_t1_player3 <- res_table[[11]][KD_pos+24] 
              m5_t1_player3_2.0 <- res_table[[11]][KD_pos+31]
              m5_t1_player3_K_D <- res_table[[11]][KD_pos+27]
              m5_t1_player4 <- res_table[[11]][KD_pos+33] 
              m5_t1_player4_2.0 <- res_table[[11]][KD_pos+40]
              m5_t1_player4_K_D <- res_table[[11]][KD_pos+36]
              m5_t1_player5 <- res_table[[11]][KD_pos+42] 
              m5_t1_player5_2.0 <- res_table[[11]][KD_pos+49]
              m5_t1_player5_K_D <- res_table[[11]][KD_pos+45]
              
              m5_t2_player1 <- res_table[[12]][KD_pos2+6]
              m5_t2_player1_2.0 <- res_table[[12]][KD_pos2+13]
              m5_t2_player1_K_D <- res_table[[12]][KD_pos2+9]
              m5_t2_player2 <- res_table[[12]][KD_pos2+17] 
              m5_t2_player2_2.0 <- res_table[[12]][KD_pos2+22]
              m5_t2_player2_K_D <- res_table[[12]][KD_pos2+18]
              m5_t2_player3 <- res_table[[12]][KD_pos2+24] 
              m5_t2_player3_2.0 <- res_table[[12]][KD_pos2+31]
              m5_t2_player3_K_D <- res_table[[12]][KD_pos2+27]
              m5_t2_player4 <- res_table[[12]][KD_pos2+33] 
              m5_t2_player4_2.0 <- res_table[[12]][KD_pos2+40]
              m5_t2_player4_K_D <- res_table[[12]][KD_pos2+36]
              m5_t2_player5 <- res_table[[12]][KD_pos2+42] 
              m5_t2_player5_2.0 <- res_table[[12]][KD_pos2+49]
              m5_t2_player5_K_D <- res_table[[12]][KD_pos2+45]
              
              perf_m5 <- c(m5_t1_player1, m5_t1_player1_2.0, m5_t1_player1_K_D, m5_t1_player2, m5_t1_player2_2.0, m5_t1_player2_K_D, m5_t1_player3, m5_t1_player3_2.0, m5_t1_player3_K_D, m5_t1_player4, m5_t1_player4_2.0, m5_t1_player4_K_D, m5_t1_player5, m5_t1_player5_2.0, m5_t1_player5_K_D, m5_t2_player1, m5_t2_player1_2.0, m5_t2_player1_K_D, m5_t2_player2, m5_t2_player2_2.0, m5_t2_player2_K_D, m5_t2_player3, m5_t2_player3_2.0, m5_t2_player3_K_D, m5_t2_player4, m5_t2_player4_2.0, m5_t2_player4_K_D, m5_t2_player5, m5_t2_player5_2.0, m5_t2_player5_K_D)
              perf <- rbind(perf, perf_m5)
            }
          }
          if(defaultnum != 5){
            if((score_t_1[5] == "-" | is.na(score_t_1[5])) & !isdefault){
              perf_m5 <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
              perf <- rbind(perf, perf_m5)
            }
          }
        }
      }
      #ranks
      rank_1 <- html_content %>% html_nodes("#lineups > div > div:nth-child(1) > div.box-headline.flex-align-center > div.teamRanking") %>% html_text()
      if(length(rank_1 == 1)){
        rank_1 <- as.numeric(substring(strsplit(rank_1, " ")[[1]][3], 2))
      }else{
        rank_1 <- "-"
      }
      rank_2 <- html_content %>% html_nodes("#lineups > div > div:nth-child(3) > div.box-headline.flex-align-center > div.teamRanking > a") %>% html_text()
      if(length(rank_2 == 1)){
        rank_2 <- as.numeric(substring(strsplit(rank_2, " ")[[1]][3], 2))
      }else{
        rank_2 <- "-"
      }
      if(length(perf[[1]]) != length(maps_played)){
        maps_played <- maps_played[-length(maps_played)]
        score_t_1 <- score_t_1[-length(score_t_1)]
        score_t_2 <- score_t_2[-length(score_t_2)]
      }
      date <- html_content %>% html_nodes("body > div.bgPadding > div.widthControl > div:nth-child(2) > div.contentCol > div.match-page > div.standard-box.teamsBox > div.timeAndEvent > div.date") %>% html_text() %>% gsub("(\\d+)(st|nd|rd|th)", "\\1", .) %>% as.Date(format = "%d of %B %Y")
      if(length(maps_played) == length(score_t_1)){
        if(include_player_links){
          new_df <- data.frame(date = as.Date(date), team_1 = teams[1], team_2 = teams[2], map = maps_played, score_t1 = score_t_1, score_t2 = score_t_2, player1_t1 = team1_players[1], player2_t1 = team1_players[2], player3_t1 = team1_players[3], player4_t1 = team1_players[4], player5_t1 = team1_players[5], player1_t2 = team2_players[1], player2_t2 = team2_players[2], player3_t2 = team2_players[3], player4_t2 = team2_players[4], player5_t2 = team2_players[5], event = LAN[1], prize_pool = prize[1], rank_1 = rank_1, rank_2 = rank_2, min_t1 = min_t1, average_roster_t1 = average_roster_t1, min_t2 = min_t2, average_roster_t2 = average_roster_t2,  performance = perf)
        }else{
          new_df <- data.frame(date = as.Date(date), team_1 = teams[1], team_2 = teams[2], map = maps_played, score_t1 = score_t_1, score_t2 = score_t_2, event = LAN[1], prize_pool = prize[1], rank_1 = rank_1, rank_2 = rank_2, min_t1 = min_t1, average_roster_t1 = average_roster_t1, min_t2 = min_t2, average_roster_t2 = average_roster_t2,  performance = perf)
        }
        
        df <- rbind(df, new_df)
      }
    }
    }
  }
}
save(df,file="CS_parsed.Rda")

write.csv(df, file="CS_parsed.csv")

