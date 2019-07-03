read_projectdata <- function(){
  library(dplyr)
  library(readr)
  experimento = read_csv(here::here("data/experimento6.csv")) %>%
    mutate_all(funs(replace(., is.na(.), 0)))
  names(experimento)[2] <- "data"
  names(experimento)[3] <- "pensamento_extremapobreza"
  names(experimento)[4] <- "justodoar_extremapobreza"
  names(experimento)[5] <- "probabilidadedoar_extremapobreza"
  names(experimento)[6] <- "ouvidofalar_extremapobreza"
  names(experimento)[7] <- "pensamento_escola"
  names(experimento)[8] <- "justodoar_escola"
  names(experimento)[9] <- "probabilidadedoar_escola"
  names(experimento)[10] <- "ouvidofalar_escola"
  names(experimento)[11] <- "doacao"
  names(experimento)[12] <- "motivacao"
  write_csv(experimento, here::here("data/experimento.csv"))
  
}


calculate_statistic <- function(){
  library(broom)
  library(boot)
  read_projectdata()
  
  theta <- function(df, i){
    df <- df[i, ]
    return(mean(df$probabilidadedoar_extremapobreza))
  }
  
  booted <- boot(data = experimento, 
                statistic = theta, 
                R = 4000)
  ci = tidy(booted, 
            conf.level = .95,
            conf.method = "bca",
            conf.int = TRUE)
  
  theta2 <- function(df, i){
    df <- df[i, ]
    return(mean(df$probabilidadedoar_escola))
  }
  booted2 <- boot(data = experimento,
                  statistic = theta2,
                  R = 4000)
  ci2 = tidy(booted2,
            conf.level = .95,
            conf.method = "bca",
            conf.int = TRUE)
  estatisticas <- data.frame(narrativa = c('Extrema Pobreza', 'Escola'),
                          statistic = c(ci$statistic, ci2$statistic),
                          bias = c(ci$bias, ci2$bias),
                          std.error = c(ci$std.error, ci2$std.error),
                          conf.low = c(ci$conf.low, ci2$conf.low),
                          conf.high = c(ci$conf.high, ci2$conf.high))
  
  write_csv(estatisticas, here::here("data/estatisticas.csv"))
  estatisticas = read_csv(here::here("data/estatisticas.csv"))
}
