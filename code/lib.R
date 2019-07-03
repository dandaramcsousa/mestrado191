read_projectdata <- function(){
  library(dplyr)
  library(readr)
  experimento = read_csv(here::here("data/experimento6.csv"))
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

read_statistic <- function(){
  read_projectdata()
  
  statistic <- data.frame(narrative = c('Extrema Pobreza', 'Escola'),
                          mean_justodoar = c(mean(experimento$justodoar_extremapobreza), mean(experimento$justodoar_escola)),
                          sd_justodoar = c(sd(experimento$justodoar_extremapobreza), sd(experimento$justodoar_escola)),
                          min_justodoar = c(min(experimento$justodoar_extremapobreza), min(experimento$justodoar_escola)),
                          max_justodoar = c(max(experimento$justodoar_extremapobreza), max(experimento$justodoar_escola)),
                          mean_probdoar = c(mean(experimento$probabilidadedoar_extremapobreza), mean(experimento$probabilidadedoar_escola)),
                          sd_probdoar = c(sd(experimento$probabilidadedoar_extremapobreza), sd(experimento$probabilidadedoar_escola)),
                          min_probdoar = c(min(experimento$probabilidadedoar_extremapobreza, min(experimento$probabilidadedoar_escola))),
                          max_probdoar = c(max(experimento$probabilidadedoar_extremapobreza, max(experimento$probabilidadedoar_escola))),
                          n = c(NROW(experimento$justodoar_extremapobreza), NROW(experimento$justodoar_escola))
  )
  write_csv(statistic, here::here("data/statistic.csv"))
  statistic = read_csv(here::here("data/statistic.csv"))
}