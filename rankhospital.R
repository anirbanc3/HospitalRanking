rm(list=ls())
rankhospital <- function(state, problem, num = "best") {
  library("readr")
  outcome <<- read_csv("/home/anirban727/Downloads/outcome-of-care-measures.csv")
  list_of_state <- lapply(outcome$State, unique)
  list_of_state <- (unique(list_of_state))
  flag_1 = 0
  for (i in 1:54) {
    if (state == list_of_state[i]) {
      flag_1 = 1
#              print(paste("your option:",state))
    }
  }
  if (flag_1 == 0)
    print("invalid state")
  
  disease <- c("heart attack", "heart failure", "pneumonia")
  flag_2 = 0
  for (i in 1:3) {
    if (problem == disease[i]) {
      flag_2 = 1
#              print(paste("your option:",problem))
    }
  }
  if (flag_2 == 0)
    print("invalid outcome")
  
  if (flag_1 == 1 && flag_2 == 1)
    compute_rank_state(state, problem, num)
}


compute_rank_state <- function(state, problem, num) {
  grouped <- split(outcome , outcome$State)
  gr_state <- data.frame(grouped[state])
  
  if (problem == "heart attack") {
    ha_outcome <- subset(gr_state, gr_state[, 11] != "Not Available")
    sorted_outcome_asc <- ha_outcome[order(as.numeric(ha_outcome[,11]),ha_outcome[,2]),]
    sorted_outcome_des <- ha_outcome[order(as.numeric(ha_outcome[,11]),ha_outcome[,2], decreasing = T),]
    
    nob <- length(ha_outcome[,2])
    if (is.numeric(num)) {
      if (num > nob) {
        return("NA")
      }
      else {
        result_set <- c(sorted_outcome_asc[num, 2])
      }
    }
    else {
      if (num == "best") {
        result_set <- c(sorted_outcome_asc[1,2])
      }
      else if (num == "worst") {
        result_set <- c(sorted_outcome_des[1,2])
      }
      else return("NA")
    }
  }
  
  if (problem == "heart failure") {
    ha_outcome <- subset(gr_state, gr_state[, 17] != "Not Available")
    sorted_outcome_asc <- ha_outcome[order(as.numeric(ha_outcome[,17]),ha_outcome[,2]),]
    sorted_outcome_des <- ha_outcome[order(as.numeric(ha_outcome[,17]),ha_outcome[,2], decreasing = T),]
    
    nob <- length(ha_outcome[,2])
    if (is.numeric(num)) {
      if (num > nob) {
        return("NA")
      }
      else {
        result_set <- c(sorted_outcome_asc[num, 2])
      }
    }
    else {
      if (num == "best") {
        result_set <- c(sorted_outcome_asc[1,2])
      }
      else if (num == "worst") {
        result_set <- c(sorted_outcome_des[1,2])
      }
      else return("NA")
    }
  }
  
  if (problem == "pneumonia") {
    ha_outcome <- subset(gr_state, gr_state[, 23] != "Not Available")
    sorted_outcome_asc <- ha_outcome[order(as.numeric(ha_outcome[,23]),ha_outcome[,2]),]
    sorted_outcome_des <- ha_outcome[order(as.numeric(ha_outcome[,23]),ha_outcome[,2], decreasing = T),]
    
    nob <- length(ha_outcome[,2])
    if (is.numeric(num)) {
      if (num > nob) {
        return("NA")
      }
      else {
        result_set <- c(sorted_outcome_asc[num, 2])
      }
    }
    else {
      if (num == "best") {
        result_set <- c(sorted_outcome_asc[1,2])
      }
      else if (num == "worst") {
        result_set <- c(sorted_outcome_des[1,2])
      }
      else return("NA")
    }
  }
  return(result_set)
}











