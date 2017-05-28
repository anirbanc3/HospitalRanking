#suppressWarnings()
best <- function(state, problem) {
    library("readr")
    outcome <<- read_csv("/home/anirban727/Downloads/outcome-of-care-measures.csv")
    list_of_state <- lapply(outcome$State, unique)
    list_of_state <- (unique(list_of_state))
    flag_1 = 0
    for (i in 1:54) {
      if (state == list_of_state[i]) {
        flag_1 = 1
#        print(paste("your option:",state))
      }
    }
    if (flag_1 == 0)
      print("invalid state")

    disease <- c("heart attack", "heart failure", "pneumonia")
    flag_2 = 0
    for (i in 1:3) {
      if (problem == disease[i]) {
        flag_2 = 1
#        print(paste("your option:",problem))
      }
    }
    if (flag_2 == 0)
      print("invalid outcome")

    if (flag_1 == 1 && flag_2 == 1)
      compute(state, problem)
}

compute <- function(state, problem) {
  grouped <- split(outcome , outcome$State)
  gr_state <- data.frame(grouped[state])

  if (problem == "heart attack") {
    mortality_rate <- gr_state[,11]
    mortality_rate <- as.numeric(subset(mortality_rate, mortality_rate != "Not Available"))
    min_rate <- min(mortality_rate)
    best_hos_row <- subset(gr_state, as.numeric(gr_state[,11]) == min_rate)
    best_hos <- best_hos_row[,2]
    print(best_hos)
  }
  if (problem == "heart failure") {
    mortality_rate <- gr_state[,17]
    mortality_rate <- as.numeric(subset(mortality_rate, mortality_rate != "Not Available"))
    min_rate <- min(mortality_rate)
    best_hos_row <- subset(gr_state, as.numeric(gr_state[,17]) == min_rate)
    best_hos <- best_hos_row[,2]
    print(best_hos)
  }
  if (problem == "pneumonia") {
    mortality_rate <- gr_state[,23]
    mortality_rate <- as.numeric(subset(mortality_rate, mortality_rate != "Not Available"))
    min_rate <- min(mortality_rate)
    best_hos_row <- subset(gr_state, as.numeric(gr_state[,23]) == min_rate)
    best_hos <- best_hos_row[,2]
    return(best_hos)
  }
 }












