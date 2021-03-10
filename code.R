### Man or Machine
### ---------------------------------------

## Libraries
library(dplyr)


## Read in the data
## Data from: https://www.kaggle.com/rohanrao/formula-1-world-championship-1950-2020
races <- read.csv("races.csv")
races$date <- as.Date(races$date)

results <- read.csv("results.csv")

standings <- read.csv("driver_standings.csv")

all_years <- 1950:2020


## Get champion driver of each season, and their team.
## ---------------------------------------------------------------------------
## The team of the champion driver is not always the constructors champion!
champion_drivers <- c()
champions_team <- c()

for (year in all_years){
  # look at who was at the top of the standings after the last race of the season
  relevant_races <- races[races$year == year, c("raceId", "date")]
  last_race_id <- relevant_races[which.max(relevant_races$date), "raceId"]
  last_standings <- standings[standings$raceId == last_race_id,]
  
  # champion and their team
  champion <- last_standings[which.min(last_standings$position), "driverId"]
  team <- results[(results$raceId %in% relevant_races$raceId) & (results$driverId == champion), "constructorId"][1]
  champion_drivers <- c(champion_drivers, champion) 
  champions_team <- c(champions_team, team)
}

champions_table <- data.frame(year = all_years, driverId = champion_drivers, constructorId = champions_team)


## Do we want to exclude DNF races in the following analyses?
## ----------------------------------------------------------
exclude_DNF = FALSE


## Calculate the effect of man vs machine for each year (using points)
## -------------------------------------------------------------------

effects <- data.frame(year = c(), effect = c())

for (year in all_years){
  relevant_races <- races[races$year == year, "raceId"]
  champion <- champions_table[champions_table$year == year, "driverId"]
  team <- champions_table[champions_table$year == year, "constructorId"]
  
  for (race in relevant_races){
    # get results for the race
    relevant_results <- results[results$raceId == race,]
    
    if(exclude_DNF){
      relevant_results <- relevant_results[relevant_results$position != "\\N",]
    }
    
    max_points <- max(relevant_results$points)
    
    # get points of champion and teammate(s)
    champ_points <- relevant_results[relevant_results$driverId == champion, "points"]
    teammate_points <- relevant_results[relevant_results$constructorId == team & 
                                        relevant_results$driverId != champion,
                                        "points"]
    
    if((length(champ_points) > 0) && (length(teammate_points) > 0)){
      eff <- (champ_points - mean(teammate_points))/max_points
      effects <- rbind(effects, c(year, eff))
    }
  }
}


## Summarize and Plot
## ------------------

names(effects) <- c("year", "effect")
mean_effects <- effects %>% group_by(year) %>% summarise(mean = mean(effect), var = var(effect))

plot(x = mean_effects$year, y = mean_effects$mean, ylim = c(-0.1, 1),
     ylab = "Mean Driver Influence", xlab = "Year")
abline(lm(mean_effects$mean ~ mean_effects$year), col = "red")
abline(lm(mean_effects$mean ~ mean_effects$year, weights = 1/mean_effects$var), col = "blue")
legend("topright", legend = c("LS", "weighted LS"), col = c("red", "blue"), lty = 1)

