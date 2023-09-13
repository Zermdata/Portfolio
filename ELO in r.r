#Step 1
# Install and load the required library
install.packages("readxl")
library(readxl)

# Load data from Excel file
file_path <- "games.xlsx"
data <- read_excel(file_path)

# Display the loaded data
head(data)

#Step 2
# Initialize ELO Parameters
K <- 32  # K-factor for ELO calculation
initial_elo <- 1500  # Initial ELO rating for players

# Adjustable variables for Step 3
point_scale <- 20  # You can adjust this to match your game's point scale
scaling_factor <- 400  # Adjust this for sensitivity of rating changes

#Step 3
# Define ELO Calculation Functions
expected_win_probability <- function(rating_a, rating_b) {
  return (1 / (1 + 10^((rating_b - rating_a) / scaling_factor)))
}

update_elo <- function(rating_a, rating_b, score_a) {
  expected_a <- expected_win_probability(rating_a, rating_b)
  delta <- K * (score_a - expected_a)
  return (rating_a + delta)
}

calculate_metrics <- function(rating_a, rating_b) {
  win_prob_a <- expected_win_probability(rating_a, rating_b)
  win_prob_b <- 1 - win_prob_a
  points_winnable_a <- round(point_scale * win_prob_a)
  points_winnable_b <- round(point_scale * win_prob_b)
  points_at_risk_a <- point_scale - points_winnable_a
  points_at_risk_b <- point_scale - points_winnable_b
  return (list(win_prob_a, win_prob_b, points_winnable_a, points_winnable_b, points_at_risk_a, points_at_risk_b))
}


#Step 4
# Initialize ELO ratings
elo_ratings <- data.frame(Player = unique(c(data$Agent, data$Scenario)), Rating = initial_elo)

# Add columns for metrics
data$Agent_Pre_game_ELO <- NA
data$Scenario_Pre_game_ELO <- NA
data$Agent_Win_Probability <- NA
data$Scenario_Win_Probability <- NA
data$Agent_Post_game_ELO <- NA
data$Scenario_Post_game_ELO <- NA
data$Agent_Points_Winnable <- NA
data$Scenario_Points_Winnable <- NA
data$Agent_Points_At_Risk <- NA
data$Scenario_Points_At_Risk <- NA

for (i in 1:nrow(data)) {
  agent <- data$Agent[i]
  scenario <- data$Scenario[i]
  outcome <- data$Outcome[i]
  
  agent_elo <- elo_ratings[elo_ratings$Player == agent, "Rating"]
  scenario_elo <- elo_ratings[elo_ratings$Player == scenario, "Rating"]
  
  agent_pre_elo <- agent_elo
  scenario_pre_elo <- scenario_elo
  
  if (outcome == "Agent") {
    agent_score <- 1
    scenario_score <- 0
  } else if (outcome == "Scenario") {
    agent_score <- 0
    scenario_score <- 1
  } else {
    agent_score <- 0.5
    scenario_score <- 0.5
  }
  
  agent_elo <- update_elo(agent_elo, scenario_elo, agent_score)
  scenario_elo <- update_elo(scenario_elo, agent_elo, scenario_score)
  
  elo_ratings[elo_ratings$Player == agent, "Rating"] <- agent_elo
  elo_ratings[elo_ratings$Player == scenario, "Rating"] <- scenario_elo
  
  metrics <- calculate_metrics(agent_pre_elo, scenario_pre_elo)
  
  data$Agent_Pre_game_ELO[i] <- agent_pre_elo
  data$Scenario_Pre_game_ELO[i] <- scenario_pre_elo
  data$Agent_Win_Probability[i] <- metrics[[1]][1]
  data$Scenario_Win_Probability[i] <- metrics[[1]][2]
  data$Agent_Post_game_ELO[i] <- agent_elo
  data$Scenario_Post_game_ELO[i] <- scenario_elo
  data$Agent_Points_Winnable[i] <- metrics[[1]][3]
  data$Scenario_Points_Winnable[i] <- metrics[[1]][4]
  data$Agent_Points_At_Risk[i] <- metrics[[1]][5]
  data$Scenario_Points_At_Risk[i] <- metrics[[1]][6]
}

#Step 5
# Initialize accuracy counts
accuracy_counts <- data.frame(Predicted_Percent = seq(0, 90, by = 10), Predicted = rep(0, 10), Actual = rep(0, 10))

for (i in 1:nrow(data)) {
  predicted_percent <- round(data$Agent_Win_Probability[i] * 100)
  actual_winner <- data$Outcome[i]
  
  bin_index <- predicted_percent %/% 10 + 1
  accuracy_counts$Predicted[bin_index] <- accuracy_counts$Predicted[bin_index] + 1
  
  if (predicted_percent >= 50 && actual_winner == "Agent") {
    accuracy_counts$Actual[bin_index] <- accuracy_counts$Actual[bin_index] + 1
  } else if (predicted_percent < 50 && actual_winner == "Scenario") {
    accuracy_counts$Actual[bin_index] <- accuracy_counts$Actual[bin_index] + 1
  }
}

# Calculate and display actual accuracy percentages
accuracy_counts$Actual_Accuracy <- accuracy_counts$Actual / accuracy_counts$Predicted

print("Predicted vs. Actual Accuracy:")
print(accuracy_counts)


#Step 6
# Plot the histogram of predicted vs. actual accuracy
library(ggplot2)

ggplot(accuracy_counts, aes(x = Predicted_Percent, y = Actual_Accuracy)) +
  geom_bar(stat = "identity", position = "dodge", width = 8, fill = "blue") +
  labs(x = "Predicted Win Percentage", y = "Actual Accuracy") +
  ggtitle("Prediction Accuracy Histogram") +
  scale_x_continuous(breaks = seq(0, 90, by = 10)) +
  ylim(0, 1) +
  theme_minimal()
