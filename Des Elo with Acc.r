library(readxl)

# Specify the file path (or file name if it's in the working directory)
file_path <- file_path <- "C:/Users/verch/OneDrive/Documents/r test.xlsx"


# Read the Excel file into R
data <- read_excel(file_path)


# Cargar paquete dplyr
library(dplyr)

# Convertir valores en la columna WIN según las especificaciones
data$WIN <- ifelse(data$WIN == "Agent", "win", "lose")

# Inicializar Elo scores
agent_elos <- setNames(rep(1500, length(unique(data$Agent))), unique(data$Agent))
scenario_elos <- setNames(rep(1500, length(unique(data$Scenario))), unique(data$Scenario))

# Inicializar columnas
data$agent_pre_game_elo <- NA
data$scenario_pre_game_elo <- NA
data$agent_win_probability <- NA
data$scenario_win_probability <- NA
data$agent_points_winnable <- NA
data$agent_points_at_risk <- NA
data$scenario_points_winnable <- NA
data$scenario_points_at_risk <- NA
data$agent_post_game_elo <- NA
data$scenario_post_game_elo <- NA

# Función para calcular la probabilidad de ganar
win_probability <- function(ra, rb) {
  return(1 / (1 + 10 ^ ((rb - ra) / 400)))
}

# Bucle para actualizar los Elo scores
for (i in 1:nrow(data)) {
  agent <- data$Agent[i]
  scenario <- data$Scenario[i]
  result <- ifelse(data$WIN[i] == "win", 1, 0)
  
  agent_elo <- agent_elos[agent]
  scenario_elo <- scenario_elos[scenario]
  
  # Asignar pre-game Elo scores
  data$agent_pre_game_elo[i] <- agent_elo
  data$scenario_pre_game_elo[i] <- scenario_elo
  
  # Calcular probabilidades de ganar
  agent_prob <- win_probability(agent_elo, scenario_elo)
  scenario_prob <- 1 - agent_prob
  
  data$agent_win_probability[i] <- agent_prob
  data$scenario_win_probability[i] <- scenario_prob
  
  # Calcular puntos en juego
  k_factor <- 32
  agent_points_winnable <- k_factor * (1 - agent_prob)
  agent_points_at_risk <- k_factor * agent_prob
  scenario_points_winnable <- k_factor * (1 - scenario_prob)
  scenario_points_at_risk <- k_factor * scenario_prob
  
  data$agent_points_winnable[i] <- agent_points_winnable
  data$agent_points_at_risk[i] <- agent_points_at_risk
  data$scenario_points_winnable[i] <- scenario_points_winnable
  data$scenario_points_at_risk[i] <- scenario_points_at_risk
  
  # Actualizar Elo scores
  new_agent_elo <- agent_elo + agent_points_winnable * result - agent_points_at_risk * (1 - result)
  new_scenario_elo <- scenario_elo + scenario_points_winnable * (1 - result) - scenario_points_at_risk * result
  
  agent_elos[agent] <- new_agent_elo
  scenario_elos[scenario] <- new_scenario_elo
  
  # Asignar post-game Elo scores
  data$agent_post_game_elo[i] <- new_agent_elo
  data$scenario_post_game_elo[i] <- new_scenario_elo
  
  # Asignar resultado a NPS_result
  data$NPS_result[i] <- result
  
  # Calculo de metricas de accuracy - MARKER 1: Added accuracy metrics calculation
  win_prob_agent <- agent_prob
  win_prob_scenario <- scenario_prob
  points_winnable_agent <- agent_points_winnable
  points_winnable_scenario <- scenario_points_winnable
  points_at_risk_agent <- agent_points_at_risk
  points_at_risk_scenario <- scenario_points_at_risk
  
  data[i, 'Agent Pre-game ELO'] <- agent_elo
  data[i, 'Scenario Pre-game ELO'] <- scenario_elo
  data[i, 'Agent Win Probability'] <- win_prob_agent
  data[i, 'Scenario Win Probability'] <- win_prob_scenario
  data[i, 'Agent Post-game ELO'] <- new_agent_elo
  data[i, 'Scenario Post-game ELO'] <- new_scenario_elo
  data[i, 'Agent points winnable'] <- points_winnable_agent
  data[i, 'Scenario points winnable'] <- points_winnable_scenario
  data[i, 'Agent points at risk'] <- points_at_risk_agent
  data[i, 'Scenario points at risk'] <- points_at_risk_scenario
}

# Opcional: remover la columna NPS_result si deseas
data <- data[, !names(data) %in% "NPS_result"]

# Conteo de accuracy
accuracy_counts <- data.frame('Predicted_Percent' = seq(0, 90, by = 10), 'Predicted' = 0, 'Actual' = 0)

for (i in 1:nrow(data)) {
  predicted_percent <- round(data$`Agent Win Probability`[i] * 100)
  actual_winner <- ifelse(data$WIN[i] == "win", "Agent", "Scenario")
  
  bin_index <- floor(predicted_percent / 10)
  accuracy_counts$Predicted[bin_index] <- 1 + accuracy_counts$Predicted[bin_index]
  
  if ((predicted_percent >= 50 && actual_winner == 'Agent') || (predicted_percent < 50 && actual_winner == 'Scenario')) {
    accuracy_counts$Actual[bin_index] <- 1 + accuracy_counts$Actual[bin_index]
  }
}

# Actual Accuracy
accuracy_counts$Actual_Accuracy <- accuracy_counts$Actual / accuracy_counts$Predicted

# Display 
print("Predicted vs Actual Accuracy:")
print(accuracy_counts)

# Plotteo
predicted_percentages <- accuracy_counts$Predicted_Percent
actual_accuracies <- accuracy_counts$Actual_Accuracy
barplot(actual_accuracies,
        names.arg = predicted_percentages,
        beside = TRUE,
        col = "blue",
        xlab = 'Predicted win %',
        ylab = 'Actual Accuracy',
        main = 'Prediction Accuracy Histogram',
        ylim = c(0, 1))
axis(1, at = seq(0, 90, by = 10), labels = seq(0, 90, by = 10))
grid()