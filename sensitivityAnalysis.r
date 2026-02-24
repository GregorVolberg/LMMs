sensitvity analysis
library(tidyverse)

# Funktion für verschiedene Paarungsszenarien
sensitivity_analysis <- function(df, pairing_prop) {
  n_A <- sum(df$condition == "A")
  n_B <- sum(df$condition == "B")
  n_paired <- floor(min(n_A, n_B) * pairing_prop)
  
  # Simuliere Paarungsstruktur
  df_sim <- df %>%
    group_by(condition) %>%
    mutate(pair_id = c(1:n_paired, rep(NA, n() - n_paired))) %>%
    ungroup()
  
  # Mixed model (wenn Paare vorhanden)
  if (n_paired > 0) {
    library(lme4)
    model <- lmer(value ~ condition + (1|pair_id), 
                  data = df_sim %>% filter(!is.na(pair_id)))
    return(summary(model)$coefficients)
  }
}

# Teste verschiedene Paarungsanteile
proportions <- seq(0, 1, by = 0.1)
results <- map_df(proportions, ~sensitivity_analysis(df, .x))