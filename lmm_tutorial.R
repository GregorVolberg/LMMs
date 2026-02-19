library(tidyverse)
library(lme4)

politeness <- read_csv("politeness_data.csv")

# from B. Winter (2018)
politeness.model = lmer(frequency ~ attitude +
                          (1|subject) + (1|scenario), data=politeness)

# oder Brown (2021), hier Daten in OSF