library(tidyverse)
library(lme4)


politeness <- read_csv("politeness_data.csv") %>%
  mutate(across(subject:attitude, as_factor))

# effect of politeness on voice pitch (Winter, 2018)
# subject   := subject ID
# gender    := gender of subject
# scenario  := 7 scenarios (e. g. asking for a favor, excusing for coming late)
# attitude  := polite or informal
# frequency := sound pitch. This is the dependent variable

p1 <- ggplot(politeness, aes(x = subject, y = frequency)) +
  geom_boxplot() +
  labs(x = "Subjects", y = "Frequency (Hz)") +
  theme_classic()  # there is variance between subjects

p2 <- ggplot(politeness, aes(x = scenario, y = frequency)) +
  geom_boxplot() +
  labs(x = "Scenario", y = "Frequency (Hz)") +
  theme_classic()  # there is variance between scenarios

library(patchwork)
p1/p2

# random intercept for subject
model1 <- lmer(frequency ~ attitude +
                          (1|subject), data=politeness)
s1 <- summary(model1)
3982 / (3982+851) # subject variability explains 82% of Variation
s1$varcor$subject[1] / (s1$varcor$subject[1] + s1$sigma^2)

# random effect for subject and scenarios
model2 <- lmer(frequency ~ attitude +
                 (1|subject) + (1|scenario), data=politeness)
s2     <- summary(model2)
# ICC
s2$varcor$scenario[1] / (s2$varcor$scenario[1] + s2$varcor$subject[1] + s2$sigma^2)
s2$varcor$subject[1]  / (s2$varcor$scenario[1] + s2$varcor$subject[1] + s2$sigma^2)

# another fixed effect
# random effect for subject and scenarios
model3 <- lmer(frequency ~ attitude + gender +
                 (1|subject) + (1|scenario), data=politeness)
s3     <- summary(model3)
# ICC
s3$varcor$scenario[1] / (s3$varcor$scenario[1] + s3$varcor$subject[1] + s3$sigma^2)
s3$varcor$subject[1]  / (s3$varcor$scenario[1] + s3$varcor$subject[1] + s3$sigma^2)

# significance test with model comparisons
#  mind to set REML = FALSE; since model tests via Deviance or AIC require ML estimation
# for gender
model_full <- lmer(frequency ~ attitude + gender +
                 (1|subject) + (1|scenario), data=politeness,
               REML = FALSE)
model_restricted <- lmer(frequency ~ attitude +
                 (1|subject) + (1|scenario), data=politeness,
               REML = FALSE)
anova(model_restricted, model_full)


## for interaction
model_full <- lmer(frequency ~ attitude*gender +
                     (1|subject) + (1|scenario), data=politeness,
                   REML = FALSE)
model_restricted <- lmer(frequency ~ gender + attitude +
                           (1|subject) + (1|scenario), data=politeness,
                         REML = FALSE)
anova(model_restricted, model_full)

## random slope
coef(model3) # different intercept, but always same slope
# change to random slope

rirs <- lmer(frequency ~ gender + attitude +
       (1+attitude|subject) + (1+attitude|scenario), data=politeness)



