library(tidyverse)
library(lme4)
library(patchwork)

# read data
# filter out two participants with only NAs in AC and TY
df <- read_csv('./dyn_stat_actions.csv') %>%
  mutate(across(c(ID:sex, condition:AC), as_factor),
         cat_true = stim_category == AC) %>%
  filter_out((ID == "329" & case == "24") | (ID == "6-08")) 


p1 <- ggplot(df, aes(x = ID, y = TY)) +
  geom_boxplot() +
  labs(x = "Subjects", y = "Typicality Rating") +
  theme_classic()  # there is variance between subjects

p2 <- ggplot(df, aes(x = stim_category, y = TY)) +
  geom_boxplot() +
  labs(x = "Stimulus category", y = "Typicality Rating") +
  theme_classic()  # there is variance between scenarios

p1/p2

# random intercept
m1 <- lmer(TY ~ condition + 
                 (1|ID) + (1|stim_category), data = df)
s1 <- summary(m1)
s1$varcor$ID[1] / (s1$varcor$ID[1] + s1$varcor$stim_category[1] + s1$sigma^2)
s1$varcor$stim_category[1] / (s1$varcor$ID[1] + s1$varcor$stim_category[1] + s1$sigma^2)

# model comparison
mc1 <- lmer(TY ~ 1 + condition + 
             (1|ID) + (1|stim_category), data = df, REML = FALSE)
mc2 <- lmer(TY ~ 1 + 
              (1|ID) + (1|stim_category), data = df, REML = FALSE)

anova(mc1, mc2)


### RI, RC

# random intercept: does not converge
m2 <- lmer(TY ~ condition + 
             (1+condition|ID) + (1+condition|stim_category), data = df)

# binary data, accuracy
# random intercept
bm1 <- glmer(cat_true ~ condition + 
             (1|ID) + (1|stim_category), family = binomial, data = df)
bs1 <- summary(bm1)


# model comparison
bm1 <- glmer(cat_true ~ 1+ condition + 
               (1|ID) + (1|stim_category), family = binomial, data = df)
bm2 <- glmer(cat_true ~ 1+ 
               (1|ID) + (1|stim_category), family = binomial, data = df)

anova(bm1, bm2)
https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html
# kein REML bei binomial
