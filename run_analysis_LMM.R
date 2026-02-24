library(tidyverse)
library(lme4)
library(lmerTest)  # for p values; alternativ afex
library(patchwork) # plotting


# read data; filter out two participants with only NAs in AC and TY
df <- read_csv('./dyn_stat_actions.csv') %>%
  mutate(across(c(ID:sex, condition:AC), as_factor),
         cat_true = stim_category == AC) %>%
  filter_out((ID == "329" & case == "24") | (ID == "6-08")) 

# variance between participants
p1 <- ggplot(df, aes(x = ID, y = TY)) +
  geom_boxplot() +
  labs(x = "Subjects", y = "Typicality Rating") +
  theme_classic()  # there is variance between subjects

# variance between lists
p2 <- ggplot(df, aes(x = list, y = TY)) +
  geom_boxplot() +
  labs(x = "Stimulus category", y = "Typicality Rating") +
  theme_classic()  # little variance between lists

p1/p2

# random intercept ID
m0 <- lmer(TY ~ 1 + (1|ID), data = df)
# von Hand
var_intercepts <- sd(unlist(coef(m0)))^2
var_residuals  <- sd(residuals(m0))^2
ICC <- var_intercepts / (var_intercepts + var_residuals)
# mit lmer
print(s0 <- summary(m0))
s0$varcor$ID[1] / (s0$varcor$ID[1] + s0$sigma^2)

# random intercept list
m0 <- lmer(TY ~ 1 + (1|list), data = df)
# von Hand
var_intercepts <- sd(unlist(coef(m0)))^2
var_residuals  <- sd(residuals(m0))^2
ICC <- var_intercepts / (var_intercepts + var_residuals)
# mit lmer
print(s0 <- summary(m0))
#print(s0$varcor$stim_category[1] / (s0$varcor$stim_category[1] + s0$sigma^2))

# random intercept: does not converge
m2 <- lmer(TY ~ condition + 
             (1+condition|ID) + (1+condition|list), data = df)

## use ID and list as random effects
mc11 <- lmer(TY ~ 1 + condition + 
              (1|ID) + (1|list), data = df)
# lmerTest implements Kenward-Roger and Satterthwaite approximations for df (thus for p)
print(s11 <- summary(mc11))

# or model comparison; mind to set REML = FALSE
mc1 <- lmer(TY ~ 1 + condition + 
              (1|ID) + (1|list), data = df, REML = FALSE)
mc2 <- lmer(TY ~ 1 + 
              (1|ID) + (1|list), data = df, REML = FALSE)
anova(mc1, mc2)

# "conditionstatisch" means "statisch" relative to "dynamisch"
# i.e. statisch less typical than dynamisch by 0.179 points
m_dyn <- s11$coefficients['(Intercept)','Estimate']
diff_dyn_sta <- s11$coefficients['conditionstatisch','Estimate']
m_sta <- m_dyn + diff_dyn_sta
cat(paste0('Typicality dynamisch: ', round(m_dyn,3), '; statisch: ', round(m_sta, 3)))


# sensitivity analysis




### binary data, accuracy
# random intercept
bm1 <- glmer(cat_true ~ condition + 
             (1|ID) + (1|list), family = binomial, data = df)
print(bs1 <- summary(bm1))

sta_iV_dyn <- bs1$coefficients['conditionstatisch','Estimate']
cat(paste0('OR statisch i. V. zu dynamisch: ', round(exp(sta_iV_dyn),3)))
cat(paste0('OR dynamisch i. V. zu statisch: ', 1/round(exp(sta_iV_dyn),3)))

# https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html
# kein REML bei binomial
