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
  labs(x = "List", y = "Typicality Rating") +
  theme_classic()  # little variance between lists

p1/p2

# random intercept ID
m0 <- lmer(TY ~ 1 + (1|ID), data = df)
# von Hand
var_intercepts <- sd(unlist(coef(m0)))^2
var_residuals  <- sd(residuals(m0))^2
print(ICC <- var_intercepts / (var_intercepts + var_residuals))
# mit lmer
print(s0 <- summary(m0))
s0$varcor$ID[1] / (s0$varcor$ID[1] + s0$sigma^2)

# random intercept list
m0 <- lmer(TY ~ 1 + (1|list), data = df)
# von Hand
var_intercepts <- sd(unlist(coef(m0)))^2
var_residuals  <- sd(residuals(m0))^2
print(ICC <- var_intercepts / (var_intercepts + var_residuals))
# mit lmer
print(s0 <- summary(m0))
#print(s0$varcor$stim_category[1] / (s0$varcor$stim_category[1] + s0$sigma^2))

# random intercept: does not converge
m2 <- lmer(TY ~ condition + 
             (1+condition|ID) + (1+condition|list), data = df)

## use ID as random effect (list does not cause variance)
mc11 <- lmer(TY ~ 1 + condition + 
              (1|ID), data = df)
# lmerTest implements Kenward-Roger and Satterthwaite approximations for df (thus for p)
print(s11 <- summary(mc11))

# or model comparison; mind to set REML = FALSE
mc1 <- lmer(TY ~ 1 + condition + 
              (1|ID), data = df, REML = FALSE)
mc2 <- lmer(TY ~ 1 + 
              (1|ID), data = df, REML = FALSE)
anova(mc1, mc2)

# "conditionstatisch" means "statisch" relative to "dynamisch"
# i.e. statisch less typical than dynamisch by 0.17 points
m_dyn <- s11$coefficients['(Intercept)','Estimate']
diff_dyn_sta <- s11$coefficients['conditionstatisch','Estimate']
m_sta <- m_dyn + diff_dyn_sta
cat(paste0('Typicality dynamisch: ', round(m_dyn,3), '; statisch: ', round(m_sta, 3)))

### ===========
# sensitivity analysis
df_dyn = df %>% filter(condition == "dynamisch")
df_sta = df %>% filter(condition == "statisch")

df %>% 
  filter(condition == 'statisch') %>%
  summarize(n = n(), .by = c(ID, list)) %>%
  summarize(n=n(), .by = list) 
# jede ID hat nur eine Liste gemacht

persons   <- unique(df_sta$ID)
lists     <- unique(df_sta$list)
n_persons <- length(persons)
n_lists   <- length(lists)

lists_per_person <- c(1, 2, 3, 4)
pct_per_nlists   <- c(10, 30, 30, 30) # ca. 90 percent lists repeated
pct_per_nlists   <- c(90, 3, 3, 4)    # ca. 10 percent lists repeated
pct_per_nlists   <- c(50, 16, 16, 17) # ca. 50 percent lists repeated
adj_pct_per_list <- pct_per_nlists / lists_per_person # percent subjects
n_per_nlist      <- round(n_persons * adj_pct_per_list / 100) # number subjects

pctpn <- rbind(c(10, 30, 30, 30),
               c(90, 3, 3, 4),
               c(50, 16, 16, 17))
out <- matrix(numeric(length(pctpn)), dim(pctpn))/0
for (nn in 1:dim(pctpn)[1]){
  adj_pct_per_list <- pctpn[nn,] / lists_per_person # percent subjects
  n_per_nlist      <- round(n_persons * adj_pct_per_list / 100) # number subjects
  
  # correct numbers if necessary
  difference <- n_persons - sum(n_per_nlist * lists_per_person)
  if (difference != 0) {
    adjust_idx <- which(difference == lists_per_person) # use group 1:4
    n_per_nlist[adjust_idx] <- n_per_nlist[adjust_idx] + 
      (difference / lists_per_person[adjust_idx])
  }
  out[nn, ] <- n_per_nlist
}

results = list(3)

for (ww in 1:dim(out)[1]){

n_random = matrix(numeric(5*100)/0, 100, 5)

for (perm_run in 1:dim(n_random)[1]){

unique_id_list <- df_sta %>% 
  summarize(n = n(), .by = c(ID, list)) %>%
  arrange(sample(1:nrow(unique_id_list))) # shuffle rows

df_dummy <- df_sta  
uid <- unique_id_list
for (k in seq(4,2,-1)){   # from 4:1:1
  #nvp <- n_per_nlist[k]
  nvp <- out[ww, k]
  
    for (j in 1:nvp){
      lpp <- sample(lists_per_person, k)
      indx <- sort(map_dbl(lpp, 
                       function(x) min(which(uid$list %in% x))))
      #old_labels <- as.character(unique(df_dummy[
                            #df_dummy$ID %in% uid$ID[indx],]$ID))
      old_labels <- as.character(unique(uid$ID[indx]))
      new_labels <- rep(paste0('A', k, j), k)
      recode_map <- set_names(old_labels, new_labels)
      df_dummy   <- df_dummy %>% 
                    mutate(ID = fct_recode(ID, !!!recode_map))
      uid        <- uid[-indx,]
      
}
}
df_all   <- rbind(df_dyn, df_dummy)
ri_model <- lmer(TY ~ 1 + condition + 
               (1|ID), data = df_all)
mm = coef(summary(ri_model))
n_random[perm_run, ] = mm['conditionstatisch',]
}
colnames(n_random) <- colnames(mm)
results[[ww]] = n_random
}


unique_id_list[sample(1:nrow(unique_id_list)),]
set.seed(12)

# shuffle persons, assign lists
shuffled_persons <- sample(persons)
########## HIER WEITER machen
n_lists_per_person <- rep(seq_along(n_per_list), times = n_per_list)
# check
data.frame(shuffled_persons, n_lists_per_person)

# identify IDs with given list 
df_sta %>% 

# draw lists per person, convert R lists to data frame
for (j in 1:length(shuffled_persons)){
  person         <- shuffled_persons[j]
  n_assigned     <- n_lists_per_person[j]
  assigned_lists <- sample(lists, n_assigned)
  df_sta %>% group_by(ID) %>% mutate(pair = list %in% assigned_lists)
  df_sta[df_sta$ID == person & df_sta$list %in% assigned_lists, ]
}
  )
shuffled_df_sta <- bind_rows(result_aslist) # from list of dfs to one df


# ?? df_sta hat 3456 rows, shuffled sta nur 2528
shuffled_df <- rbind(df_dyn, shuffled_df_sta)



### binary data, accuracy
# random intercept
bm1 <- glmer(cat_true ~ condition + 
             (1|ID) , family = binomial, data = df)
print(bs1 <- summary(bm1))

sta_iV_dyn <- bs1$coefficients['conditionstatisch','Estimate']
cat(paste0('OR statisch i. V. zu dynamisch: ', round(exp(sta_iV_dyn),3)))
cat(paste0('OR dynamisch i. V. zu statisch: ', 1/round(exp(sta_iV_dyn),3)))

# https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html
# kein REML bei binomial
