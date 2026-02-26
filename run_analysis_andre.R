library(tidyverse)
library(lme4)
library(lmerTest)  # for p values; alternativ afex
library(patchwork) # plotting


#### read data -----
# filter out two participants with only NAs in AC and TY
df <- read_csv('./dyn_stat_actions.csv') %>%
  mutate(across(c(ID:sex, condition:AC), as_factor),
         cat_true = stim_category == AC) %>%
  filter_out((ID == "329" & case == "24") | (ID == "6-08")) 

#### plot random factor variance ----
p1 <- ggplot(df, aes(x = ID, y = TY)) +
  geom_boxplot() +
  labs(x = "Subjects", y = "Typicality Rating") +
  theme_classic()  # there is variance between subjects

p2 <- ggplot(df, aes(x = list, y = TY)) +
  geom_boxplot() +
  labs(x = "List", y = "Typicality Rating") +
  theme_classic()  # little variance between lists

p1/p2 

#### get ICC for random intercept "ID" and "list" ----
m0 <- lmer(TY ~ 1 + (1|ID), data = df)
print(s0 <- summary(m0))
print(paste0('ID ICC: ', round(s0$varcor$ID[1] / (s0$varcor$ID[1] + s0$sigma^2),3)))

# random intercept list: causes only minimal variance
m0 <- lmer(TY ~ 1 + (1|list), data = df)
print(s0 <- summary(m0))
print(paste0('list ICC: ', round(s0$varcor$list[1] / (s0$varcor$list[1] + s0$sigma^2),3)))

### LMM with dv "TY", fixed effect "condition", random effect "ID" ----
mc11 <- lmer(TY ~ 1 + condition + 
              (1|ID), data = df)
print(s11 <- summary(mc11))
m_dyn <- s11$coefficients['(Intercept)','Estimate']
diff_dyn_sta <- s11$coefficients['conditionstatisch','Estimate']
m_sta <- m_dyn + diff_dyn_sta
cat(paste0('Typicality dynamisch: ', round(m_dyn,3), '; statisch: ', round(m_sta, 3)))

### GLMM with dv accuracy, , fixed effect "condition", random effect "ID" ----
bm1 <- glmer(cat_true ~ condition + 
               (1|ID) , family = binomial, data = df)
print(bs1 <- summary(bm1))
sta_iV_dyn <- bs1$coefficients['conditionstatisch','Estimate']
cat(paste0('OR statisch i. V. zu dynamisch: ', round(exp(sta_iV_dyn),3)))
cat(paste0('OR dynamisch i. V. zu statisch: ', round(1/exp(sta_iV_dyn),3)))

### sensitivity analysis ===========
# prepare randomization
df_dyn = df %>% filter(condition == "dynamisch")
df_sta = df %>% filter(condition == "statisch")

persons   <- unique(df_sta$ID)
lists     <- unique(df_sta$list)
n_persons <- length(persons)
n_lists   <- length(lists)

# compute n subjects with n lists 
lists_per_person <- c(1, 2, 3, 4)
pctpn <- rbind(c(90, 3, 3, 4),    # ca. 10 percent lists repeated measures
               c(50, 16, 16, 17), # ca. 50 percent lists repeated measures
               c(10, 30, 30, 30)) # ca. 90 percent lists repeated measures
out <- matrix(numeric(length(pctpn)), dim(pctpn))/0
for (nn in 1:dim(pctpn)[1]){
  adj_pct_per_list <- pctpn[nn,] / lists_per_person # percent subjects
  n_per_nlist      <- round(n_persons * adj_pct_per_list / 100) # number subjects
  difference <- n_persons - sum(n_per_nlist * lists_per_person)
  if (difference != 0) {
    adjust_idx <- which(difference == lists_per_person) # use group 1:4
    n_per_nlist[adjust_idx] <- n_per_nlist[adjust_idx] + 
      (difference / lists_per_person[adjust_idx])
  }
  out[nn, ] <- n_per_nlist
}

# randomization loop
# pre-allocate results
N_LOOPS = 100
results_TY = list(3)
results_AC = list(3)

for (ww in 1:dim(out)[1]){

TY_random = matrix(numeric(5 * N_LOOPS)/0, N_LOOPS, 5)
AC_random = matrix(numeric(4 * N_LOOPS)/0, N_LOOPS, 4)

for (perm_run in 1:dim(TY_random)[1]){

unique_id_list <- df_sta %>% 
  summarize(n = n(), .by = c(ID, list)) %>%
  arrange(sample(1:nrow(.))) # shuffle rows

for (k in seq(4,2,-1)){   # from 4:1:1
  df_dummy <- df_sta  
  uid      <- unique_id_list
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
TY_random[perm_run, ] = mm['conditionstatisch',]

bm       <- glmer(cat_true ~ condition + 
               (1|ID) , family = binomial, data = df_all)
bmm = coef(summary(bm))
AC_random[perm_run, ] = bmm['conditionstatisch',]

}
colnames(TY_random) <- colnames(mm)
results_TY[[ww]] = as.data.frame(TY_random)
colnames(AC_random) <- colnames(bmm)
results_AC[[ww]] = as.data.frame(AC_random)
}

list_rbind(results_TY, names_to = 'nr') %>%
  as_tibble() %>% 
  rename(p = `Pr(>|t|)`,
         t = `t value`,
         SE = `Std. Error`) %>%
  summarize(m = mean(p), .by = nr) %>%
  mutate(fracRepeated = 100 - pctpn[,1])


list_rbind(results_AC, names_to = 'nr') %>%
  as_tibble() %>% 
  rename(p = `Pr(>|z|)`,
         t = `z value`,
         SE = `Std. Error`) %>%
  summarize(m = mean(p), .by = nr) %>%
  mutate(fracRepeated = 100 - pctpn[,1])

