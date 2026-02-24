library(tidyverse)

# Ziel: 
# (1) inferenzstatistische Auswertung allgemein zwischen der dynamischen zu der statischen Gruppe bzgl Accuracy und Typicality
# (2) Inferenzstatistik jeweils für die 72 statisch-dynamischen-Handlunsgpaare

# read data
# filter out two participants with only NAs in AC and TY
df <- read_csv('./dyn_stat_actions.csv') %>%
  mutate(across(c(ID:sex, condition:AC), as_factor)) %>%
  filter_out((ID == "329" & case == "24") | (ID == "6-08")) 

# zu (2): Der Vergleich statisch vs. dynamisch ist doch dann jeweils unabhängig, pro Bild, oder? Weil niemand sowohl statisch als auch dynamisch gemacht hat und auch keiner mehrmals pro Liste?
# df |> count(condition, list,stim_A) %>% filter(list==1)
# df %>% filter(list==1 & condition == 'dynamisch') %>% pull(ID) %>% unique() |> length()
# Liste 1: n = 18 pro Bild, unique(ID) = 18
# Liste 2: n = 17 pro Bild, unique(ID) = 17
# Liste 3: n = 17 pro Bild, unique(ID) = 17
# Liste 4: n = 17 pro Bild, unique(ID) = 17

# fisher's exact test on colums of tibbles
res_table <- df %>% 
  mutate(correct = (AC == stim_category)) %>%
  summarise(n_correct    = sum(correct), .by = c(condition, stim_category),
            n = n()) %>%
  pivot_wider(id_cols = stim_category,
              names_from = condition,
              values_from = c(n_correct, n)) %>%
  rename('dynamisch_success' = n_correct_dynamisch,
         'statisch_success' = n_correct_statisch) %>%
  mutate(dynamisch_failure = n_dynamisch - dynamisch_success,
         statisch_failure  = n_statisch - statisch_success,
         p_dynamisch = dynamisch_success/n_dynamisch,
         p_statisch = statisch_success/n_statisch) %>%
  rowwise() %>%
  mutate(
    test = list(run_fisher(dynamisch_success, dynamisch_failure, 
                           statisch_success, statisch_failure)),
    p_value = test$p.value,
    odds_ratio = test$estimate,
    m = mean(c_across(c(p_dynamisch, p_statisch)))) %>%
    ungroup() %>%
  pivot_longer(p_dynamisch:p_statisch, 
               names_to = c("condition"),
               values_to = "p") %>%
  select(stim_category, condition, p, m, p_value, odds_ratio) %>%
  mutate(condition = as_factor(condition),
         condition = fct_recode(condition, dynamisch = "p_dynamisch",
                                statisch  = "p_statisch"),
         p_fdr = p.adjust(p))
  

cbPalette <- c("#999999", "#E69F00", "#56B4E9", 
               "#009E73", "#F0E442", "#0072B2",
               "#D55E00", "#CC79A7")

ggplot(res_table, aes(x = p, 
                      y = reorder(stim_category, m, decreasing = TRUE),
                      group = condition,
                      fill = condition)) +
  geom_bar(stat = 'identity',
           position = "dodge") +
  scale_fill_manual(values = cbPalette) +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(0.9,0.95)) +
  labs(x = 'Proportion correct', y = 'Action category')
  
  
  

%>%
  select(-n_dynamisch)
    
  mutate(
    n = 54,
    test = pmap(
      list(dynamisch, statisch, n),
      ~prop.test(x = c(..1, ..2), n = c(..3, ..3))
    ),
    p_value   = map_dbl(test, "p.value"),
    statistic = map_dbl(test, "statistic"),
    df        = map_dbl(test, "parameter")) %>%
  rowwise() %>%
  mutate(m = mean(c_across(c(dynamisch, statisch)))) %>%
  ungroup %>%
  select(-test) %>%
  arrange(m)

## correction for multiple comparisons?




getchisq <- function(indf){
  prop.test(c(indf$dynamisch, indf$statisch), c(54, 54))
}

k = n_success %>% group_by(stim_category)

map_df(n_success, getchisq, .id = "flist_index")

  mutate(chisq = prop.test(c(dynamisch, statisch), c(54, 54))$statistic)

n_success %>% group_by(stim_category) %>%
prop.test(c(44, 42), c(54,54))

# proportion correct
prz <- df %>% mutate(IDplus = as_factor(paste0(ID, '_', list)),
              correct = AC == stim_category) %>%
  summarise(p_correct    = mean(correct), .by = c(IDplus, condition, stim_category)) %>%
  summarise(gavg_correct = mean(p_correct), .by = c(condition, stim_category)) %>%
  pivot_wider(id_cols = stim_category, names_from = condition, values_from = gavg_correct)

prz %>% arrange(dynamisch, statisch) 


# unbalanced design

# probability for group membership
# https://pmc.ncbi.nlm.nih.gov/articles/PMC8636443/