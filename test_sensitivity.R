df

# zähle fälle
df %>% summarize(n = n(), .by = c(ID, list, condition)) %>%
  arrange(ID, n) %>%
  print(., n=140)

n_dynamisch <- df %>% summarize(n = n(), .by = c(ID, list, condition)) %>%
  filter(condition == 'dynamisch') %>%
  nrow()

# schätze rm aus Bedingung "dynamisch"
# welcher Anteil hat mehr als eine Liste gemacht
frac_dynamisch <- df %>% 
  filter(condition == 'dynamisch') %>%
  summarize(n = n(), .by = c(ID)) %>%
  mutate(n_lists = n / 54) %>%
  summarize(n=n(), .by = n_lists) %>%
  mutate(frc = n * n_lists / n_dynamisch) %>%
  arrange(n_lists)
print(frac_dynamisch)

# kontrolliere ob statisch immer nur eine Liste
frac_statisch <- df %>% 
  filter(condition == 'statisch') %>%
  summarize(n = n(), .by = c(ID)) %>%
  mutate(n_lists = n / 54) %>%
  summarize(n=n(), .by = n_lists)
print(frac_statisch)
n_statisch = frac_statisch$n

stat_id <- df %>% 
  filter(condition == 'statisch') %>%
  select(ID) %>%
  pull() %>%
  unique()

# wähle xy % der Teilnehmer aus, die jeweils 1 bis 4 Listen bearbeitet haben
nlists <- c(1, 2, 3, 4)
props  <- cbind(c(15, 15, 20, 50),
                c(50, 20, 15, 15),
                c(5, 5,   45, 45),
                c(45, 45, 5, 5),
                c(25, 25, 25, 25))
propdf <- as_tibble(
  as.data.frame(cbind(nlists, props),
                col.names = c('nlists', 
                              'V1', 'V2', 'V3', 'V4', 'V5')))
floor(propdf$V2/100*n_statisch) # n Personen

stat_id()


# simuliere rm für statisch
rm_prop <- 0.2 # prop repeated measures
n_rm    <- length(unique(df$ID))
#n_B <- sum(df$condition == "B")
n_paired <- floor(n_rm * rm_prop)

df_sim <- df %>%
  group_by(ID, list) %>%
  mutate(pair_id = c(1:n_paired, rep(NA, n() - n_paired))) %>%
  ungroup()