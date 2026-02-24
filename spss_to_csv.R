library(tidyverse)

###  functions 
get_file_list <- function(){
  rawpath <- "./raw/"
  file_list <- c(
    "Datensatz_Dynamisch_Liste1.sav",
    "Datensatz_Dynamisch_Liste2.sav",
    "Datensatz_Dynamisch_Liste3.sav",
    "Datensatz_Dynamisch_Liste4.sav",
    "Datensatz_Statisch_Liste1.sav",
    "Datensatz_Statisch_Liste2.sav",
    "Datensatz_Statisch_Liste3.sav",
    "Datensatz_Statisch_Liste4.sav"
    )
  return(paste0(rawpath, file_list))
}

make_df <- function(infile){
  tmp <- haven::read_spss(infile) 
  df  <- haven::read_spss(infile) %>%
    select(REF, CASE, DE01, age = any_of(c("DE09_01", "DE08_01")),
           acq_date = STARTED, AC01:AC54, TY01:TY54) %>%
    mutate(DE01 = haven::as_factor(DE01),
           nr = 1:nrow(.)) %>%
           #condition = as_factor(tolower(str_split_i(infile, '_',2)))) %>%
    pivot_longer(AC01:TY54, 
                 names_to = c(".value", "stim_nr"),
                 names_pattern = "(\\D+)(\\d+.*)") %>%
    mutate(stim_A = replace_values(stim_nr,
                                   from = str_pad(1:54, 2, pad = "0"), 
                                   to   = unname(map_vec(select(tmp, AC01:AC54), attr, "label"))),
           stim_T = replace_values(stim_nr,
                                   from = str_pad(1:54, 2, pad = "0"), 
                                   to   = unname(map_vec(select(tmp, TY01:TY54), attr, "label"))),
           cat_nr = str_split_i(stim_A, '_', 3),
           stim_category     = replace_values(cat_nr,
                                    from = as.character(unname(attributes(.$AC)$labels)), 
                                    to   = names(attributes(.$AC)$labels)),
           across(c(AC, stim_category, stim_A, stim_T),  as_factor),
           across(c(stim_nr, TY), as.numeric)) %>%
    select(nr, case = CASE, ref = REF, sex = DE01, age, acq_date, stim_nr, stim_category, stim_A, stim_T, AC, TY)
  return(df)
}  

### call functions
flist <- get_file_list()

df    <- map_df(flist, make_df, .id = "flist_index") %>%
  mutate(list = as_factor(replace_values(flist_index, from = unique(flist_index),
                               to = str_sub(flist, -5, -5))),
         condition = as_factor(tolower(replace_values(flist_index, from = unique(flist_index),
                                    to = str_split_i(flist, '_', 2)))),
         ID = case_when(
           ref == "" ~ paste0(flist_index, '-', str_pad(nr, 2, pad = "0")),
           TRUE ~ ref),
         ID = as_factor(ID)) %>%
  select(ID, list, case, sex:acq_date, condition, stim_nr:TY)

write_csv(df, './dyn_stat_actions.csv')

# sanity checks
nvp <- numeric(length(flist))
for (i in 1:length(flist)){
  tmp <- haven::read_spss(flist[i])
  cat(sprintf('\n%s\t%i participants', flist[i], nrow(tmp)))
  nvp[i] <- nrow(tmp)
}
cat(sprintf('\n%i participants overall\n', sum(nvp)))

df %>% mutate(IDplus = as_factor(paste0(ID, '_', list))) %>%
         select(IDplus) %>%
         group_by(IDplus) %>%
         tally() %>%
         select(n) %>%
  unique()
