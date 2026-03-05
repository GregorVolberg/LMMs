library(tidyverse)
library(mlmhelpr)

# the hsb data set is used in Raudenbush, S. & Bryk, A. (2002).
# It is part of the package mlmhelpr
# use ?hsb to show description of data set
# added column for subject id (sid)
hsbcsv <- hsb %>% 
  mutate(sid = as_factor(paste0('S', 1:nrow(.)))) %>%
  relocate(sid, .before = id) %>%
  readr::write_csv("hsb.csv")

# test
read_csv("hsb.csv") %>%
  mutate_if(is.character, as_factor)
