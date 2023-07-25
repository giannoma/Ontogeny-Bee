# Preparing Data for Manova
# List of all inds separated into ryhthmic vs non-rhythmic by window.
library(tidyverse)

#Choose files of interest
path = "/analysis/"  
all_files = list.files(path, recursive = TRUE)
files_of_int = all_files[stringr::str_detect(all_files, '3 day window/lomb_scargle_results.csv')]
paths_of_int = paste0(path, '/', files_of_int)


#Read in the files
df = readr::read_csv(paths_of_int, id = 'file')

#pretty names
df_names = str_remove(df$file, '.*Eddie\'s Analysis of Experimental Data/')
df_names = str_remove(df_names, '/3 day.*')
df$file = df_names
df = unite(df, 'uid', c(file, unique_identifier), sep = '_')

#Select the relevant columns
df_gc = select(df, uid, window, gc_raw_to_cos, gc_cos_to_raw)

#Generate rhythmic vs non-rythmic column
df_gc = mutate(df_gc, rhythmic = case_when(
  gc_raw_to_cos <= 0.01 ~ 1,
  gc_cos_to_raw <= 0.01 ~ 1,
  TRUE ~ 0
))
df_gc = select(df_gc, uid, window, rhythmic)

#remove problematic ind
df_gc = filter(df_gc, uid != 'TGiray1_09_14_12_NA')


#If an individual was rhythmic at any time, mark it as rhythmic
df_rhythm = df_gc %>% group_by(uid) %>% 
  mutate(rhythmic = cumsum(rhythmic)) %>% 
  mutate(rhythmic = ifelse(rhythmic > 0, 1, 0))

df_rhythm = df_rhythm %>% pivot_wider(uid, names_from = window, values_from = rhythmic)

write.csv(df_rhythm, file = paste0(path, '/', '2022_11_14_rhythm_bouts_by_oversall00001.csv'))


#
