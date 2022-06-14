source(here("scripts", "00_libs.R"))
source(here("scripts", "01_helpers.R"))


# Hours
# 6/9 12:30 - 2:30 
# 3:30 - 5:30

# Tidy data and divide all data into group dataframes

# Read data 

df_raw = read.csv(here("data", "data_v.csv"), header=T, na.strings=c(""))
groups = read.csv(here("data", "groups.csv"), header=T, na.strings=c(""))

df_raw <- df_raw %>% 
  select(speaker, 4:200) %>%
  filter(!is.na(speaker))  
  
df_raw[ df_raw == "n/a" ] <- NA

df_new = df_raw[ , colSums(is.na(df_raw)) == 0] # What to do with n/a data? 
# There are 28 participants who have n/a data that interferes with the analysis
# We could either remove them or somehow recode the n/a data. 
# so that it works
rownames(df_new) <- df_new$speaker
df_int = select_if(df_new, is.numeric)
# There are 7 additional participants who have data that is coded with 
# spaces or something similar - I'll work to recode this data (remove spaces etc)
# so that it works

df_t = df_int %>% 
  t() %>%
  as.data.frame() %>% 
  rownames_to_column("participant") %>% 
  mutate(participant = str_remove(participant, "X"))

group_east_asian = groups %>% filter(Code == "111")
group_south_asian = groups %>% filter(Code == "112")
group_se_asian = groups %>% filter(Code == "113")
group_eng_mono = groups %>% filter(Code == "100")
group_non_target_multi = groups %>% filter(Code == "101")

# Create dfs for each group 

eng_mono_df = df_t %>% 
  filter(participant %in% group_eng_mono$Participant)

rownames(eng_mono_df) <- eng_mono_df$participant

eng_mono_df %>% 
  select(-participant) %>% 
  t() %>% 
  as.data.frame() %>%
  rownames_to_column("speaker") %>% 
  write_csv(here("data", "tidy", "eng_mono_df.csv"))


#######

east_asian_df = df_t %>% 
  filter(participant %in% group_east_asian$Participant)

rownames(east_asian_df) <- east_asian_df$participant

east_asian_df %>% 
  select(-participant) %>% 
  t() %>% 
  as.data.frame() %>%
  rownames_to_column("speaker") %>% 
  write_csv(here("data", "tidy", "east_asian_df.csv"))

#######

south_asian_df = df_t %>% 
  filter(participant %in% group_south_asian$Participant) 

rownames(south_asian_df) <- south_asian_df$participant

south_asian_df %>% 
  select(-participant) %>% 
  t() %>% 
  as.data.frame() %>%
  rownames_to_column("speaker") %>% 
  write_csv(here("data", "tidy", "south_asian_df.csv"))

#######

se_asian_df = df_t %>% 
  filter(participant %in% group_se_asian$Participant) %>% 
  write_csv(here("data", "tidy", "se_asian_df.csv"))

rownames(se_asian_df) <- se_asian_df$participant

se_asian_df %>% 
  select(-participant) %>% 
  t() %>% 
  as.data.frame() %>%
  rownames_to_column("speaker") %>% 
  write_csv(here("data", "tidy", "se_asian_df.csv"))


#######

non_target_df = df_t %>% 
  filter(participant %in% group_non_target_multi$Participant) 

rownames(non_target_df) <- non_target_df$participant

non_target_df %>% 
  select(-participant) %>% 
  t() %>% 
  as.data.frame() %>%
  rownames_to_column("speaker") %>% 
  write_csv(here("data", "tidy", "non_target_df.csv"))
