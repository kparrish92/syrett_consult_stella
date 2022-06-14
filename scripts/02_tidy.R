source(here::here("scripts", "00_libs.R"))
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


####

new_df = df_int
  
container = matrix(nrow = ncol(new_df), ncol = 2)

for(i in 1:ncol(new_df)) {       # for-loop over columns
  container[i, 1] <- length(unique(new_df[ , i]))
  container[i, 2] <- colnames(new_df[i])
}


container_df = as.data.frame(container) %>% 
  mutate(Participant = str_remove(V2, "X")) %>% 
  rename(no_categories = V1)

groups$Participant = as.character(groups$Participant)

no_cats_df = left_join(container_df, groups, by = "Participant") %>% 
  select(no_categories, Participant, Code) %>% 
  mutate(no_categories = as.numeric(no_categories)) %>% 
  mutate(group_name = case_when(
    Code == 111 ~ "east_asian",
    Code == 112 ~ "south_asian",
    Code == 113 ~ "se_asian",
    Code == 100 ~ "eng_mono",
    Code == 101 ~ "non_asian_multi"
  ))

mean(no_cats_df$no_categories)

length(unique(df_raw$lang_3)) # no of possible cats 

length(unique(df_raw$lang_2)) # no of less specific cats 

no_cats_df %>% 
  group_by(group_name) %>% 
  summarize(mean_cats = mean(no_categories), sd_cats = sd(no_categories))


no_cats_df %>% 
  write.csv(here("data", "tidy", "desc_all.csv"))

########

df = df_int

df$speaker <- rownames(df)

df$speaker

types = df_raw %>% 
  select(speaker, lang_1, lang_2) %>% 
  filter(!is.na(speaker))

asian_df = left_join(df, types, by = "speaker") %>% 
  filter(lang_1 == "Asian") 
###

container_b = matrix(nrow = ncol(asian_df), ncol = 2)

for(i in 1:ncol(asian_df)) {       # for-loop over columns
  container_b[i, 1] <- length(unique(asian_df[ , i]))
  container_b[i, 2] <- colnames(asian_df[i])
}


container_df_b = as.data.frame(container_b) %>% 
  mutate(Participant = str_remove(V2, "X")) %>% 
  rename(no_categories = V1)


no_cats_df_b = left_join(container_df_b, groups, by = "Participant") %>% 
  select(no_categories, Participant, Code) %>% 
  mutate(no_categories = as.numeric(no_categories)) %>% 
  mutate(group_name = case_when(
    Code == 111 ~ "east_asian",
    Code == 112 ~ "south_asian",
    Code == 113 ~ "se_asian",
    Code == 100 ~ "eng_mono",
    Code == 101 ~ "non_asian_multi"
  )) %>% 
  filter(!is.na(Code))

## The English monolingual group made the second most categories 

no_cats_df_b %>% 
  group_by(group_name) %>% 
  summarize(mean_cats = mean(no_categories), sd_cats = sd(no_categories))

no_cats_df_b %>% 
  write.csv(here("data", "tidy", "desc_asian.csv"))


