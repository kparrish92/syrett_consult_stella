## Error rate plots 

source(here::here("scripts", "00_libs.R"))
source(here("scripts", "01_helpers.R"))
source(here("scripts", "03_load_data.R"))


### Test max no of errors 


e2_error_eng = error_rate_2(eng_mono) %>% 
  mutate(group = "English monolingual") %>% 
  filter(participant != "speaker")

e2_error_ea = error_rate_2(e_asian) %>% 
  mutate(group = "East Asian") %>% 
  filter(participant != "speaker")

e2_error_sa = error_rate_2(s_asian) %>% 
  mutate(group = "South Asian") %>% 
  filter(participant != "speaker")

e2_error_sea = error_rate_2(se_asian) %>% 
  mutate(group = "South-east Asian") %>% 
  filter(participant != "speaker")

e2_error_nm = error_rate_2(non_multi) %>% 
  mutate(group = "non_multi") %>% 
  filter(participant != "speaker")

e2_error_df = rbind(e2_error_eng, e2_error_ea, e2_error_sa, e2_error_sea, e2_error_nm)

e2_error_df %>% 
  write.csv(here("data", "tidy", "e2_error_df.csv"))

#### 5 category error rate 

e5_error_eng = error_rate_5(eng_mono) %>% 
  mutate(group = "English monolingual") %>% 
  filter(participant != "speaker")

e5_error_ea = error_rate_5(e_asian) %>% 
  mutate(group = "East Asian") %>% 
  filter(participant != "speaker")

e5_error_sa = error_rate_5(s_asian) %>% 
  mutate(group = "South Asian") %>% 
  filter(participant != "speaker")

e5_error_sea = error_rate_5(se_asian) %>% 
  mutate(group = "South-east Asian") %>% 
  filter(participant != "speaker")

e5_error_nm = error_rate_5(non_multi) %>% 
  mutate(group = "non_multi") %>% 
  filter(participant != "speaker")

e5_error_df = rbind(e5_error_eng, e5_error_ea, e5_error_sa, e5_error_sea, e5_error_nm)

e5_error_df %>% 
  write.csv(here("data", "tidy", "e5_error_df.csv"))


#### 15 category error rate 

e15_error_eng = error_rate_15(eng_mono) %>% 
  mutate(group = "English monolingual") %>% 
  filter(participant != "speaker")

e15_error_ea = error_rate_15(e_asian) %>% 
  mutate(group = "East Asian") %>% 
  filter(participant != "speaker")

e15_error_sa = error_rate_15(s_asian) %>% 
  mutate(group = "South Asian") %>% 
  filter(participant != "speaker")

e15_error_sea = error_rate_15(se_asian) %>% 
  mutate(group = "South-east Asian") %>% 
  filter(participant != "speaker")

e15_error_nm = error_rate_15(non_multi) %>% 
  mutate(group = "non_multi") %>% 
  filter(participant != "speaker")

e15_error_df = rbind(e15_error_eng, e15_error_ea, e15_error_sa, e15_error_sea, e15_error_nm)

e15_error_df %>% 
  write.csv(here("data", "tidy", "e15_error_df.csv"))