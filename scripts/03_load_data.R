# read tidy dfs 
eng_mono =  read.csv(here("data", "tidy", "eng_mono_df.csv")) 
e_asian = read.csv(here("data", "tidy", "east_asian_df.csv")) 
s_asian = read.csv(here("data", "tidy", "south_asian_df.csv")) 
se_asian =  read.csv(here("data", "tidy", "se_asian_df.csv")) 
non_multi = read.csv(here("data", "tidy", "non_target_df.csv"))


rownames(eng_mono) <- eng_mono$speaker
rownames(e_asian) <- e_asian$speaker
rownames(s_asian) <- s_asian$speaker
rownames(se_asian) <- se_asian$speaker
rownames(non_multi) <- non_multi$speaker

eng_mono = eng_mono %>% 
  dplyr::select(-speaker)

e_asian = e_asian %>% 
  dplyr::select(-speaker)

s_asian = s_asian %>% 
  dplyr::select(-speaker)

se_asian = se_asian %>% 
  dplyr::select(-speaker)

non_multi = non_multi %>% 
  dplyr::select(-speaker)


no_cats_df = read.csv(here("data", "tidy", "desc_all.csv"))

no_cats_df_b = read.csv(here("data", "tidy", "desc_asian.csv"))


mod = read_rds(here("data", "models", "mod_b.rds"))

# Run with 3, 5, and 12? 


# English, int English, Asian 

# English, int English, S Asian, Se asian, E asian

# English, int English, 



#data.frame(group = c("english_mono", "e_asian", "s_asian", "se_asian", "non_multi"),
 #          no_clusters = c())


#get_best_k(eng_mono)
#get_best_k(e_asian) 
#get_best_k(s_asian)  
#get_best_k(se_asian)   
#get_best_k(non_multi) 

mod = readRDS(here("data", "models", "mod_b.rds"))

brm_df = conditional_effects(mod)

re_plot_condef = as.data.frame(brm_df[["group:lang_2"]])

error_rates = read.csv(here("data", "tidy", "error_rates.csv"))



t = read.csv(here("data", "tidy", "max_no_cats.csv"))
