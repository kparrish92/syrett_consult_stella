source(here::here("scripts", "00_libs.R"))



plot_needed_clusters = function(df)
{
  
  
p1 <- fviz_nbclust(df, FUN = hcut, method = "wss",
                     k.max = 10) +
    ggtitle("(A) Elbow method")
  
p2 <- fviz_nbclust(df, FUN = hcut, method = "silhouette",
                     k.max = 10) +
    ggtitle("(B) Silhouette method")

p3 <- fviz_nbclust(df, FUN = hcut, method = "gap_stat",
                     k.max = 10) +
    ggtitle("(C) Gap statistic")

plot = ggarrange(p1, p2, p3, nrow = 1)
  
  return(plot)
}




plot_clusters = function(df, clusters)
{
hc.cut <- hcut(df, k = clusters, hc_method = "ward.D2")
plot = fviz_dend(hc.cut, show_labels = TRUE, rect = TRUE)
return(plot)
}

get_best_k = function(df)
{
  cl <- kmeansruns(df, krange = 4:10, iter.max = 1000)
  
  return(cl$bestk)
}


plot_clusters2 = function(df, clusters)
{df.scaled <- scale(df)

km.res <- hcut(df, k = clusters)
plot = fviz_cluster(km.res, df, ellipse.type = "norm", repel = TRUE)

return(plot)
}




freq_model = function(df)
{
  
  ex = dist(df) %>% as.matrix()
  
  group_mem = read.csv(here("data", "data_v.csv"), header=T, na.strings=c("")) %>% 
    select(speaker, lang_1, lang_2) %>% 
    filter(!is.na(speaker))
  
  american = group_mem %>% 
    filter(lang_2 == "American")
  american$speaker
  
  df_dist = ex %>% as.data.frame()
  
  df_dist_eng = df_dist %>% select(american$speaker)
  
  rows = rownames(df_dist_eng)
  
  df_dist_eng = df_dist_eng %>% 
    mutate(speaker = rows)
  
  an_df = left_join(df_dist_eng, group_mem, by = "speaker") %>% 
    pivot_longer(cols = 1:3, values_to = "distance", names_to = "eng_type") %>% 
    filter(!speaker %in% american$speaker) %>% 
    mutate(speaker_sep = speaker) %>% 
    separate(speaker_sep, into = c("language", "sep"), sep = "_") %>% 
    select(-sep)
  
  null = lme4::lmer(distance ~ 1 + (1 | speaker) + (1 | eng_type), data = an_df)
  mod1 = lme4::lmer(distance ~ lang_2 + (1 | speaker) + (1 | eng_type), data = an_df)
  
  nmc = anova(null, mod1)
  
  mod_plot = plot_model(mod1, show.intercept = TRUE)
  
  
  return(mod_plot)
}

# language group = am eng, other eng, se asian, e asian, s asian

