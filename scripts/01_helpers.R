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
    dplyr::select(speaker, lang_1, lang_2) %>% 
    filter(!is.na(speaker))
  
  american = group_mem %>% 
    filter(lang_2 == "American")
  
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
 
error_rate_2 = function(df)
{
  df_lang_cats = read.csv(here("data", "data_v.csv"), header=T, na.strings=c("")) %>% 
    select(speaker, lang_1, lang_2, lang_3) %>% 
    filter(!is.na(speaker))
  
  df = df %>% 
    mutate(speaker = rownames(df)) %>% 
    left_join(df_lang_cats, by = "speaker")
  
  cols = colnames(df)
  
  nruns = ncol(df) - 3
  
  #i = 2
  #thisrun = 2
  
  # 2 cat error rate 
  
  error_2_df = matrix()
  participant = matrix()
  
  for (i in 1:nruns) {
    
    df_2 = df %>% 
      rename("col" = cols[i])
    
    hope = df_2 %>% 
      group_by(lang_1, col) %>% 
      summarize(n = n()) %>% 
      group_by(col) %>% 
      filter(n()>1 | n == 1)
    
    error_df = matrix()
    
    for (thisrun in 1:nrow(unique(hope[,2]))) {
      
      rundf = unique(hope[,2])
      hope2 = hope %>% filter(col == rundf[thisrun,1])
      
      error_df[thisrun] = ifelse(nrow(hope2) > 1, sum(hope2$n) - max(hope2$n),
                                 ifelse(nrow(hope2)==1, 1,
                                        0))        
      
      
    }
    
    error_2_df[i] = sum(error_df)
    participant[i] = cols[i]
    x = cbind(error_2_df, participant)
  }
  return(as.data.frame(x)) 
}

error_rate_5 = function(df)
{
  df_lang_cats = read.csv(here("data", "data_v.csv"), header=T, na.strings=c("")) %>% 
    select(speaker, lang_1, lang_2, lang_3) %>% 
    filter(!is.na(speaker))
  
  df = df %>% 
    mutate(speaker = rownames(df)) %>% 
    left_join(df_lang_cats, by = "speaker")
  
  cols = colnames(df)
  
  nruns = ncol(df) - 3
  
  #i = 2
  #thisrun = 2
  
  # 2 cat error rate 
  
  error_2_df = matrix()
  participant = matrix()
  
  for (i in 1:nruns) {
    
    df_2 = df %>% 
      rename("col" = cols[i])
    
    hope = df_2 %>% 
      group_by(lang_2, col) %>% 
      summarize(n = n()) %>% 
      group_by(col) %>% 
      filter(n()>1 | n == 1)
    
    error_df = matrix()
    
    for (thisrun in 1:nrow(unique(hope[,2]))) {
      
      rundf = unique(hope[,2])
      hope2 = hope %>% filter(col == rundf[thisrun,1])
      
      error_df[thisrun] = ifelse(nrow(hope2) > 1, sum(hope2$n) - max(hope2$n),
                                 ifelse(nrow(hope2)==1, 1,
                                        0))        
      
      
    }
    
    error_2_df[i] = sum(error_df)
    participant[i] = cols[i]
    x = cbind(error_2_df, participant)
  }
  return(as.data.frame(x)) 
}

error_rate_15 = function(df)
{
  df_lang_cats = read.csv(here("data", "data_v.csv"), header=T, na.strings=c("")) %>% 
    dplyr::select(speaker, lang_1, lang_2, lang_3) %>% 
    filter(!is.na(speaker))
  
  df = df %>% 
    mutate(speaker = rownames(df)) %>% 
    left_join(df_lang_cats, by = "speaker")
  
  cols = colnames(df)
  
  nruns = ncol(df) - 3
  
  #i = 2
  #thisrun = 2
  
  # 2 cat error rate 
  
  error_2_df = matrix()
  participant = matrix()
  
  for (i in 1:nruns) {
    
    df_2 = df %>% 
      rename("col" = cols[i])
    
    hope = df_2 %>% 
      group_by(lang_3, col) %>% 
      summarize(n = n()) %>% 
      group_by(col) %>% 
      filter(n()>1 | n == 1)
    
    error_df = matrix()
    
    for (thisrun in 1:nrow(unique(hope[,2]))) {
      
      rundf = unique(hope[,2])
      hope2 = hope %>% filter(col == rundf[thisrun,1])
      
      error_df[thisrun] = ifelse(nrow(hope2) > 1, sum(hope2$n) - max(hope2$n),
                                 ifelse(nrow(hope2)==1, 1,
                                        0))        
      
      
    }
    
    error_2_df[i] = sum(error_df)
    participant[i] = cols[i]
    x = cbind(error_2_df, participant)
  }
  return(as.data.frame(x)) 
}


e_dist = function(df, x1, x2, y1, y2)
{
  d = sqrt((x1 - x2)^2  + (y1 - y2)^2)
  return(d)}



mds_plot = function(df)
{
  mydata = df
  d <- dist(mydata) # euclidean distances between the rows
  fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
  
  
  group_mem = read.csv(here("data", "data_v.csv"), header=T, na.strings=c("")) %>% 
    dplyr::select(speaker, lang_1, lang_2, lang_3) %>% 
    filter(!is.na(speaker))
  
  x <- fit$points[,1]
  y <- fit$points[,2]
  data = data.frame(x,y)
  
  data$speaker = rownames(data)
  
  data = data %>% 
    left_join(group_mem, by = "speaker")
  
  means = data %>% 
    group_by(lang_2) %>% 
    summarize(x.2 = mean(x), y.2 = mean(y))
  
  e_dist = function(df, x1, x2, y1, y2)
  {
    d = sqrt((x1 - x2)^2  + (y1 - y2)^2)
    return(d)}
  
  
  final_df = left_join(data, means) %>% 
    mutate(dist_from_center = e_dist(x1 = x, x2 = x.2, y1 = y, y2 = y.2))
  
  
  p = ggplot(data, aes(x, y, color = lang_2)) +
    geom_point(size = 1) + stat_ellipse(level = .8) + theme_minimal() + 
    ylab("Dimension 1") +
    xlab("Dimension 2") +
    labs(color = "Language Group") 
  return(final_df)
}


plot_ind_est = function(eff)
{
  cbPalette <- c("#999999", "#E69F00", "#56B4E9",
                 "#009E73", "#F0E442", "#0072B2", 
                 "#D55E00", "#CC79A7")
  fixef_df_param = re_plot_condef %>% 
    filter(effect2__ == eff)
  
  re_plot_condef$group = stringr::str_replace(re_plot_condef$group, "non_multi", "Non-Asian Multilingual")

  
  plot = re_plot_condef %>% 
    filter(effect2__ == eff) %>% 
    ggplot(aes(y = reorder(effect1__, +estimate__), x = estimate__, fill = group, 
               shape = group)) +
    geom_pointrange(aes(xmin = lower__, xmax = upper__), 
                    shape = 21, 
                    position = position_dodge(width = .6)) + 
    theme_minimal() +
    theme(text=element_text(size=10)) +
    xlim(-1,11) +
    theme(legend.position = "none", legend.text=element_text(size=5),
          legend.title=element_text(size=5)) +
    xlab("Language Group") + ylab("Estimate") +
    scale_fill_manual(values=cbPalette) + 
    geom_text(data = mutate_if(fixef_df_param, is.numeric, round, 2),
              aes(label = paste0(estimate__, " [", lower__, " - ", upper__, "]"), x = Inf), 
              hjust = "inward", size = 2.5) +
    ggtitle(paste("Parameter estimates for distance from the centroid for", eff))
  return(plot)
}


mds_plot_15 = function(df)
{
  mydata = df
  d <- dist(mydata) # euclidean distances between the rows
  fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
  
  
  group_mem = read.csv(here("data", "data_v.csv"), header=T, na.strings=c("")) %>% 
    dplyr::select(speaker, lang_1, lang_2, lang_3) %>% 
    filter(!is.na(speaker))
  
  x <- fit$points[,1]
  y <- fit$points[,2]
  data = data.frame(x,y)
  
  data$speaker = rownames(data)
  
  data = data %>% 
    left_join(group_mem, by = "speaker")
  
  means = data %>% 
    group_by(lang_3) %>% 
    summarize(x.2 = mean(x), y.2 = mean(y))
  
  e_dist = function(df, x1, x2, y1, y2)
  {
    d = sqrt((x1 - x2)^2  + (y1 - y2)^2)
    return(d)}
  
  
  final_df = left_join(data, means) %>% 
    mutate(dist_from_center = e_dist(x1 = x, x2 = x.2, y1 = y, y2 = y.2))
  
  
  p = ggplot(data, aes(x, y, color = lang_2)) +
    geom_point(size = 1) + stat_ellipse(level = .8) + theme_minimal() + 
    ylab("Dimension 1") +
    xlab("Dimension 2") +
    labs(color = "Language Group") 
  return(final_df)
}




mds_plot_1 = function(df)
{
  mydata = df
  d <- dist(mydata) # euclidean distances between the rows
  fit <- cmdscale(d,eig=TRUE, k=1) # k is the number of dim
  
  
  group_mem = read.csv(here("data", "data_v.csv"), header=T, na.strings=c("")) %>% 
    dplyr::select(speaker, lang_1, lang_2, lang_3) %>% 
    filter(!is.na(speaker))
  
  x <- fit$points[,1]
  data = data.frame(x)
  
  data$speaker = rownames(data)
  
  data = data %>% 
    left_join(group_mem, by = "speaker")
  
  means = data %>% 
    group_by(lang_3) %>% 
    summarize(x.2 = mean(x))
  
  e_dist = function(df, x1, x2, y1, y2)
  {
    d = sqrt((x1 - x2)^2  + (y1 - y2)^2)
    return(d)}
  
  
  final_df = left_join(data, means) 
  
  p = ggplot(data, aes(x, y, color = lang_2)) +
    geom_point(size = 1) + stat_ellipse(level = .8) + theme_minimal() + 
    ylab("Dimension 1") +
    xlab("Dimension 2") +
    labs(color = "Language Group") 
  return(final_df)
}


make_tree = function(df)
{
  df_raw = read.csv(here("data", "data_v.csv"), header=T, na.strings=c(""))
  df_speakers  <- df_raw %>% 
    dplyr::select(speaker, lang_1, lang_2, lang_3) %>% 
    filter(!is.na(speaker))
  
  df$speaker_3 = df_speakers$lang_3
  df$speaker_2 = df_speakers$lang_2
  df$speaker_1 = df_speakers$lang_1
  
  geo <- factor(df$speaker_2)
  
  (mycol <- c("purple", "yellow", "blue",
              "red", "orange") [geo])
  
  
  
  x = dist(df)
  names = df$speaker_3
  
  M <- matrix(0, 45, 45)
  M[lower.tri(M)] <- x
  M <- t(M)
  M[lower.tri(M)] <- x
  dimnames(M) <- list(names, names)
  tr <- nj(M)
  
  plot = plot(tr)
  return(plot)
}


make_tree_5_colors = function(df)
{
  df_raw = read.csv(here("data", "data_v.csv"), header=T, na.strings=c(""))
  df_speakers  <- df_raw %>% 
    dplyr::select(speaker, lang_1, lang_2, lang_3) %>% 
    filter(!is.na(speaker))
  
  df$speaker_3 = df_speakers$lang_3
  df$speaker_2 = df_speakers$lang_2
  df$speaker_1 = df_speakers$lang_1
  
  geo <- factor(df$speaker_2)
  
  (mycol <- c("darkorchid", "gold2", "deepskyblue2",
              "firebrick2", "darkorange") [geo])
  
  
  
  x = dist(df)
  names = df$speaker_3
  
  M <- matrix(0, 45, 45)
  M[lower.tri(M)] <- x
  M <- t(M)
  M[lower.tri(M)] <- x
  dimnames(M) <- list(names, names)
  tr <- nj(M)
  
  plot = plot(tr, tip.color = mycol)
  return(plot)
}


make_tree_15_colors = function(df)
{
  df_raw = read.csv(here("data", "data_v.csv"), header=T, na.strings=c(""))
  df_speakers  <- df_raw %>% 
    dplyr::select(speaker, lang_1, lang_2, lang_3) %>% 
    filter(!is.na(speaker))
  
  df$speaker_3 = df_speakers$lang_3
  df$speaker_2 = df_speakers$lang_2
  df$speaker_1 = df_speakers$lang_1
  
  geo <- factor(df$speaker_3)
  
  (mycol <- c("#999999", "#E69F00", "#56B4E9",
              "#009E73", "#F0E442", "#0072B2", 
              "#D55E00", "#CC79A7", "red",
              "blue", "green", "grey",
              "#a8a8f8", "#bada55", "#57dece") [geo])
  
  
  
  x = dist(df)
  names = df$speaker_3
  
  M <- matrix(0, 45, 45)
  M[lower.tri(M)] <- x
  M <- t(M)
  M[lower.tri(M)] <- x
  dimnames(M) <- list(names, names)
  tr <- nj(M)
  
  plot = plot(tr, tip.color = mycol)
  return(plot)
}
