
df_lang_cats = read.csv(here("data", "data_v.csv"), header=T, na.strings=c("")) %>% 
  select(speaker, lang_1, lang_2, lang_3) %>% 
  filter(!is.na(speaker))

source(here("scripts", "03_load_data.R"))

head(eng_mono)

color_plot = function(df)
{
pca <- stats::prcomp(df, scale = TRUE, center = FALSE)
ind <- facto_summarize(pca, element = "ind", result = "coord") %>% 
  rename(speaker = name)

plot_viz_df = left_join(ind, df_lang_cats, by = "speaker")

plot = plot_viz_df %>% 
  ggplot(aes(x = Dim.1, y = Dim.2, color = lang_2)) + geom_point() +
  geom_label() + 
  stat_ellipse(level = .67)
return(plot)
}


color_plot2(eng_mono)
color_plot2(s_asian)
color_plot2(se_asian)
color_plot2(e_asian)
color_plot2(non_multi)




color_plot2 = function(df)
{
  pca <- stats::prcomp(df, scale = TRUE, center = FALSE)
  ind <- facto_summarize(pca, element = "ind", result = "coord") %>% 
    rename(speaker = name)
  
  plot_viz_df = left_join(ind, df_lang_cats, by = "speaker")
  
  plot = plot_viz_df %>% 
    ggplot(aes(x = Dim.1, y = Dim.2, color = lang_2)) + geom_point() +
    stat_ellipse(level = .67)
  return(plot)
}




## Number of people per category in groups




