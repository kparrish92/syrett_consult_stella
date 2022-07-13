source(here::here("scripts", "00_libs.R"))
source(here("scripts", "01_helpers.R"))
source(here("scripts", "03_load_data.R"))

## American English speakers should be good at subgrouping (English), 
## and more accurate than the other groups. 

mod_5 = read_rds(here("data", "models", "mod_b.rds"))

## Filter only English language speakers 

df_raw = read.csv(here("data", "data_v.csv"), header=T, na.strings=c(""))
groups = read.csv(here("data", "groups.csv"), header=T, na.strings=c(""))

df_speakers  <- df_raw %>% 
  dplyr::select(speaker, lang_1, lang_2, lang_3) 


p1_15 = mds_plot_15(eng_mono) %>% 
  mutate(group = "English mono")

p2_15 = mds_plot_15(e_asian) %>% 
  mutate(group = "East Asian")

p3_15 = mds_plot_15(s_asian) %>% 
  mutate(group = "South Asian")

p4_15 = mds_plot_15(se_asian) %>% 
  mutate(group = "Southeast Asian")

p5_15 = mds_plot_15(non_multi) %>% 
  mutate(group = "Non multi")

plot_df_15 = rbind(p1_15,p2_15,p3_15,p4_15,p5_15)


# mod_15 = brm(dist_from_center ~ group*lang_3 + (1 | lang_2), data = plot_df_15)

mod_15 <- 
  brm(formula = dist_from_center ~ group*lang_3 +
        (1 | lang_2),
      warmup = 1000, iter = 3000, chains = 4, 
      cores = parallel::detectCores(), 
      control = list(adapt_delta = 0.99, max_treedepth = 20), 
      data = plot_df_15,
      file = here("data", "models", "mod15.rds"))


con_df = conditional_effects(mod_15)[["group:lang_3"]]

langs = df_raw %>% 
  dplyr::select(lang_1, lang_2, lang_3)

df_all = con_df %>% 
  dplyr::select(-lang_2) %>% 
  left_join(langs, by = "lang_3") 

df_all %>% 
  ggplot(aes(y = estimate__, x = group, label = lang_3, color = lang_2)) + 
  geom_point(position = position_dodge(width = .5), size = .5) +
  geom_text(position = position_dodge(width = .5), size = 2, 
            hjust = -.1) + theme_minimal() + ylab("Distance from centroid") + 
  theme(text=
          element_text(
            size=8,
            family="Arial")) + 
  theme(legend.position = "bottom", legend.text=element_text(size=5)) +
  scale_color_discrete(name = "5-category language group")
  

library("ggrepel")

df_all %>% 
  ggplot(aes(y = estimate__, x = group, label = lang_3, color = lang_2)) + 
  geom_text_repel()

  geom_pointrange(aes(ymin = lower__, ymax = upper__),
                  shape = 21,
                  position = position_dodge(width = .6))  

