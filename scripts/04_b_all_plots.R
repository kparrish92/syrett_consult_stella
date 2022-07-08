source(here::here("scripts", "00_libs.R"))
source(here("scripts", "01_helpers.R"))
source(here("scripts", "03_load_data.R"))


## define color palette 
cbPalette <- c("#999999", "#E69F00", "#56B4E9",
               "#009E73", "#F0E442", "#0072B2", 
               "#D55E00", "#CC79A7")

### No categories plot  
no_cats_df %>% 
  group_by(group_name) %>% 
  summarize(mean_cats = mean(no_categories), sd_cats = sd(no_categories)) 

no_cats_df_desc = no_cats_df %>% 
  group_by(group_name) %>% 
  summarize(mean_cats = mean(no_categories), sd_cats = sd(no_categories)) 


no_cats_df$group_name <- factor(no_cats_df$group_name, 
                                levels = 
                                  c("non_asian_multi", 
                                    "south_asian",
                                    "se_asian",
                                    "east_asian",
                                    "eng_mono"))

no_cats_df %>% 
  filter(!is.na(Code)) %>% 
  ggplot(aes(x = no_categories, y = group_name, fill = group_name)) + geom_boxplot(outlier.shape = NA) +
  geom_text(data = mutate_if(no_cats_df_desc, is.numeric, round, 2),
            aes(label = paste0(mean_cats, " (", sd_cats, ")"), x = Inf), 
            hjust = "inward", size = 5) + xlim(0, 20) + ylab("Group Name") + 
  theme_minimal() + theme(legend.position = "none") +
  scale_y_discrete(labels=c("eng_mono" = "English monolingual", 
                            "east_asian" = "East Asian",
                            "se_asian" = "Southeast Asian",
                            "south_asian" = "South Asian",
                            "non_asian_multi" = "Non-Asian Multilingual")) +
  ggtitle("Average Number of Categories Created per group") +
  scale_fill_manual(values=cbPalette) +
  scale_x_continuous(name="Number of Categories", breaks=c(0,5,10,15), limits=c(0, 20)) +
  theme(text=
          element_text(
            size=14,
            family="Arial")) +
  ggsave(here("docs", "figs", "desc_all.png"), width = 8, height= 7)

### MDS plot 

p1 = mds_plot(eng_mono) %>% 
  mutate(group = "English mono")

p2 = mds_plot(e_asian) %>% 
  mutate(group = "East Asian")

p3 = mds_plot(s_asian) %>% 
  mutate(group = "South Asian")

p4 = mds_plot(se_asian) %>% 
  mutate(group = "Southeast Asian")

p5 = mds_plot(non_multi) %>% 
  mutate(group = "Non multi")

plot_df = rbind(p1,p2,p3,p4,p5)

plot_df %>%
  ggplot(aes(x, y, color = lang_2)) +
  geom_point(size = 1, alpha = .6) + 
  stat_ellipse(level = .8, linetype = "dashed") + 
  theme_minimal() + 
  ylab("Dimension 1") +
  xlab("Dimension 2") +
  labs(color = "Language Group") +
  scale_color_manual(values=cbPalette) +
  theme(legend.position = "bottom", legend.text=element_text(size=5),
        legend.title=element_text(size=5)) + facet_wrap(~group, nrow = 2) +
  ggsave(here("data",
              "plots", "mds.png"))

## Plots with 15 languages and colors for 5 grouping 
plot_df %>%
  ggplot(aes(x, y, color = lang_3)) +
  geom_point(size = 1, alpha = .6) + 
  stat_ellipse(aes(x, y, color = lang_2), level = .8, linetype = "dashed") +
  theme_minimal() + 
  ylab("Dimension 1") +
  xlab("Dimension 2") +
  labs(color = "Language Group") +
  theme(legend.position = "bottom", legend.text=element_text(size=5),
        legend.title=element_text(size=5)) + facet_wrap(~group, nrow = 2)

## Mod plot 

re_plot_condef %>% 
  ggplot(aes(x = effect1__, y = estimate__, fill = effect2__)) +
  geom_pointrange(aes(ymin = lower__, ymax = upper__), 
                  shape = 21, 
                  position = position_dodge(width = .6)) + 
  scale_fill_manual(values=cbPalette, name = "Language type") + 
  theme_minimal() +
  xlab("Language Group") + ylab("Estimate") +
  ggsave(here("data",
              "plots", "mod_plot.png"))

## Error plot 

level_order = c("2 category", "5 category", "15 category")

error_rates %>% 
  ggplot(aes(y = error_r, x = factor(grouping, level = level_order), fill = group, group = group)) +
  geom_line(position = position_dodge(width = .6), alpha = .5) +
  geom_pointrange(aes(ymin = error_r + error_r_sd, ymax = error_r - error_r_sd), 
                  shape = 21, 
                  position = position_dodge(width = .6)) +
  theme_minimal() + scale_fill_manual(values=cbPalette) + ylab("Error rate") +
  xlab("Number of categories considered correct") + ggsave(here("data",
                                                                "plots", "error_plot.png"))

## Dump cat plot 

t_desc = t %>% 
  group_by(group_name) %>% 
  summarize(mean_cats = mean(V1), sd_cats = sd(V1)) 


t %>% 
  filter(!is.na(Code)) %>% 
  ggplot(aes(x = as.numeric(V1), y = as.factor(group_name), fill = as.factor(group_name))) + 
  geom_boxplot(outlier.shape = NA) + 
  scale_fill_brewer(palette = "Set3") + 
  geom_vline(xintercept = 
               mean(as.numeric(t$V1)), 
             linetype = "dashed", 
             alpha = .5) + 
  xlab("Maximum Number of Speakers in a category") + 
  ylab("Group") +
  theme(text=
          element_text(
            size=8,
            family="Arial")) +
  scale_y_discrete(labels=c("eng_mono" = "English monolingual", 
                            "east_asian" = "East Asian",
                            "se_asian" = "Southeast Asian",
                            "south_asian" = "South Asian",
                            "non_asian_multi" = "Non-Asian Multilingual")) +
  theme_minimal() + theme(legend.position = "none") + 
  geom_text(data = mutate_if(t_desc, is.numeric, round, 2),
            aes(label = paste0(mean_cats, " (", sd_cats, ")"), x = Inf), 
            hjust = "inward", size = 3) + 
  ggsave(here("docs", "figs", "max_mems.png"), width = 8, height= 7)



### Appendix plots

posterior <- as.data.frame(mod)

pars = colnames(posterior[1:25])

fixef_df = fixef(mod) %>% 
  as.data.frame() %>% 
  rownames_to_column("parameter") %>% 
  mutate(parameter = paste0("b_", parameter))

bayesplot::mcmc_areas(posterior,
                      pars = pars,
                      prob = 0.8) + 
  geom_text(data = mutate_if(fixef_df, is.numeric, round, 2),
            aes(label = paste(Estimate, "[", Q2.5, "-", Q97.5, "]"), x = Inf), 
            hjust = "inward", size = 2) + 
  xlim(-10,15) +
  theme(text = element_text(size=8)) +
  ggsave(here("data",
              "plots", "mcmc_plot.png"), dpi = 300)



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

