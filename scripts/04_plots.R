source(here::here("scripts", "00_libs.R"))
source(here("scripts", "01_helpers.R"))
source(here("scripts", "03_load_data.R"))


plot_needed_clusters(eng_mono) + 
  ggsave(here("docs", "figs", "eng_check.png"), width = 10, height=8)

plot_clusters(eng_mono, clusters = 5) + 
  ggtitle("Cluster Dendrogram of English monolingual group") +
  ggsave(here("docs", "figs", "eng_mono_den.png"), width = 10, height=10)

plot_needed_clusters(se_asian) +
  ggsave(here("docs", "figs", "se_asian_check.png"), width = 10, height=8)

plot_clusters(se_asian, clusters = 5) + 
  ggtitle("Cluster Dendrogram of Southeast asian multilingual group") +
  ggsave(here("docs", "figs", "se_asian_den.png"), width = 10, height=10)


plot_needed_clusters(s_asian) +
  ggsave(here("docs", "figs", "s_asian_check.png"), width = 10, height=8)


plot_clusters(s_asian, clusters = 5) + 
  ggtitle("Cluster Dendrogram of South asian multilingual group") +
  ggsave(here("docs", "figs", "s_asian_den.png"), width = 10, height=10)

plot_needed_clusters(e_asian) + 
  ggsave(here("docs", "figs", "e_asian_check.png"), width = 10, height=8)

plot_clusters(e_asian, clusters = 5) + 
ggtitle("Cluster Dendrogram of East asian multilingual group") +
  ggsave(here("docs", "figs", "e_asian_den.png"), width = 10, height=10)

plot_needed_clusters(non_multi) +
  ggsave(here("docs", "figs", "non_multi_check.png"), width = 10, height=8)

plot_clusters(non_multi, clusters = 5) + 
  ggtitle("Cluster Dendrogram of Non-asian multilingual group") +
  ggsave(here("docs", "figs", "non_multi_den.png"), width = 10, height=10)



ex_df_eng = hcut(eng_mono, k = 4) 

plot_data = ex_df_eng$data

pca <- stats::prcomp(plot_data, scale = FALSE, center = FALSE)
ind <- facto_summarize(pca, element = "ind", result = "coord")

ind$name
ind %>% 
  ggplot(aes(x = Dim.1, y = Dim.2)) + geom_text(label = name)


ex_df_eng$cluster
ex_df_eng$height


ex_df_eng$dist.method
ex_df_eng$order
ex_df_eng$labels

eng_mono_t = t(eng_mono)


ggarrange(plot_clusters(eng_mono, clusters = 4),
          plot_clusters2(eng_mono, clusters = 4))

ggarrange(plot_clusters(eng_mono_t, clusters = 4),
          plot_clusters2(eng_mono_t, clusters = 4))


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

cbPalette <- c("#999999", "#E69F00", "#56B4E9",
               "#009E73", "#F0E442", "#0072B2", 
               "#D55E00", "#CC79A7")

no_cats_df %>% 
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



no_cats_df_b %>% 
  group_by(group_name) %>% 
  summarize(mean_cats = mean(no_categories), sd_cats = sd(no_categories))



no_cats_df_desc_b = no_cats_df_b %>% 
  group_by(group_name) %>% 
  summarize(mean_cats = mean(no_categories), sd_cats = sd(no_categories)) 


no_cats_df_b$group_name <- factor(no_cats_df_b$group_name, 
                                  levels = 
                                    c("non_asian_multi", 
                                      "south_asian",
                                      "se_asian",
                                      "east_asian",
                                      "eng_mono"))

no_cats_df_b %>% 
  ggplot(aes(x = no_categories, y = group_name, fill = group_name)) + geom_boxplot(outlier.shape = NA) +
  geom_text(data = mutate_if(no_cats_df_desc_b, is.numeric, round, 2),
            aes(label = paste0(mean_cats, " (", sd_cats, ")"), x = Inf), 
            hjust = "inward", size = 5) + xlim(0, 20) + ylab("Group Name") + 
  theme_minimal() + theme(legend.position = "none") +
  scale_y_discrete(labels=c("eng_mono" = "English monolingual", 
                            "east_asian" = "East Asian",
                            "se_asian" = "Southeast Asian",
                            "south_asian" = "South Asian",
                            "non_asian_multi" = "Non-Asian Multilingual")) +
  ggtitle("Average Number of Asian Language Categories") +
  scale_x_continuous(name="Number of Categories", breaks=c(0,5,10,15), limits=c(0, 20)) +
  scale_fill_brewer(palette = "Set3") +
  theme(text=
          element_text(
            size=14,
            family="Arial")) + 
  ggsave(here("docs", "figs", "desc_asian.png"), width = 8, height= 7)


library("TOSTER")

TOSTtwo(m1=5.85, m2=6.12, sd1=1.73, sd2=1.8, n1=45, n2=24, 
        low_eqbound_d=-0.48, high_eqbound_d=0.48, alpha = 0.05, var.equal = TRUE)

