
source(here::here("scripts", "00_libs.R"))
source(here("scripts", "01_helpers.R"))
source(here("scripts", "03_load_data.R"))


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

cbPalette <- c("#999999", "#E69F00", "#56B4E9",
               "#009E73", "#F0E442", "#0072B2", 
               "#D55E00", "#CC79A7")

plot_df$group = stringr::str_replace(plot_df$group, "Non multi", "Non-Asian Multilingual")
plot_df$group = stringr::str_replace(plot_df$group, "English mono", "English monolingual")

plot_df$lang_2 = stringr::str_replace(plot_df$lang_2, "American", "American English")
plot_df$lang_2 = stringr::str_replace(plot_df$lang_2, "International ", "International English")

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

# use dfs to determine centroid of each cluster ellipse - calculate the euc dist
# from each centroid to the point - use as the outcome? 


# d =√[(x2 – x1)2 + (y2 – y1)2]



mds <- eng_mono %>%
  dist() %>%          
  cmdscale() %>%
  as.data.frame()

colnames(mds) <- c("Dim.1", "Dim.2")
# Plot MDS
clust <- kmeans(mds, 3)$cluster %>%
  as.factor()
mds <- mds %>%
  mutate(groups = clust)
# Plot and color by groups
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = rownames(eng_mono),
          color = "groups",
          palette = "jco",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)


ggscatter(p1, x = "x", y = "y", 
          label = "speaker",
          color = "group",
          palette = "jco",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)


### analysis 

glimpse(plot_df)
time | participant

mod = brm(dist_from_center ~ group*lang_2 + (lang_2 | lang_3), data = plot_df)

mod %>% write_rds(here("data", "models", "mod_b.rds"))

brm_df = conditional_effects(mod)

re_plot_condef = as.data.frame(brm_df[["group:lang_2"]])

re_plot_condef$group = stringr::str_replace(re_plot_condef$group, "Non multi", "Non-Asian Multilingual")
re_plot_condef$group = stringr::str_replace(re_plot_condef$group, "mono", "monolinugal")


re_plot_condef %>% 
  ggplot(aes(x = group, y = estimate__, fill = effect2__)) +
  geom_pointrange(aes(ymin = lower__, ymax = upper__), 
                  shape = 21, 
                  position = position_dodge(width = .6)) + 
  scale_fill_manual(values=cbPalette, name = "Language type") + 
  theme_minimal() +
  xlab("Language Group") + ylab("Estimate") +
  theme(axis.text = element_text(size = 4.5)) +
  ggsave(here("data",
              "plots", "mod_plot.png"))


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

### Do a frequentist model 


mod_0 = lmer(dist_from_center ~ 1 + (1 | lang_3), data = plot_df)

mod_1 = lmer(dist_from_center ~ group + (1 | lang_3), data = plot_df)

mod_2 = lmer(dist_from_center ~ group + lang_2 + (1 | lang_3), data = plot_df)

mod_3 = lmer(dist_from_center ~ group + lang_2 + group:lang_2 + 
               (1 | lang_3), data = plot_df)

anova(mod_0, mod_1, mod_2, mod_3)



tab_model(mod)

rope(mod)

post_d = describe_posterior(
  mod,
  effects = "all",
  component = "all",
  test = c("p_direction", "p_significance"),
  centrality = "all"
)

posterior <- as.data.frame(mod)
rope(posterior, range = c(-0.1, 0.1))
