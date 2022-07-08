source(here::here("scripts", "00_libs.R"))
source(here("scripts", "01_helpers.R"))
source(here("scripts", "03_load_data.R"))

mod = readRDS(here("data", "models", "mod_b.rds"))

brm_df = conditional_effects(mod)

re_plot_condef = as.data.frame(brm_df[["group:lang_2"]])


#### Making effect plots for each individual language type 


plot_ind_est = function(eff)
{
cbPalette <- c("#999999", "#E69F00", "#56B4E9",
               "#009E73", "#F0E442", "#0072B2", 
               "#D55E00", "#CC79A7")
fixef_df_param = re_plot_condef %>% 
  filter(effect2__ == eff)

plot = re_plot_condef %>% 
  filter(effect2__ == eff) %>% 
  ggplot(aes(y = reorder(effect1__, +estimate__), x = estimate__, fill = group, 
             shape = group)) +
  geom_pointrange(aes(xmin = lower__, xmax = upper__), 
                  shape = 21, 
                  position = position_dodge(width = .6)) + 
  theme_minimal() +
  theme(text=element_text(size=10,  family="Times")) +
  xlim(0,9) +
  theme(legend.position = "none", legend.text=element_text(size=5),
        legend.title=element_text(size=5)) +
  xlab("Language Group") + ylab("Estimate") +
  scale_fill_manual(values=cbPalette) + 
  geom_text(data = mutate_if(fixef_df_param, is.numeric, round, 2),
            aes(label = paste0(estimate__, " [", lower__, " - ", upper__, "]"), x = Inf), 
            hjust = "inward", size = 2.5, family="Times") +
  ggtitle(paste("Parameter estimates for distance from the centroid for", eff))
return(plot)
}

plot_ind_est("South")

#+
#  ggsave(here("data",
#              "plots", "mod_plot.png"))

