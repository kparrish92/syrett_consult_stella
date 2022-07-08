source(here::here("scripts", "00_libs.R"))
source(here("scripts", "01_helpers.R"))
source(here("scripts", "03_load_data.R"))

# Visualize error rate per group 


e2_error_df = read.csv(here("data", "tidy", "e2_error_df.csv")) %>% 
  mutate(error_rate = error_2_df/45)
e5_error_df = read.csv(here("data", "tidy", "e5_error_df.csv")) %>% 
  mutate(error_rate = error_2_df/45)
e15_error_df = read.csv(here("data", "tidy", "e15_error_df.csv")) %>% 
  mutate(error_rate = error_2_df/45)

e2_error_df %>% 
  group_by(group) %>%
  summarize(error_r = mean(error_rate), error_r_sd = sd(error_rate)) %>% 
  ggplot(aes(x = error_r, y = group, fill = group)) + geom_col() + xlim(0,.5)

e5_error_df %>% 
  group_by(group) %>%
  summarize(error_r = mean(error_rate), error_r_sd = sd(error_rate)) %>% 
  ggplot(aes(x = error_r, y = group, fill = group)) + geom_col() + xlim(0,.5)


e15_error_df %>% 
  group_by(group) %>%
  summarize(error_r = mean(error_rate), error_r_sd = sd(error_rate)) %>% 
  ggplot(aes(x = error_r, y = group, fill = group)) + geom_col() + xlim(0,1)


d1 = e2_error_df %>% 
  group_by(group) %>%
  summarize(error_r = mean(error_rate), error_r_sd = sd(error_rate)) %>% 
  mutate(grouping = "2 category")

d2 = e5_error_df %>% 
  group_by(group) %>%
  summarize(error_r = mean(error_rate), error_r_sd = sd(error_rate)) %>% 
  mutate(grouping = "5 category")

d3 = e15_error_df %>% 
  group_by(group) %>%
  summarize(error_r = mean(error_rate), error_r_sd = sd(error_rate)) %>% 
  mutate(grouping = "15 category")

df_c = rbind(d1,d2,d3) 

level_order = c("2 category", "5 category", "15 category")

df_c %>% 
  ggplot(aes(y = error_r, x = factor(grouping, level = level_order), fill = group, group = group)) +
  geom_line(position = position_dodge(width = .6), alpha = .5) +
  geom_pointrange(aes(ymin = error_r + error_r_sd, ymax = error_r - error_r_sd), 
                  shape = 21, 
                  position = position_dodge(width = .6)) +
  theme_minimal() + scale_fill_manual(values=cbPalette) + ylab("Error rate") +
  xlab("Number of categories considered correct") + ggsave(here("data",
                                                                "plots", "error_plot.png"))

df_c %>% 
  write.csv(here("data", "tidy", "error_rates.csv"))
# flip

