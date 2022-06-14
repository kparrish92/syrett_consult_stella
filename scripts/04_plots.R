source(here("scripts", "00_libs.R"))
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



ggarrange(plot_clusters(eng_mono, clusters = 4),
          plot_clusters2(eng_mono, clusters = 4))


