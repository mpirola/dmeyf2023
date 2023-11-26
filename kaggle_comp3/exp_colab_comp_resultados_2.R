### Comparación exp-baseline
library(tidyverse)

setwd("~")

bl <- read_delim("buckets/b1/exp/KA8240_baseline_exp_colab/KA8240_baseline_exp_colab_ganancias_semillerio.csv")
goss <- read_delim("buckets/b1/exp/KA8240_exp_colab_goss_B/KA8240_exp_colab_goss_B_ganancias_semillerio.csv")
bagging <- read_delim("buckets/b1/exp/KA8240_exp_colab_bagging_B/KA8240_exp_colab_bagging_B_ganancias_semillerio.csv")


#bagging <- read_delim("buckets/b1/exp/KA8240_FE1_bag_v1_test/KA8240_FE1_bag_v1_test_ganancias_semillerio.csv")
#goss <- read_delim("buckets/b1/exp/KA8240_FE1_goss_v1_test/KA8240_FE1_goss_v1_test_ganancias_semillerio.csv")

bagging_opt <- bagging %>% filter(envios == 11250)
goss_opt <- goss %>% filter(envios == 11250)

test_bag_bl_opt <- wilcox.test(bagging_opt$ganancia,bl_opt$ganancia, alternative = 'greater',paired = T)
test_goss_bl_opt <- wilcox.test(goss_opt$ganancia,bl_opt$ganancia, alternative = 'greater',paired = T)
test_bag_goss_opt <- wilcox.test(goss_opt$ganancia,bagging_opt$ganancia, alternative = 'two.sided',paired = T)

bl <- bl %>% 
  rename(baseline = ganancia)

goss <- goss %>% 
  rename(goss = ganancia)

bagging <- bagging %>% 
  rename(bagging = ganancia)



tabla <- goss %>% 
  full_join(bagging) %>% 
  full_join(bl) %>% 
  filter(!is.na(baseline) & !is.na(bagging) & !is.na(goss)) %>% 
  pivot_longer(cols = c(goss,bagging,baseline),
               names_to = "modelo",
               values_to = "ganancia") %>% 
  mutate(ganancia = ganancia/1000000)

summary <- tabla %>% 
  group_by(modelo,envios) %>% 
  summarize(ganancia_media = mean(ganancia),
            sd = sd(ganancia),
            median = median(ganancia)) %>% 
  ungroup() %>% 
  distinct() %>% 
  mutate(n = 20)

#

### Grafico curvas por envios

graf_envios <- ggplot(summary, aes(x = envios, y = ganancia_media,colour = modelo)) +
  geom_ribbon(aes(x = envios, ymin = ganancia_media - 2*sd/sqrt(20), ymax = ganancia_media + 2*sd/sqrt(20), fill = modelo, alpha = 0.8)) +
  geom_line() +
  geom_point() +
  scale_alpha(guide = "none")

  

graf_envios


tests <- tribble(~comparacion,~p.value,~statistic,
                  "bagging vs. baseline",test_bag_bl_opt$p.value,test_bag_bl_opt$statistic[1],
                  "goss vs. baseline",test_goss_bl_opt$p.value,test_goss_bl_opt$statistic[1],
                  "bagging vs. goss",test_bag_goss_opt$p.value,test_bag_goss_opt$statistic[1])


writexl::write_xlsx(list("summary" = tabla,
                     "wilcoxon" = tests),
                    "~/buckets/b1/exp/exp_colab_comparacion/resultados_grupoB.xlsx")
                    
