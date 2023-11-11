### Comparación exp-baseline
library(tidyverse)

setwd("~")

bl <- read_delim("buckets/b1/exp/KA8240_baseline_exp_colab/KA8240_baseline_exp_colab_ganancias_semillerio.csv")
goss <- read_delim("buckets/b1/exp/KA8240_exp_colab_goss/KA8240_exp_colab_goss_ganancias_semillerio.csv")
bagging <- read_delim("buckets/b1/exp/KA8240_exp_colab_bagging/KA8240_exp_colab_bagging_ganancias_semillerio.csv")

bagging_opt <- bagging %>% filter(envios == envios[1])
bl_opt <- bl %>% filter(envios == envios[1])
goss_opt <- goss %>% filter(envios == envios[1])

test_bag_bl_opt <- wilcox.test(bagging_opt$ganancia,bl_opt$ganancia, alternative = 'greater',paired = T)
test_goss_bl_opt <- wilcox.test(goss_opt$ganancia,bl_opt$ganancia, alternative = 'greater',paired = T)
test_bag_goss_opt <- wilcox.test(goss_opt$ganancia,bagging_opt$ganancia, alternative = 'two.sided',paired = T)

bl <- bl %>% 
  rename(baseline = ganancia)

goss <- goss %>% 
  rename(goss = ganancia)

bagging <- bagging %>% 
  rename(bagging = ganancia)



tabla <- bl %>% 
  full_join(goss) %>% 
  full_join(bagging) %>% 
  #filter(!is.na(baseline) & !is.na(bagging) & !is.na(goss)) %>% 
  pivot_longer(cols = c(baseline,goss,bagging),
               names_to = "modelo",
               values_to = "ganancia") %>% 
  mutate(ganancia = ganancia/1000000)

summary <- tabla %>% 
  group_by(modelo,envios) %>% 
  summarize(media = mean(ganancia),
            sd = sd(ganancia),
            median = median(ganancia)) %>% 
  ungroup() %>% 
  distinct() %>% 
  mutate(n = 20)

#

### Grafico curvas por envios

graf_envios <- ggplot(tabla, aes(x = envios, y = ganancia,colour = modelo)) +
  geom_smooth() +
  geom_vline(xintercept = 12636,
             colour = "#F8766D",
             linetype = "dashed") +
  geom_vline(xintercept = 13117,
             colour = "#00BA38",
             linetype = "dashed") +
  geom_vline(xintercept = 11755,
             colour = "#619CFF",
             linetype = "dashed")
  

graf_envios


tests <- tribble(~comparacion,~p.value,~statistic,
                  "bagging vs. baseline",test_bag_bl_opt$p.value,test_bag_bl_opt$statistic[1],
                  "goss vs. baseline",test_goss_bl_opt$p.value,test_goss_bl_opt$statistic[1],
                  "bagging vs. goss",test_bag_goss_opt$p.value,test_bag_goss_opt$statistic[1])


writexl::write_xlsx(list("summary" = tabla,
                     "wilcoxon" = tests),
                    "~/buckets/b1/exp/exp_colab_comparacion/resultados_grupoB.xlsx")
                    
