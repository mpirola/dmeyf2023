### Comparación exp-baseline
library(tidyverse)

setwd("~")

bl <- read_delim("buckets/b1/exp/KA8240_baseline_exp_colab/KA8240_baseline_exp_colab_ganancias_semillerio.csv")
goss <- read_delim("buckets/b1/exp/KA8240_exp_colab_goss/KA8240_exp_colab_goss_ganancias_semillerio.csv")
bagging <- read_delim("buckets/b1/exp/KA8240_exp_colab_bagging/KA8240_exp_colab_bagging_ganancias_semillerio.csv")

test_bag_bl <- wilcox.test(bagging$ganancia,bl$ganancia, alternative = 'greater',paired = T)
test_goss_bl <- wilcox.test(goss$ganancia,bl$ganancia, alternative = 'greater',paired = T)
test_bag_goss <- wilcox.test(goss$ganancia,bagging$ganancia, alternative = 'two.sided',paired = T)

bl <- bl %>% 
  rename(bl = ganancia)

goss <- goss %>% 
  rename(goss = ganancia)

bagging <- bagging %>% 
  rename(bagging = ganancia)



tabla <- bl %>% 
  full_join(goss) %>% 
  full_join(bagging) %>% 
  pivot_longer(cols = c(3:5)) %>% 
  group_by(name) %>% 
  summarize(media = mean(value/1000000),
            sd = sd(value/1000000),
            median = median(value/1000000)) %>% 
  ungroup() %>% 
  distinct() %>% 
  mutate(n = 20) 

tests <- tribble(~comparacion,~p.value,~statistic,
                 "bagging vs. baseline",test_bag_bl$p.value,test_bag_bl$statistic[1],
                 "goss vs. baseline",test_goss_bl$p.value,test_goss_bl$statistic[1],
                 "bagging vs. goss",test_bag_goss$p.value,test_bag_goss$statistic[1])


writexl::write_xlsx(list("summary" = tabla,
                     "wilcoxon" = tests),
                    "~/buckets/b1/exp/exp_colab_comparacion/resultados_grupoB.xlsx")
                    
