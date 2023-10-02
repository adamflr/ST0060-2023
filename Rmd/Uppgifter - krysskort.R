library(tidyverse)
tab <- expand_grid(Question = 1:30 + rep(1:6, each = 5),
                   Exercise = seq(1, 4, length.out = 8)) %>% 
  group_by(Exercise) %>% 
  mutate(Rank = rank(Question),
         Type = ifelse(Rank %in% 1:20, "Sal", 
                       ifelse(Rank %in% 21:25, "Bonus", "Hemuppgift")))

tab2 <- tab %>% filter(Type != "Hemuppgift")

g <- ggplot(tab2, aes(y = -Question, Exercise, color = Type)) +
  geom_point(shape = 21, size = 9.5, stroke = 1) +
  # geom_vline(xintercept = seq(5.5, 30.5, 5)) +
  annotate("text", y = -0.9, x = unique(tab$Exercise), label = paste0("", 1:length(unique(tab$Exercise))), family = "serif") +
  # annotate("text", x = c(25.5, 31.5), y = max(tab$Exercise) + 0.5, label = c("Bonus", "Hem"), family = "serif", hjust = 0) +
  cowplot::theme_nothing() +
  scale_color_manual(values = c("purple", "hotpink")) +
  xlim(-2, 7)
g

ggsave("../Datorövningar/Krysskort.pdf", height = 297, width = 210, units = "mm")

