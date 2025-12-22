library(openxlsx)
library(ggplot2)

mate.choice.tbl <- read.xlsx("crossing Evi.xlsx")

mate.choice.tbl$Result <- factor(mate.choice.tbl$Result, levels = c("ND", "TRUE"))

ggplot(mate.choice.tbl) + geom_bar(aes(x = Type, fill = Result), position = "stack") + 
  coord_flip() + 
  scale_fill_manual(values = c("#CDC9C9", "#4EEE94")) +
  theme_bw(base_size = 14) + theme(panel.grid.minor.x = element_blank())
#ggsave("mate_choice_draft.png")
ggsave("mate_choice_draft.svg")
