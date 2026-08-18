library(ggtree)
library(treeio)
library(ggplot2)
library(ggtreeExtra)
library(ggrepel)

#topology <- read.tree("./GMYC/EviCOI_coalescent_const-Evi_COI_trimmed_wEve.nwk")
topology <- read.beast("./GMYC/EviCOI_coalescent_const-Evi_COI_trimmed_wEve.tree")

delimitation <- read.csv("./delim_results.csv")

ptr <- ggtree(topology) + 
  geom_tiplab(size=1) + 
  geom_label_repel(aes(label=round(as.numeric(posterior), 2), 
                color=as.numeric(posterior) > 0.7), size = 2) + 
  scale_color_manual(values = c("NA", "red")) + 
  guides(color="none")
ptr


ptr + 
  geom_fruit(data = delimitation, geom = geom_tile, width=.01,
             mapping = aes(fill=factor(Haplogroup), y=Specimen, x=1.0)) +
  geom_fruit(data = delimitation, geom = geom_tile, width=.01,
             mapping = aes(fill=factor(ABGD), y=Specimen, x=1.2)) +
  geom_fruit(data = delimitation, geom = geom_tile, width=.01,
                 mapping = aes(fill=factor(ASAP), y=Specimen, x=2)) + 
  geom_fruit(data = delimitation, geom = geom_tile, width=.01,
                 mapping = aes(fill=factor(BIN), y=Specimen, x=1.4)) + 
  geom_fruit(data = delimitation, geom = geom_tile, width=.01,
                 mapping = aes(fill=factor(GMYC), y=Specimen, x=1.6)) + 
  geom_fruit(data = delimitation, geom = geom_tile, width=.01,
                 mapping = aes(fill=factor(bPTP), y=Specimen, x=1.8)) + 
  guides(fill="none") +
  scale_fill_manual(values = rep(c("red", "green", "yellow", 
                               "brown", "purple", "blue2",
                               "turquoise", "blue4", "black"), 4))

ggsave("tree_draft.svg", device = svg(), height=16, width = 16, units="cm")

