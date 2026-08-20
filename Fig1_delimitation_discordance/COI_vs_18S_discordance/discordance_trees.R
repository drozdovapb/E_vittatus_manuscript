library(ggtree)
library(ggplot2)
library(cowplot)
library(tidyverse) #to bind rows

## from ggtree book

library(dplyr)
library(ggtree)
library(ggnewscale)

x <- read.tree("iqtree_trees/Evi_COI_120_trimmed_wEve.fas.treefile")
y <- read.tree("iqtree_trees/Evi_18S_120_trimmed_wEve.fas.treefile")

## process support values
evaluate_support <- function(tre) {
  ## aLRT > 70% or aBayes >0.7 => a red dot
  label <- tre$node.label
  ## Get aLRT from newick and identify large values
  alrt <- sapply(strsplit(label, split="/"), "[", 1)
  bigalrt <- alrt > 70 & !is.na(alrt)
  ## Get aBayes from newick and identify large values
  bayes <- sapply(strsplit(label, split="/"), "[", 2)
  bigbayes <- bayes > 0.7 & !is.na(bayes)
  ## get UFB from newick and identify large values
  bb <- sapply(strsplit(label, split="/"), "[", 3)
  bigbb <- bb > 90 & !is.na(bb)
  ## add dots where appropriate
  newlabel <- ifelse(bigalrt & bigbayes & bigbb, intToUtf8(9679), "")
  tre$node.label <- newlabel
  return(tre)
}

x <- evaluate_support(x)
y <- evaluate_support(y)

## fix one sequence name that was not identical in two trees
x$tip.label[x$tip.label=='Amspa61_To'] <- 'AmspA61_To'

## make ggtree objects for each tree, we'll need them later
p1 <- ggtree(x)
p2 <- ggtree(y)

## extract data from tree
d1 <- p1$data
d2 <- p2$data

## reverse x-axis and set distance (offset) 
## to make the tree on the right-hand side of the first tree
d2$x <- max(d2$x) - d2$x + max(d1$x) + .1


## use known haplogroups for coloring tip labels
hapl <- read.csv("./Haplogroups.csv")

## find which samples are discordant to make their links red
which.discordant <- hapl$Haplogroup_COI != hapl$Haplogroup_18S
hapl.discordant <- hapl[which.discordant,]
discordant.inds <- hapl.discordant$Label

## make data frame for all links between the same specimens in COI and 18S
dd <- bind_rows(d1, d2) %>% 
  filter(isTip)
## and now the same but only for discordant samples
ddd <- bind_rows(d1, d2) %>% 
  filter(label %in% discordant.inds)


## add haplogroups to tables
d1 <- left_join(d1, hapl, by = join_by(label == Label))
d2 <- left_join(d2, hapl, by = join_by(label == Label))



ggtree(tr=x, data=d1) +  
  geom_tree(data=d2) + 
  geom_line(aes(x, y, group=label), data=dd, color='grey', linewidth = .3) + 
  geom_line(aes(x, y, group=label), data=ddd, color='red', linewidth = .3) + 
  geom_tiplab(data=d1, geom = 'label', aes(fill=Haplogroup_COI), 
              alpha=.5, label.padding = unit(0.05, "lines")) +
  geom_tiplab(data=d2, hjust=1, geom='label', aes(fill=Haplogroup_18S), 
              alpha=.5, label.padding = unit(0.05, "lines")) + 
  #geom_tiplab(data=d1, geom = 'shadowtext', size=2, aes(color=Haplogroup_COI),  bg.colour='grey50') +
  #geom_tiplab(data=d2, hjust=1, size=2, geom='shadowtext', aes(color=Haplogroup_18S), bg.colour='grey50') + 
  geom_treescale(x = 0, y=100, offset = 3) + 
  scale_fill_manual(values = c("#F6850C", "#4275A8", "#F0E33F"), name="Haplogroup") 

#ggsave("disc_tree.svg", width = 50, height=50, units="cm")


ggtree(tr=x, data=d1) +  
  geom_tree(data=d2) + 
  geom_line(aes(x, y, group=label), data=dd, color='grey', linewidth = .3) + 
  geom_line(aes(x, y, group=label), data=ddd, color='red', linewidth = .3) + 
  geom_tiplab(data=d1, geom = 'label', aes(fill=Subgroup_COI),
              alpha=.5, label.padding = unit(0.03, "lines")) +
  scale_fill_manual(values = c("#F6850C", "#4275A8", "#E3FF7A", "#FFF400"), name="COI (left):",) + 
  new_scale_fill() + 
  geom_tiplab(data=d2, hjust=1, geom='label', aes(fill=Subgroup_18S),
              alpha=.5, label.padding = unit(0.03, "lines")) +
  scale_fill_manual(values = c("#ff9955ff", "#946032ff", "#0e4985ff", "#8aba19ff"), name="18S (right):") + 
  geom_nodelab(color="purple", size=4, nudge_x = -.001) + 
  annotate(geom = "text", x=.01, y=87, label = "aBayes > 0.7 & \n aLRT > 70% & \n UFB > 90%") + 
  annotate(geom="text", x=.001, y=90, label = intToUtf8(9679), col = "purple") +
  geom_treescale(x = 0, y=100, offset = 1) + 
  theme(legend.position = "inside", legend.position.inside = c(0.2, 0.8))


ggsave("FigS2_disc_tree_more_colors.svg", width = 36, height=48, units="cm", device=svg)
ggsave("FigS2_disc_tree_more_colors.png", width = 36, height=48, units="cm", device=png, dpi = 180)
ggsave("FigS2_disc_tree_more_colors.pdf", width = 36, height=48, units="cm", device=cairo_pdf)

## FIG1D
## Sankey diagram
######
#devtools::install_github("davidsjoberg/ggsankey")
library(ggsankey)

hapl <- read.csv("./Haplogroups.csv")

hapl %>% make_long(Haplogroup_COI, Haplogroup_18S) -> hapl_long

hapl_long$node <- factor(hapl_long$node, levels = c("W", "N", "S"))
hapl_long$next_node <- factor(hapl_long$next_node, levels = c("W", "N", "S"))


ggplot(hapl_long, aes(x = x, 
                      next_x = next_x, 
                      node = node, 
                      next_node = next_node,
                      fill = factor(node))) +
  geom_sankey(color="black", width = .3) +
  theme_sankey(base_size = 14, base_family = "Arial") + 
  scale_fill_manual(values = c( "#F0E33F", "#F6850C", "#4275A8")) + 
  #  scale_y_reverse() + 
  labs(x="", fill="Haplogroup", col="Haplogroup") + 
  geom_sankey_label(aes(label = after_stat(paste0(node, "\n n = ", freq)),
                        color=factor(node)), 
                    size = 4) + 
  scale_color_manual(values=c("black", "black", "white"))
ggsave('discordance_sankey.svg', width=15, height=13, units="cm")
