library(tanggle)
library(dplyr) ## for some data rearrangement
library(phangorn)
library(ggplot2)
library(ggtree)
library(ggpubr)
## read the data = nexus file recorded with SplitsTree4 (!)
##R version 4.3.2.


#########################
#literature data COI
Nnet <- read.nexus.networx("./Evi_COI_other_data.nex")

pn <- 
  ggsplitnet(Nnet, col="grey33") + 
  geom_treescale(x=-.04, y=.035, offset=.001) + 
  coord_fixed()

tips <- pn$data[pn$data$isTip, ]
tips$group <- sapply(tips$label, function(x) strsplit(x, split = "_")[[1]][3])
tips$group[which(is.na(tips$group))] <- "out"

tips %>% count(x, y, group) -> tips.occur

tips$place <- sapply(tips$label, function(x) strsplit(x, split = "_")[[1]][2])
tips$place <- ifelse(startsWith(tips$place, "MK") | tips$place == "Evi", "Lake Baikal", "Angara River")


tips %>% count(x, y, group, place) -> tips.occur
tips.occur <- tips.occur[order(tips.occur$n), ]

## list groups to color
unique(tips.occur$group)




pn + 
  geom_point(data = tips.occur, aes(x=x, y=y, fill=group, shape=group, alpha=group, color=group), 
             size=6, stroke = 1.2) + 
  scale_fill_manual(values = c("#4477aa","#E3FF7A","#F4CE98","#E3FF7A","black","#E3FF7A","#4477aa","#4477aa", "#E3FF7A", "#E3FF7A",  "#E3FF7A"), 
                    name="Haplogroup") +
  # scale_shape_manual(values = c(25, 25, 21, 25, 22, 25, 21, 25, 25, 21, 25), 
  #                    name="Haplogroup") +
  ## asterisks version
  # scale_shape_manual(values = c(8, 8, 21, 8, 21, 8, 21, 8, 8, 21, 8), 
  #                    name="Haplogroup") +
  ## tried triangles; they don't work
  # scale_shape_manual(values = c(25, 25, 24, 25, 22, 25, 24, 25, 25, 24, 25), 
  #                    name="Haplogroup") +
  # scale_alpha_manual(values = c(1, 1, 0.3, 1, 1, 1, 0.3, 1, 1, 0.3, 1), 
  #                    name="Haplogroup") +
  scale_shape_manual(values = c(23, 23, 21, 23, 23, 23, 21, 23, 23, 21, 23), 
                     name="Haplogroup") +
  scale_alpha_manual(values = c(1, 1, 0.2, 1, 1, 1, 0.2, 1, 1, 0.2, 1), 
                     name="Haplogroup") +
  scale_color_manual(values = c("black", "black", "grey33", rep("black", 3), "grey33", "black", "black", "grey33", "black"),
                     name="Haplogroup") + 
  expand_limits(x=.03) + 
  #  scale_color_manual(values=c("white", "black"), name = "Place") + 
  #  scale_color_manual(values = c("#66BB3C", "#D81B60", "grey50", "#4477AA", "#F0E442"), 
  #                    name="Haplogroup") +
  #  scale_shape_manual(values = c(21, 22), name = "Place") +
  #guides(fill = guide_legend(override.aes=list(shape=21)),
  #       color = guide_legend(override.aes=list(col="black"))) + 
  theme(legend.position = "left") -> pnCOI
pnCOI





###########################

#18S
Nnet1 <- read.nexus.networx("Evi_18S_tree.nex")

pn1 <- 
  ggsplitnet(Nnet1, col="grey50") + 
  geom_treescale(x=0, y=0.005, offset=.0001) + 
  coord_fixed()

tips <- pn1$data[pn1$data$isTip, ]
tips$group <- sapply(tips$label, function(x) strsplit(x, split = "_")[[1]][3])
tips$group[which(is.na(tips$group))] <- "out"


tips$place <- sapply(tips$label, function(x) strsplit(x, split = "_")[[1]][2])
tips$place <- ifelse(startsWith(tips$place, "MK") | tips$place == "Evi", "Lake Baikal", "Angara River")


tips %>% count(x, y, group, place) -> tips.occur
tips.occur <- tips.occur[order(tips.occur$n), ]

sort(unique(tips.occur$group))

pn1 + 
  geom_point(data = tips.occur, aes(x=x, y=y, fill=group, shape=group,  size=group, alpha=group, stroke = 0.5)) + #alpha=group,
  scale_fill_manual(values = c("#FF9955", "#946032", "#8ABA19","black" ,"#0E4985", "#8ABA19","#8ABA19"), 
                    name="Haplogroup") +
  ## shapes: 21 is circle for our data and 23 is lozenge for literature data
  scale_shape_manual(values = c(21, 21, 23, 23, 21, 21, 23), 
                     name="Haplogroup") +
  ## size: lozenges a bit larger to make them visible
  scale_size_manual(values = c(8, 8, 10, 8, 8, 8, 10), 
                     name="Haplogroup") +
  ## transparency: 0.2 for our data and 1 for literature data
  scale_alpha_manual(values = c(0.2, 0.2, 1, 0.2, 1, 0.2, 0.2, 1), 
                     name="Haplogroup") +
  ## stroke color: grey33 for our data & black for lit data
  scale_color_manual(values = c("grey90", "grey90", "black", "grey90", "black", "black", "grey90", "black"),
                     name="Haplogroup") + 
  
  #expand_limits(x=.03) + 
  # guides(fill = guide_legend(override.aes=list(shape=21)),
  #        color = guide_legend(override.aes=list(col="black"))) + 
  theme(legend.position = "left") -> pn18s
pn18s


ggarrange(pnCOI, pn18s)
ggsave("splitnets_wlit.svg", device=svg, width=45, height = 15, units="cm")
