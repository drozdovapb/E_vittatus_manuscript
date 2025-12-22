library(tanggle)
library(dplyr) ## for some data rearrangement
library(phangorn)
library(ggplot2)
library(ggtree)
## read the data = nexus file recorded with SplitsTree4 (!)
##R version 4.3.2.



#literature data COI
#Nnet <- read.nexus.networx("E:/Evi_COI_other_data.nex")
Nnet <- read.nexus.networx("../Evi_COI_other_data.nex")

pn <- 
  ggsplitnet(Nnet, col="black") + 
  geom_treescale(x=-.04, y=.035, offset=.001) + 
  coord_fixed()
#  geom_tiplab2() + 
#  geom_point(aes(shape="21", color=isTip), size=2)

tips <- pn$data[pn$data$isTip, ]
tips$group <- sapply(tips$label, function(x) strsplit(x, split = "_")[[1]][3])
tips$group[which(is.na(tips$group))] <- "out"

tips %>% count(x, y, group) -> tips.occur



tips$place <- sapply(tips$label, function(x) strsplit(x, split = "_")[[1]][2])
tips$place <- ifelse(startsWith(tips$place, "MK") | tips$place == "Evi", "Lake Baikal", "Angara River")


tips %>% count(x, y, group, place) -> tips.occur
tips.occur <- tips.occur[order(tips.occur$n), ]

pn + 
  geom_point(data = tips.occur, aes(x=x, y=y, fill=group), size=6, shape=21) + 
  scale_fill_manual(values = c("#0081ffff","#ffff00ff","#D81B60","#ffff00ff","black","#ffff00ff","#4477aaff","#0081ffff","#ffff00ff","#F0E442","#ffff00ff"), 
                    name="Haplogroup") +
  expand_limits(x=.03) + 
  scale_color_manual(values=c("white", "black"), name = "Place") + 
  #  scale_color_manual(values = c("#66BB3C", "#D81B60", "grey50", "#4477AA", "#F0E442"), 
  #                    name="Haplogroup") +
#  scale_shape_manual(values = c(21, 22), name = "Place") +
  guides(fill = guide_legend(override.aes=list(shape=21)),
         color = guide_legend(override.aes=list(col="black"))) + 
  theme(legend.position = "left") -> pnCOI
pnCOI







#18S
#Nnet1 <- read.nexus.networx("E:/Emar18s.nex")
Nnet1 <- read.nexus.networx("Evi_18s_tree.nex")

pn1 <- 
  ggsplitnet(Nnet1, col="black") + 
  geom_treescale(x=0, y=0.005, offset=.0001) + 
  coord_fixed()
#  geom_tiplab2() + 
#  geom_point(aes(shape="21", color=isTip), size=2)

tips <- pn1$data[pn1$data$isTip, ]
tips$group <- sapply(tips$label, function(x) strsplit(x, split = "_")[[1]][3])
tips$group[which(is.na(tips$group))] <- "out"

tips %>% count(x, y, group) -> tips.occur


tips$place <- sapply(tips$label, function(x) strsplit(x, split = "_")[[1]][1])
tips$place <- ifelse(startsWith(tips$place, "MK") | tips$place == "Evi", "Lake Baikal", "Angara River")


tips %>% count(x, y, group, place) -> tips.occur
tips.occur <- tips.occur[order(tips.occur$n), ]

pn1 + 
  geom_point(data = tips.occur, aes(x=x, y=y, fill=group), size=6, shape=21) + 
  scale_fill_manual(values = c("#913030ff", "#ed1e48ff","black" ,"#4477aaff", "#F0E442"), 
                    name="Haplogroup") +
  #expand_limits(x=.03) + 
  scale_color_manual(values=c("white", "black"), name = "Place") + 
  #  scale_color_manual(values = c("#66BB3C", "#D81B60", "grey50", "#4477AA", "#F0E442"), 
  #                    name="Haplogroup") +
  #  scale_shape_manual(values = c(21, 22), name = "Place") +
  guides(fill = guide_legend(override.aes=list(shape=21)),
         color = guide_legend(override.aes=list(col="black"))) + 
  theme(legend.position = "left") -> pn18s
pn18s





#COI
Nnet <- read.nexus.networx("E:/Evi_COI.nex")

pn <- 
  ggsplitnet(Nnet, col="black") + 
  geom_treescale(x=-.04, y=.035, offset=.001) + 
  coord_fixed()
#  geom_tiplab2() + 
#  geom_point(aes(shape="21", color=isTip), size=2)

tips <- pn$data[pn$data$isTip, ]
tips$group <- sapply(tips$label, function(x) strsplit(x, split = "_")[[1]][3])
tips$group[which(is.na(tips$group))] <- "out"

tips %>% count(x, y, group) -> tips.occur

tips$place <- sapply(tips$label, function(x) strsplit(x, split = "_")[[1]][2])
tips$place <- ifelse(startsWith(tips$place, "MK") | tips$place == "Evi", "Lake Baikal", "Angara River")


tips %>% count(x, y, group, place) -> tips.occur
tips.occur <- tips.occur[order(tips.occur$n), ]

pn + 
  geom_point(data = tips.occur, aes(x=x, y=y, fill=group), size=6, shape=21) + 
  scale_fill_manual(values = c("#D81B60","black","#4477aaff","#F0E442", '#F0E442'), 
                    name="Haplogroup") +
  expand_limits(x=.03) + 
  scale_color_manual(values=c("white", "black"), name = "Place") + 
  #  scale_color_manual(values = c("#66BB3C", "#D81B60", "grey50", "#4477AA", "#F0E442"), 
  #                    name="Haplogroup") +
  #  scale_shape_manual(values = c(21, 22), name = "Place") +
  guides(fill = guide_legend(override.aes=list(shape=21)),
         color = guide_legend(override.aes=list(col="black"))) + 
  theme(legend.position = "left") -> pnCOI
pnCOI






#COI_E
Nnet <- read.nexus.networx("E:/Evi_COI_E_tree.nex")

pn <- 
  ggsplitnet(Nnet, col="black") + 
  geom_treescale(x=-.04, y=.035, offset=.001) + 
  coord_fixed()
#  geom_tiplab2() + 
#  geom_point(aes(shape="21", color=isTip), size=2)

tips <- pn$data[pn$data$isTip, ]
tips$group <- sapply(tips$label, function(x) strsplit(x, split = "_")[[1]][3])
tips$group[which(is.na(tips$group))] <- "out"

tips %>% count(x, y, group) -> tips.occur

tips$place <- sapply(tips$label, function(x) strsplit(x, split = "_")[[1]][2])
tips$place <- ifelse(startsWith(tips$place, "MK") | tips$place == "Evi", "Lake Baikal", "Angara River")


tips %>% count(x, y, group, place) -> tips.occur
tips.occur <- tips.occur[order(tips.occur$n), ]

pn + 
  geom_point(data = tips.occur, aes(x=x, y=y, fill=group), size=6, shape=21) + 
  scale_fill_manual(values = c("#f09cabff"), 
                    name="Haplogroup") +
  expand_limits(x=.03) + 
  scale_color_manual(values=c("white", "black"), name = "Place") + 
  #  scale_color_manual(values = c("#66BB3C", "#D81B60", "grey50", "#4477AA", "#F0E442"), 
  #                    name="Haplogroup") +
  #  scale_shape_manual(values = c(21, 22), name = "Place") +
  guides(fill = guide_legend(override.aes=list(shape=21)),
         color = guide_legend(override.aes=list(col="black"))) + 
  theme(legend.position = "left") -> pnCOI
pnCOI



#COI_S
Nnet <- read.nexus.networx("E:/Evi_COI_S_tree.nex")

pn <- 
  ggsplitnet(Nnet, col="black") + 
  geom_treescale(x=-.04, y=.035, offset=.001) + 
  coord_fixed()
#  geom_tiplab2() + 
#  geom_point(aes(shape="21", color=isTip), size=2)

tips <- pn$data[pn$data$isTip, ]
tips$group <- sapply(tips$label, function(x) strsplit(x, split = "_")[[1]][3])
tips$group[which(is.na(tips$group))] <- "out"

tips %>% count(x, y, group) -> tips.occur

tips$place <- sapply(tips$label, function(x) strsplit(x, split = "_")[[1]][2])
tips$place <- ifelse(startsWith(tips$place, "MK") | tips$place == "Evi", "Lake Baikal", "Angara River")


tips %>% count(x, y, group, place) -> tips.occur
tips.occur <- tips.occur[order(tips.occur$n), ]

pn + 
  geom_point(data = tips.occur, aes(x=x, y=y, fill=group), size=6, shape=21) + 
  scale_fill_manual(values = c("#4477aaff"), 
                    name="Haplogroup") +
  expand_limits(x=.03) + 
  scale_color_manual(values=c("white", "black"), name = "Place") + 
  #  scale_color_manual(values = c("#66BB3C", "#D81B60", "grey50", "#4477AA", "#F0E442"), 
  #                    name="Haplogroup") +
  #  scale_shape_manual(values = c(21, 22), name = "Place") +
  guides(fill = guide_legend(override.aes=list(shape=21)),
         color = guide_legend(override.aes=list(col="black"))) + 
  theme(legend.position = "left") -> pnCOI
pnCOI




#COI_W
Nnet <- read.nexus.networx("E:/Evi_COI_W_tree.nex")

pn <- 
  ggsplitnet(Nnet, col="black") + 
  geom_treescale(x=-.04, y=.035, offset=.001) + 
  coord_fixed()
#  geom_tiplab2() + 
#  geom_point(aes(shape="21", color=isTip), size=2)

tips <- pn$data[pn$data$isTip, ]
tips$group <- sapply(tips$label, function(x) strsplit(x, split = "_")[[1]][3])
tips$group[which(is.na(tips$group))] <- "out"

tips %>% count(x, y, group) -> tips.occur

tips$place <- sapply(tips$label, function(x) strsplit(x, split = "_")[[1]][2])
tips$place <- ifelse(startsWith(tips$place, "MK") | tips$place == "Evi", "Lake Baikal", "Angara River")


tips %>% count(x, y, group, place) -> tips.occur
tips.occur <- tips.occur[order(tips.occur$n), ]

pn + 
  geom_point(data = tips.occur, aes(x=x, y=y, fill=group), size=6, shape=21) + 
  scale_fill_manual(values = c("#e3ff7aff", "#f5ca07ff"), 
                    name="Haplogroup") +
  expand_limits(x=.03) + 
  scale_color_manual(values=c("white", "black"), name = "Place") + 
  #  scale_color_manual(values = c("#66BB3C", "#D81B60", "grey50", "#4477AA", "#F0E442"), 
  #                    name="Haplogroup") +
  #  scale_shape_manual(values = c(21, 22), name = "Place") +
  guides(fill = guide_legend(override.aes=list(shape=21)),
         color = guide_legend(override.aes=list(col="black"))) + 
  theme(legend.position = "left") -> pnCOI
pnCOI


