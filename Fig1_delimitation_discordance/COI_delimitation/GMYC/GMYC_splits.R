#install.packages("splits", repos="http://R-Forge.R-project.org")
#install.packages("paran")

#https://tmfujis.wordpress.com/2013/04/23/how-to-run-gmyc/

library(splits)

tr <- read.tree("./EviCOI_coalescent_const-Evi_COI_trimmed_wEve.nwk")
result <- gmyc(tr)
summary(result)

plot(result)

spec.list(result)
write.csv(x = spec.list(result), file="sGMYC_spec_list.csv")
