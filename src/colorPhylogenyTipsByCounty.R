library(phytools)
library(ape)
library(ggtree)
library(treeio)
library(RColorBrewer)
library(pals)

tree <- read.nexus(file = "../../washingtonCountiesCovid19/output/raxml/RAxML_bestTree.testrun.rooted")

tip.county <- unlist(lapply(tree$tip.label, function(x) strsplit(x, split = "_")[[1]][2]))

county.grp <- list()

for (i in unique(tip.county)) {
  # Group tips by subclade label in the tip name
  county.grp[[i]] <- tree$tip.label[grepl(i, tree$tip.label)]
}

tree <- groupOTU(tree, county.grp)

mycolors <- colorRampPalette(brewer.pal(8, "Spectral"))(length(unique(tip.county)))

ggtree(tree) +
  geom_tippoint(aes(color = group)) + 
  scale_color_manual(values=as.vector(polychrome(26)))

beast_file <- system.file("examples/MCC_FluA_H3.tree", package="ggtree")
beast_tree <- read.beast(beast_file)
ggtree(beast_tree, mrsd="2013-01-01") + theme_tree2()

tree <- treeio::read.nexus(file = "output/raxml/RAxML_bestTree.testrun.rooted")

str(tree)
