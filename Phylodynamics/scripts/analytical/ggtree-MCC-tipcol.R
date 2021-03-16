#Lambodhar Damodaran
# 3/2021
# Visualize BEAST tree with assc. metadata states as tips from tsv using ggtree


library(ggplot2)
library(ggtree)
library(treeio)


beast_tree <- read.beast("washington_covid_200M_combined_trees_annotated.tree")


### create a data frame with first column as taxa name and second column as discrete character to color by
data <- read.csv("trait-metadata.tsv", header = TRUE, sep = '\t')
df <- as.data.frame(data)



p1 <- ggtree(beast_tree, mrsd='2020-10-02') +
  theme_tree2(panel.grid.major.x=element_line()) + 
  ggtitle("BEAST phylogeny - COVID-19 - WA state - 2/20/2020 - 10/02/2020") +
  geom_range(range='height_0.95_HPD', color='red', alpha=2, size=.3)

p1


# to add legend and colored tips
p2 <- p1 %<+% df + geom_tippoint(aes(color=corew), size=1, alpha=.75) 

p2

## export high quality figure
ggsave("Tree.jpeg", dpi ="retina" ) 
