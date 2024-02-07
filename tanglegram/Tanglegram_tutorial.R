# Tangle gram tutorial

library(ggplot2)
library(ggtree)
library(phangorn)
library(dplyr)

tree1 <- read.tree('Tanglegram/tree1.nwk')
tree2 <- read.tree('Tanglegram/tree2')

meta <- read.table('Tanglegram/meta.csv', sep=',', header=T)

# Midpoint rooting
tree1 <- midpoint(tree1)
tree2 <- midpoint(tree2)


t1 <-ggtree(tree1)  %<+%  meta + geom_tiplab()
t2 <- ggtree(tree2)  %<+%  meta + geom_tiplab()

t1
t2

# Extract data
d1 <- t1$data
d2 <- t2$data


d1$tree <-'t1'
d2$tree <-'t2'

# Right now it does not show the tree label from right 
d2$x <- max(d2$x) - d2$x + max(d1$x) +  max(d1$x)*0.3
pp <- t1 + geom_tree(data=d2)
pp$data

dd <- bind_rows(d1, d2) %>% 
  filter(isTip == TRUE)
dd1 <- as.data.frame(dd) 

head(dd1)


# Conditionally join the tips from both tree
green_tree <- dd1[which(dd1$Genotype == 'Green'), ]
pp + geom_line(aes(x, y, group=label), data=green_tree, color='#009E73') 

pp + geom_line(aes(x, y, group=label), data=dd1)
