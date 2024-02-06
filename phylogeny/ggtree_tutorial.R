library(ggplot2)
library(ggtree) # for plotting tree
library(phangorn) # for midpoint root of tree
library(treeio) # for loading special tree format
library(ggnewscale) #for new color/fill scale
library(viridis) #using speacial color scale


vshv.tree <- read.tree('Tree_tutorial/Phylogeny_module_example_data.fasta.aln.treefile')
meta <- read.table('Tree_tutorial/Phylogeny_module_tutorial_meta_data.csv', sep=',', header=T)
vshv.tree <- midpoint(vshv.tree)

vshv.tree <- read.iqtree('Tree_tutorial/Phylogeny_module_example_data.fasta.aln.treefile')
vshv.tree@phylo <- midpoint(vshv.tree@phylo)

# Just tree
plot(vshv.tree@phylo)


# Lets add tip point
t1 <- ggtree(vshv.tree) %<+% meta +
  geom_tippoint(aes(color=Host))

# Make it circular
t1 <- ggtree(vshv.tree, layout='circular')  %<+% meta +
  geom_tippoint(aes(color=Host)) 

# Add bootstrap support
t1$data$bootstrap <- '0'
t1$data[which(t1$data$SH_aLRT >= 70 & t1$data$UFboot  >= 70),]$bootstrap <- '1'

t1 <- t1 + new_scale_color() +
  geom_tree(aes(color=bootstrap == '1')) +
  scale_color_manual(name='Bootstrap', values=setNames( c('black', 'grey'), c(T,F)), guide = "none")

t1

# Lets add histogram
meta.water <- as.data.frame(meta[,'Water'])
colnames(meta.water) <- 'Water'
rownames(meta.water) <- meta$Strain

meta.year <- as.data.frame(meta[,'Year'])
colnames(meta.year) <- 'Year'
rownames(meta.year) <- meta$Strain

# Categorical variable
t2 <- gheatmap(t1, meta.water, width=0.2) + 
  scale_fill_viridis_d(option="A", name="Country") +
  new_scale_fill()
  
# Continuous variable
t2 <- gheatmap(t2, meta.year, width=0.1, offset=0.05) + 
  scale_fill_viridis(option="D", name="Year") +
  new_scale_fill()

# Plot tiplab2
t2 + geom_tiplab2(aes(label=Country), color="gray40", offset=0.003, size=3)

