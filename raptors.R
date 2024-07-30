library(tidyverse)
library(FactoMineR)
library(factoextra)
library(ggpubr)

raptors<-read_csv("csv/raptors.csv") %>% slice(-(9:9))
raptors_res<-read_csv("csv/raptors_res.csv")

raptors_res %>% 
  column_to_rownames(var = "name")->rap_res
raptors %>% rename("diet_3"="3_diet", "diet_6"="6_diet")->raptors
raptors %>% select(!c(diet_3, diet_6)) %>% 
  column_to_rownames(var = "name")->rap

raptors %>% as_tibble() %>%
  select(name, diet_3) %>%
  column_to_rownames(var = "name")->diet_3
raptors %>% as_tibble() %>%
  select(name, diet_6) %>%
  column_to_rownames(var = "name")->diet_6
raptors %>% mutate(group = str_remove_all(name, pattern = "[:digit:]")) %>%
  mutate(group = str_to_upper(group, locale = "en")) %>%
  select(name, group) %>%
  column_to_rownames(var = "name")->grouping
  
#===============================================================================
res.pca<-PCA(rap, scale.unit = TRUE, ncp = 5, ind.sup = NULL,
             quanti.sup = NULL, quali.sup = NULL, row.w = NULL,
             col.w = NULL, graph = TRUE, axes = c(1,2))

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, NA)) # visualize scree plot

fviz_pca_var(res.pca) # visualize individuals
fviz_pca_ind(res.pca) # visualize individuals

#VARIABLES
fviz_pca_var(
  X = res.pca,
  axes = c(1, 2),
  geom = c("arrow", "text"),
  geom.var = c("arrow", "text"),
  repel = TRUE,
  col.var = "blue",
  fill.var = "white",
  alpha.var = 1,
  col.quanti.sup = "blue",
  col.circle = "grey70",
  select.var = list(name = NULL, cos2 = NULL, contrib = NULL)) # visualize variables

# Quality of representation
fviz_cos2(res.pca, choice = "var", axes = 1:2)

fviz_pca_var(
  X = res.pca,
  axes = c(1, 2),
  geom = c("arrow", "text"),
  geom.var = c("arrow", "text"),
  repel = TRUE,
  col.var = "cos2",
  gradient.cols = c("red", "yellow", "blue"),
  alpha.var = 1,
  col.quanti.sup = "blue",
  col.circle = "blue",
  select.var = list(name = NULL, cos2 = NULL, contrib = NULL)) 
# Color variables according to their quality of representation (cos2)

# Contributions of variables
fviz_contrib(res.pca, choice = "var", axes = 1:2)

fviz_pca_var(
  X = res.pca,
  axes = c(1, 2),
  geom = c("arrow", "text"),
  geom.var = c("arrow", "text"),
  repel = TRUE,
  col.var = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  alpha.var = 1,
  col.quanti.sup = "blue",
  col.circle = "blue",
  select.var = list(name = NULL, cos2 = NULL, contrib = NULL)) 
# Color variables according to their contribution

#INDIVIDUALS
fviz_pca_ind(
  X = res.pca,
  axes = c(1, 2),
  geom = c("point", "text"),
  geom.ind = c("point", "text"),
  repel = TRUE,
  habillage = "none",
  palette = NULL,
  addEllipses = FALSE,
  col.ind = "cos2",
  fill.ind = "white",
  col.ind.sup = "blue",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  alpha.ind = 1,
  select.ind = list(name = NULL, cos2 = NULL, contrib = NULL))

fviz_pca_ind(
  X = res.pca,
  axes = c(1, 2),
  geom = c("point", "text"),
  geom.ind = c("point", "text"),
  repel = TRUE,
  habillage = "none",
  palette = NULL,
  addEllipses = FALSE,
  col.ind = "contrib",
  fill.ind = "white",
  col.ind.sup = "blue",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  alpha.ind = 1,
  select.ind = list(name = NULL, cos2 = NULL, contrib = NULL))

fviz_pca_ind(res.pca, pointsize = "cos2",
             pointshape = 21, fill = "#E7B800",
             repel = TRUE) # Avoid text overlapping (slow if many points)
fviz_pca_ind(res.pca, col.ind = "cos2", pointsize = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) # Avoid text overlapping (slow if many points)

# Quality of representation
fviz_cos2(res.pca, choice = "ind", axes = 1:2)


# Color individuals by the continuous variable
fviz_pca_ind(res.pca, col.ind = rap$mandgl,
             gradient.cols = c("blue", "yellow", "red"),
             legend.title = "Cont.Var")

# Color individuals by groups
pca.ind<-fviz_pca_ind(res.pca, axes = 1:2,
                      geom.ind = c("point", "text"), # show points only (nbut not "text")
                      col.ind = grouping$group, # color by groups
                      addEllipses = FALSE,
                      ellipse.type = "convex",
                      labelsize = 5,
                      pointshape = 20,
                      pointsize = 3,
                      palette = "simpsons",
                      repel = TRUE,
                      mean.point = FALSE,
                      axes.linetype = "dashed",
                      legend.title = "Groups")

ggpar(
  pca.ind,
  palette = "simpsons",
  gradient.cols = NULL,
  main = "PCA with Species",
  submain = NULL,
  caption = NULL,
  xlab = "PCA1",
  ylab = "PCA2",
  title = NULL,
  subtitle = NULL,
  font.main = NULL,
  font.submain = NULL,
  font.x = NULL,
  font.y = NULL,
  font.caption = NULL,
  font.title = NULL,
  font.subtitle = NULL,
  font.family = "",
  xlim = NULL,
  ylim = NULL,
  xscale = c("none", "log2", "log10", "sqrt"),
  yscale = c("none", "log2", "log10", "sqrt"),
  format.scale = FALSE,
  legend = "right",
  legend.title = "Species",
  font.legend = 12,
  ticks = TRUE,
  tickslab = TRUE,
  font.tickslab = NULL,
  font.xtickslab = NULL,
  font.ytickslab = NULL,
  x.text.angle = NULL,
  y.text.angle = NULL,
  xtickslab.rt = NULL,
  ytickslab.rt = NULL,
  xticks.by = NULL,
  yticks.by = NULL,
  rotate = FALSE,
  orientation = c("vertical", "horizontal", "reverse"),
  ggtheme = NULL)

fviz_pca_biplot(res.pca) # visualize biplot

fviz_pca_biplot(
  X = res.pca,
  axes = 1:2,
  geom.ind = c("point"),
  geom.var = c("arrow", "text"),
  col.ind = grouping$group,
  fill.ind = grouping$group,
  col.var = "black",
  fill.var = "black",
  gradient.cols = NULL,
  label = "all",
  invisible = "none",
  repel = TRUE,
  habillage = "none",
  palette = "simpsons",
  addEllipses = FALSE,
  ellipse.type = "convex",
  title = "PCA - Biplot",
  pointshape = 21,
  pointsize = 3,
  labelsize = 5,
  arrowsize = 0.7,
  mean.point = FALSE,
  legend.title = "Species",
  xlab = "PCA1 (37.8%)",
  ylab = "PCA2 (17%)")
ggsave("plot.png", width = 10, height = 5)

# Supplementary elements
res.pca1<-PCA(diss, scale.unit = TRUE, ncp = 5, ind.sup = NULL,
              quanti.sup = NULL, quali.sup = 32:34, row.w = NULL,
              col.w = NULL, graph = TRUE, axes = c(1,2))

fviz_pca_ind(res.pca1, habillage = 34,
             addEllipses =FALSE, ellipse.type = "confidence", mean.point = FALSE,
             palette = "simpsons", repel = TRUE)

# Filtering results
# Visualize variable with cos2 >= 0.6
fviz_pca_var(res.pca, select.var = list(cos2 = 0.6))

# Top 5 active variables with the highest cos2
fviz_pca_var(res.pca, select.var= list(cos2 = 5))

# Select by names
name <- list(name = c("elev", "slope"))
fviz_pca_var(res.pca, select.var = name)

# top ... contributing individuals and variables
fviz_pca_biplot(res.pca, 
                repel = TRUE, 
                col.ind = com_types$com, 
                palette = "simpsons",
                mean.point = FALSE,
                col.var = "black",
                pointshape = 20,
                pointsize = 3,
                select.ind = list(contrib = 50),
                select.var = list(contrib = 10),
                legend.title = "Community Types",
                ggtheme = theme_minimal())