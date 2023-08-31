# Loading Packages

library(doParallel)
library(readxl)
library(prospectr)
library(cluster)
library(purrr)
library(factoextra)

# Loading Parallelization

cl <- makePSOCKcluster(8)
registerDoParallel(cl)

# Loading data

pw_data <- read_excel("~/Documents/Doctorado/Tesis Doctoral/Investigación Cepsa/Vis-NIR/XDS-NIR_FOSS/Estudio según Tipo de Parafina e Hidrotratamiento/NIRS_PW_Type_Hydrotreating.xlsx", 
                      sheet = "PW_Type")
                  
# First derivative and Savitzky Golay Smoothing

pw_data$Sample <- as.factor(pw_data$Sample)

sgvec <- savitzkyGolay(X = pw_data[,-1], p = 3, w = 11, m = 1)
pw_sg <- cbind.data.frame(Sample = pw_data$Sample, sgvec)

pw_mw <- as.matrix(pw_sg[,-1])
rownames(pw_mw) <- pw_sg$Sample

# Linkage methods to assess

m <- c("average", "single", "complete", "ward")
names(m) <- c("average", "single", "complete", "ward")

#Compute coefficient

ac <- function(x) {
  agnes(pw_mw, method = x)$ac
}

# Print method and coefficient

map_dbl(m, ac)      

# Dissimilarity matrix

d <- dist(pw_mw, method = "euclidean")

# Hierarchical clustering 

hc1 <- hclust(d, method = "ward.D2")

# Dendrogram

set.seed(5665)

dendrogram <- fviz_dend(x = hc1, 
                        show_labels = TRUE, 
                        cex = 0.7,
                        lwd = 0.5,
                        main = "",
                        xlab = "Samples",
                        ylab = "Dendogram using Ward's linkage and Euclidean distance",
                        sub = "",
                        ggtheme = theme_classic(),
                        horiz = FALSE,
                        k = 2,
                        k_colors = c("#4EBBBA", "#EA6A60"),
                        color_labels_by_k = TRUE,
                        type = "rectangle")

dendrogram
 
# Stop Parallelization

stopCluster(cl)