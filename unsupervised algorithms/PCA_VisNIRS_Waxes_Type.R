####### Application of a system based on Vis-NIRS and Machine Learning ########
################### for the analysis of petroleum waxes #######################
######################## Marta Barea-Sepúlveda ################################
###############################################################################

###############################################################################
#################### Principal Components Analysis (PCA) ######################
###############################################################################

# Loading Packages

library(doParallel)
library(readxl)
library(prospectr)
library(cluster)
library(factoextra)
library(data.table)
library(egg)

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

pw_mw <- as.data.frame(pw_sg)
pw_mw$Sample = as.numeric(as.factor(pw_sg$Sample))

# PCA

pw_pca <- prcomp(pw_mw[,-1], scale = FALSE)

# Visualizing PCA results

pw_pca
summary(pw_pca)

# Visualizing eigenvalues (scree plot)

fviz_eig(pw_pca,
         xlab = "Principal Components (PCs)",
         ylab = "Explained Variance (%)",
         main = "",
         addlabels = TRUE,
         ggtheme = theme_minimal(),
         barcolor = "#4EBBBA",
         barfill = "#4EBBBA",
         linecolor = "#000000")

# Score plot for PC1 and PC2

scores_pca <- cbind.data.frame(predict(pw_pca),
                               waxes = pw_data$Sample)

m_labels <- as.matrix(pw_data[,-1])
rownames(m_labels) <- pw_data$Sample

cols <- c("#EA6A60", "#4EBBBA")
cols

scatter_plot <- ggplot(scores_pca, aes(x = PC1, y = PC2, col = waxes, shape = waxes, label = rownames(m_labels))) +
  geom_hline(yintercept = 0, lty = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 0, lty = "dashed", alpha = 0.5) +
  geom_point(alpha = 1, size = 3.75) +
  guides(color = guide_legend(title = "Wax Type"), shape = guide_legend(title = "Wax Type")) +
  labs(x = "PC1 (56.8%)",y = "PC2 (26.7%)", title = "") + 
  scale_color_manual(values = cols) +
  scale_shape_manual(values = c(15, 17)) +
  theme(axis.title = element_text(size = 12), 
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12)) +
  theme_test() 

scatter_plot

# Loadings plot

loadings <- cbind.data.frame(pw_pca$rotation[,c(1,2)])
setDT(loadings, keep.rownames = TRUE)[]

ld <- melt(loadings, "rn")

loadings_plot <- ggplot(ld, aes(x = rn, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") + 
  theme_test()+ 
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 8, hjust = 1, angle = 90),
        axis.title = element_text(size = 8)) +
  labs(x = "Wavelength (nm)", y = "Loadings PCs", title = "") +
  scale_x_discrete(limits = loadings$rn,
                   breaks = loadings$rn[seq(1, length(loadings$rn), by = 50)]) +
  scale_fill_manual(values = c("#4DAD83", "#ED9525")) +
  geom_hline(yintercept = c(0.05, -0.05), linetype = "dotted")
  
loadings_plot

# Combining plots

scatterloadings_plot <- ggarrange(scatter_plot, loadings_plot,
                                  ncol = 1,
                                  nrow = 2,
                                  labels = c("A", "B"))

scatterloadings_plot

# Stop Parallelization

stopCluster(cl)