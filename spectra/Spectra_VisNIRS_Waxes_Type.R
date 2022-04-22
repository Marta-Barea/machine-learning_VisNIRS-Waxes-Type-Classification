###############################################################################
####### Application of a system based on Vis-NIRS and Machine Learning ########
################### for the analysis of petroleum waxes #######################
######################## Marta Barea-Sepúlveda ################################
###############################################################################

###############################################################################
######################## Spectra Representation ###############################
###############################################################################

# Loading Packages

library(doParallel)
library(readxl)
library(data.table)
library(ggplot2)
library(prospectr)
library(egg)

# Loading Parallelization

cl <- makePSOCKcluster(8)
registerDoParallel(cl)

# Loading data

pw_data <- read_excel("~/Documents/Doctorado/Tesis Doctoral/Investigación Cepsa/Vis-NIR/XDS-NIR_FOSS/Estudio según Tipo de Parafina e Hidrotratamiento/NIRS_PW_Type_Hydrotreating.xlsx", 
                      sheet = "PW_Type")

# Original NIR Spectra

df <- reshape2::melt(pw_data, "Sample")

spectra_plot <- ggplot(data = df, aes(x = variable, y = value, color = Sample)) + 
  geom_line() +
  labs(x = "Wavelength (nm)", y = "Absorbance") +
  theme_test()+ 
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 8, hjust = 1, angle = 90),
        axis.title = element_text(size = 8)) +
  scale_x_discrete(limits = df$variable,
                   breaks = df$variable[seq(1, length(df$variable), by = 3000)])

spectra_plot

# First derivative spectra

pw_data$Sample <- as.factor(pw_data$Sample)

sgvec <- savitzkyGolay(X = pw_data[,-1], p = 3, w = 11, m = 1)
pw_sg <- cbind.data.frame(Sample = pw_data$Sample, sgvec)

df_2 <- reshape2::melt(pw_sg, "Sample")

savitzky_plot <- ggplot(data = df_2, aes(x = variable, y = value, color = Sample)) + 
  geom_line() +
  labs(x = "Wavelength (nm)", y = "Absorbance") +
  theme_test() + 
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 8, hjust = 1, angle = 90),
        axis.title = element_text(size = 8)) +
  scale_x_discrete(limits = df_2$variable,
                   breaks = df_2$variable[seq(1, length(df_2$variable), by = 3000)])

savitzky_plot

# Combining plots

spectrasvitzky_plot <- ggarrange(spectra_plot, savitzky_plot,
                                  ncol = 1, nrow = 2,
                                  labels = c("A", "B"))

spectrasvitzky_plot

# Stop Parallelization

stopCluster(cl)