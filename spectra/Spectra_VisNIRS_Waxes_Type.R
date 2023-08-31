# Loading Packages

library(doParallel)
library(readxl)
library(dplyr)
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

# Apply Multiplicative Scatter Correction (MSC)
pw_data_msc <- pw_data
pw_data_msc[,-1] <- msc(pw_data[,-1])

pw_mean <- aggregate(.~ Sample, pw_data_msc, mean)

# Original NIR Spectra

df <- reshape2::melt(pw_mean, "Sample")

spectra_plot <- ggplot(data = df, aes(x = variable, y = value, color = Sample, group = Sample)) + 
  geom_line() +
  labs(x = "Wavelength (nm)", y = "Absorbance") +
  theme_test() + 
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 8, hjust = 1, angle = 90),
        axis.title = element_text(size = 8)) +
  scale_x_discrete(limits = df$variable,
                   breaks = df$variable[seq(1, length(df$variable), by = 300)])
spectra_plot

# First derivative spectra

pw_data_msc$Sample <- as.factor(pw_data_msc$Sample)

sgvec <- savitzkyGolay(X = pw_data_msc[,-1], p = 3, w = 11, m = 1)
pw_sg <- cbind.data.frame(Sample = pw_data_msc$Sample, sgvec)

pw_sg_mean <- aggregate(.~ Sample, pw_sg, mean)

df_2 <- reshape2::melt(pw_sg_mean, "Sample")

savitzky_plot <- ggplot(data = df_2, aes(x = variable, y = value, color = Sample, group = Sample)) + 
  geom_line() +
  labs(x = "Wavelength (nm)", y = "Relative Signal") +
  theme_test() + 
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 8, hjust = 1, angle = 90),
        axis.title = element_text(size = 8)) +
  scale_x_discrete(limits = df_2$variable,
                   breaks = df_2$variable[seq(1, length(df_2$variable), by = 300)])
savitzky_plot

# Combining plots

spectrasvitzky_plot <- ggarrange(spectra_plot, savitzky_plot,
                                 ncol = 1, nrow = 2,
                                 labels = c("A", "B"))
spectrasvitzky_plot

# Stop Parallelization

stopCluster(cl)
