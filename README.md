# Rapid Classification of Petroleum Waxes: A Vis-NIR Spectroscopy and Machine Learning Approach

## Description

This repository contains the source code for all data processing and the application of machine learning algorithms used in the article "Rapid Classification of Petroleum Waxes: A Vis-NIR Spectroscopy and Machine Learning Approach".

## Contents

- `spectra/`: Folder containing the spectra data.
- `supervised algorithms/`: Source code for all the supervised machine learning models and experiments.
- `unsupervised algorithms/`: Source code related to unsupervised learning techniques and clustering.
- `App/`: A Shiny application to demonstrate and visualize the findings.

## Requirements

All data analysis was performed with **RStudio (version 4.1.2)**. The software and packages used include:

- **prospectr (version 0.2.3)**: Used for calculating the first derivative for each sample spectrum with the `savitzkyGolay` function.
- **stats (version 4.1.2)**: Utilized for HCA with the `hclust` function, PCA with the `prcomp` function, and one-way ANOVA with the `aov` function.
- **cluster (version 2.1.2)**: Linkage method selection for the HCA established using the `agnes` function.
- **factoextra (version 1.0.7)**: Used for visualizing HCA results with the `fviz_dend` function and for extracting and visualizing the PCA result with the `fviz_eig` function.
- **ggplot2 (version 3.3.5)**: Employed for plotting the scores and loadings of the PCA with the `ggplot` function and generating the spectralprint radar chart.
- **caret (version 6.0-90)**: Utilized for developing the SVM and RF models.
- **graphics (version 4.1.2)**: The `filled.contour` function was used to generate the contour plot for the SVM model.
- **ggiraphExtra (version 0.3.0)**: Assisted in generating the spectralprint radar chart.
- **shiny (version 1.7.1)**: Utilized for developing the web application.

## Usage

1. Clone the repository.
2. Ensure that the necessary versions of RStudio and packages are installed.
3. Navigate to the respective folders and run the scripts to reproduce the results or launch the Shiny app.

