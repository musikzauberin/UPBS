
# Added ggplots

Old script | Updated/New script | Newly Added | New Plot Name | Old Plot Name | 
--- | --- | --- |---: |---: |
GenomeSize.R | GenomeSize_plots.R | Scatterplot: <br/> Total Length ~ Genome Size | GenomeSize_ggplot.pdf | GenomeSize.pdf | 
regress.R | regress_plots.R | Scatterplot: Body Weight ~ Genome Size & LM | Diagmod_ggplot.pdf | DiagMod.pdf | 
glm.R | glm_plots.R | Boxplot: logColonyCount~Strain, separated by Treatment | PracDataBoxplot.pdf | PracDataBoxplot_ggplot.pdf <br/> PracDataBoxplot_ggplot2.pdf |
glm.R | glm_plots.R | Barplot: logColonyCount~Strain| PracDataBarplot.pdf | PracDataBarplot_ggplot.pdf|
PP_Lattice.R | PP_ggplot.R | **Scatterplot:** PreyMass~PredatorMass <br/>  **Ratio between PredatorMass&PreyMass:** <br/> Jitterplot,Boxplot,Histogram, Density plot, Multi-faceted plots, Subset of Multi-faceted plots| PP_ggplot.pdf <br/> PP_boxplot.pdf <br/> PP_jitterplot.pdf <br/> PP_histogram.pdf <br/> PP_density.pdf <br/> PP_multi.pdf <br/> PP_multisubset.pdf|--- |

# New datasets

Source | Data file | Script | Data Management | Visualisation | Analysis | Comments |
--- | --- |--- |--- |--- |--- |--- |
--- | PoundHillData.csv | PoundHillData.R | melt, tapply, ddply, cbind, for loop, replace values, extract rows| ggplots: PoundHill_ShannonIndexes.pdf PoundHill_SpeciesRichness.pdf | Species Richness, Shannon diversity index | two different methods to calculate Shannon, with and without for loop|
Levin et al (2012) Biodiversity Genomics final year module | Celegans_transcriptomics.csv (Original Data file : original_evodevomics.csv) | Celegans_transcriptomics.R | melt, reshape, extract columns | Celegans_summary.pdf, Celegans_heatmap.pdf | linearmodel and quadraticmodels, creating function and using dlply to iterate function over multiple genes, distance-matrixes | Note: script and data edited from those provided in BGmodule, analysis may be slightly too complicated for undergraduate |
Data folder in SilBioComp Old Script:TimeSer.R | ClimateData.csv  | ClimateData.R | melt, adding a new variable using R | boxplot, facets | simple anova |--- |
