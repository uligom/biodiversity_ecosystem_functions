[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.13898192.svg)](https://doi.org/10.5281/zenodo.13898192)

Code and input data to reproduce the analysis described in the manuscript: "Satellite remote sensing reveals the footprint of biodiversity on multiple ecosystem functions across the NEON eddy covariance network" by Gomarasca et al., in review at Environmental Research: Ecology.

All analyses are described in meta_script.Rmd, including a listing of all scripts necessary to reproduce the results.
The data folder is included and should be unzipped from the data_UNZIPME.zip file. Individual README.txt files in the data folders link to the original
data repositories were all data can be found or calculated from.
Available data for the statistical analyses is provided in data/inter/data_efps_clim_struct_biodiv_v06.06.csv (after unzipping the data_UNZIPME.zip folder).
Input data for single multimodel inference analyses is provided in data/output/data_mumin_input_xx_yy_v11.06.06_zz.csv, 
where xx stands for the type of analysis (all = all available predictors, main = a-posteriori analysis as described in the article,
no = analysis including no biodiversity variable, ground = analysis including only ground-measured biodiversity variable,
satellite = analysis including only remotely-sensed biodiversity variable); yy stands for the type of Rao Q used (ndvi = RaoQ calculated from NDVI, nirv = RaoQ calculated from NIRv,
bands = RaoQ calculated from all Sentinel 2 bands); and zz stands for the predicted ecosystem functional property.
