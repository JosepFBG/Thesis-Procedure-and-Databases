# Thesis-Procedure-and-Databases

# From voting booth to nation-wide: Measuring and Spatially Interpolating Electoral Polarization in the Netherlands

This repository contains the R code used to prepare Dutch election data, link voting results to polling-station locations, calculate electoral polarization, and analyze the spatial structure of voting outcomes in the Netherlands.

## Repository structure

The main scripts should be run in numerical order. If the scripts are not run in this order they will not be able to run because they outputs produced are required for some of the later files to run As such, the required run of the files is, in order:

```text
Part_1_Record_Linkage_&_Data_Cleanup.R
Part_2_Incorrect_Linkages_Corrections.R
Part_3_Voting_Data_Preparation.R
Part_4_ER_Index_Calculation_Every_Level.R
Part_5_Mapping_of_Administrative_Boundaries.R
Part_6_IDW_and_Assumptions_Check.R
Part_7_SACD.R
Part_8_Divergence_Measurement.R
```

## Required input files

The code expects the required raw data files to be available in the working directory. These include, among others:

```text
TK2025_Stemmen_Per_Lijst_Per_Stembureau.csv
Stembureau_Locatie_Nederland_VK_2025.csv
Kieskompas partijcoordinaten 2025(1).xlsx
```
There is are 4 additional required database which cannot be uploaded to this Git due to their size. That would be the BAG, cbs grid size databases, and the administrative divisions data. To get these databases please go to:

* https://service.pdok.nl/lv/bag/atom/bag.xml and download the "BAG (EPSG:28992) Geopackage" file. 
* https://www.cbs.nl/nl-nl/dossier/nederland-regionaal/geografische-data/kaart-van-500-meter-bij-500-meter-met-statistieken to get the cbsgebiedsindelingen2025.gpkg
* https://www.cbs.nl/nl-nl/dossier/nederland-regionaal/geografische-data/kaart-van-100-meter-bij-100-meter-met-statistieken to get the cbs_vk100_2024_v1.gpkg
* https://www.cbs.nl/nl-nl/dossier/nederland-regionaal/geografische-data/cbs-gebiedsindelingen to get the cbsgebiedsindelingen2025.gpkg

The scripts assume that files are in the main project folder. If the files are stored somewhere else, update the paths inside the scripts before running them.

Multiple output folders will be created with files that will be produced so that the entire script does not need to be rerun. 

The folder called Spatial_Objects_Ready_For_Users has files of interest for users that might not be interested in running the scripts and are seraching only for the outputs. It is important to note that the 100m grid cells results could not be stored there due to their size. As such, it will be necessary to run the scripts to produce these outputs.

The scripts are designed for reproducibility, but they assume that the input files use the same names and structures as in the original project. The coordinate reference system used for the spatial analysis is RD New / EPSG:28992.

## Required R packages

The main packages used in the workflow are:

```r
install.packages(c(
  "readr",
  "readxl",
  "dplyr",
  "tidyr",
  "stringr",
  "sf",
  "ggplot2",
  "patchwork",
  "gstat",
  "spdep",
  "spatstat.geom",
  "spatstat.explore",
  "compositions",
  "reclin2"
))
```

## How to run the analysis

### Step 1: Record linkage and cleanup


This script prepares the voting data and polling-station location data for linkage.

### Step 2: Correct incorrect linkages

This script corrects unresolved or incorrect matches between voting results and polling-station locations.
### Step 3: Create the final geospatial voting dataset

This script prepares the final party-level voting dataset. It combines polling stations that share the same physical location, adds Kieskompas party coordinates, attaches COROP, province, and municipality information, and saves the final geospatial dataset.

### Step 4: Calculate Esteban-Ray polarization scores

This script calculates Esteban-Ray polarization scores at several spatial levels, including polling-station locations, municipalities, COROP areas, provinces, and the national level.

### Step 5: Map administrative-level polarization

This script maps the Esteban-Ray scores at administrative levels such as municipality, COROP, province, and national level.

The maps are saved in the `figures/` folder when the saving code is run.

### Step 6: Run IDW interpolation and diagnostics

This script interpolates Esteban-Ray values across CBS 100m and 500m populated grid cells using inverse distance weighting.

It also runs spatial diagnostics, including Moran's I, variograms, leave-one-out validation, and IDW power selection.

### Step 7: Run SACD analysis

This script performs the Spatial Analysis of Compositional Data.

### Step 8: Run KL divergence analysis

This script calculates Kullback-Leibler divergence between vote-share compositions at different administrative levels.

## Recreating maps without rerunning everything

Some parts of the analysis can take time to run. For this reason, several scripts save intermediate or final outputs.

For example, if the IDW GeoPackages already exist, the IDW maps can be recreated directly from:

```text
cbs_500m_idw.gpkg
cbs_100m_idw.gpkg
```

