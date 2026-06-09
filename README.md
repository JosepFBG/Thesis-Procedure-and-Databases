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
cbsgebiedsindelingen2025.gpkg
cbs_vk500_2024_v1.gpkg
cbs_vk100_2024_v1.gpkg
```
There is an additional required database which cannot be uploaded to this Git due to its size. That would be the BAG.gpkg. To get this database please go to

https://service.pdok.nl/lv/bag/atom/bag.xml and download the "BAG (EPSG:28992) Geopackage" file. 
The scripts assume that file paths are relative to the project folder. If the files are stored somewhere else, update the paths inside the scripts before running them.

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

Load the required packages inside each script. The scripts already include the relevant `library()` calls.

## How to run the analysis

### Step 1: Record linkage and cleanup

Run:

```r
source("Part_1_Record_Linkage_&_Data_Cleanup.R")
```

This script prepares the voting data and polling-station location data for linkage. It creates unique voting-booth identifiers and writes intermediate files used in the later correction steps.

### Step 2: Correct incorrect linkages

Run:

```r
source("Part_2_Incorrect_Linkages_Corrections.R")
```

This script corrects unresolved or incorrect matches between voting results and polling-station locations. It combines automated correction steps with manual corrections for difficult cases.

### Step 3: Create the final geospatial voting dataset

Run:

```r
source("Part_3_Voting_Data_Preparation.R")
```

This script prepares the final party-level voting dataset. It combines polling stations that share the same physical location, adds Kieskompas party coordinates, attaches COROP, province, and municipality information, and saves the final geospatial dataset.

Important outputs include:

```text
voting_data_kieskompas.csv
complete_geospatial_data.csv
voting_data_kieskompas_analysis_admin.gpkg
```

The GeoPackage contains one row per party at one final physical voting location, including address information, vote counts, party coordinates, administrative tags, and geometry.

### Step 4: Calculate Esteban-Ray polarization scores

Run:

```r
source("Part_4_ER_Index_Calculation_Every_Level.R")
```

This script calculates Esteban-Ray polarization scores for:

* Links-Rechts
* Progressief-Conservatief
* 2D Euclidean ideological distance

Scores are calculated at several spatial levels, including polling-station locations, municipalities, COROP areas, provinces, and the national level.

Important outputs include:

```text
er_stembureau.csv
er_gemeente.csv
er_corop.csv
er_province.csv
er_national.csv
gemeente_party_votes.csv
corop_party_votes.csv
province_party_votes.csv
national_party_votes.csv
```

### Step 5: Map administrative-level polarization

Run:

```r
source("Part_5_Mapping_of_Administrative_Boundaries.R")
```

This script maps the Esteban-Ray scores at administrative levels such as municipality, COROP, province, and national level.

The maps are saved in the `figures/` folder when the saving code is run.

### Step 6: Run IDW interpolation and diagnostics

Run:

```r
source("Part_6_IDW_and_Assumptions_Check.R")
```

This script interpolates Esteban-Ray values across CBS 100m and 500m populated grid cells using inverse distance weighting.

It also runs spatial diagnostics, including Moran's I, variograms, leave-one-out validation, and IDW power selection.

Important outputs include:

```text
moran_results_idw_diagnostics.csv
variogram_results_idw_diagnostics.csv
idw_power_results_all.csv
optimal_power_results.csv
final_idw_error_results.csv
idw_range_checks.csv
residual_summary_by_stembureaus.csv
loo_residuals_500m.csv
loo_residuals_100m.csv
cbs_500m_idw.gpkg
cbs_100m_idw.gpkg
```

The files `cbs_500m_idw.gpkg` and `cbs_100m_idw.gpkg` are especially useful because they allow the IDW maps to be recreated without rerunning the full interpolation procedure.

### Step 7: Run SACD analysis

Run:

```r
source("Part_7_SACD.R")
```

This script performs the Spatial Analysis of Compositional Data. It aggregates party vote shares to CBS grid cells, replaces zero shares, transforms the compositions using ilr coordinates, runs spatial diagnostics, performs IDW interpolation in ilr space, and back-transforms the predictions to party shares.

Important outputs are saved in:

```text
sacd_outputs/
```

This folder contains RDS and CSV files for intermediate and final SACD objects, including transformed compositions, diagnostics, validation summaries, predicted party shares, and party-level error summaries.

### Step 8: Run KL divergence analysis

Run:

```r
source("Part_8_Divergence_Measurement.R")
```

This script calculates Kullback-Leibler divergence between vote-share compositions at different administrative levels.

Important outputs are saved in:

```text
kl_outputs/
```

This folder includes KL divergence tables, summary tables, and top-divergence areas.

## Recreating maps without rerunning everything

Some parts of the analysis can take time to run. For this reason, several scripts save intermediate or final outputs.

For example, if the IDW GeoPackages already exist, the IDW maps can be recreated directly from:

```text
cbs_500m_idw.gpkg
cbs_100m_idw.gpkg
```

These files contain the interpolated ER values for the 500m and 100m CBS grid cells.

To check whether they exist, run:

```r
file.exists("cbs_500m_idw.gpkg")
file.exists("cbs_100m_idw.gpkg")
```

If both return `TRUE`, the maps can be recreated without rerunning the full Part 6 interpolation.

## Notes

* The scripts are designed for reproducibility, but they assume that the input files use the same names and structures as in the original project.
* Some correction steps in Part 2 are project-specific and depend on the original Dutch polling-station data.
* The coordinate reference system used for the spatial analysis is RD New / EPSG:28992.
* The raw data files may not be included in this repository, depending on data-sharing restrictions.
* Output folders such as `figures/`, `sacd_outputs/`, and `kl_outputs/` are created by the scripts when needed.

## Main outputs

The most important final outputs are:

```text
voting_data_kieskompas_analysis_admin.gpkg
complete_geospatial_data.csv
er_stembureau.csv
er_gemeente.csv
er_corop.csv
er_province.csv
er_national.csv
cbs_500m_idw.gpkg
cbs_100m_idw.gpkg
sacd_outputs/
kl_outputs/
figures/
```

These outputs together contain the prepared geospatial voting data, polarization scores, interpolated polarization surfaces, SACD results, KL divergence results, and map figures.
