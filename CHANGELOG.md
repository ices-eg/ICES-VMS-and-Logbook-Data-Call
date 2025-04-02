# VMS&LB Datacall R Workflow CHANGELOG - Version Control

All notable changes and version released to "VMS&LB Datacall R Workflow" are be documented in this file.

The CHANGELOG format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

# [2.1.0] - 02-04-2025

The changes are listed by the R Workflow block files. [0.global, 1.Eflalo Tacsat Preprocessing, 2.Eflalo Tacsat Analysis , 3.Data Submissions ] 

**R Library dependencies:**
   - [VMSTools 0.77](https://github.com/nielshintzen/vmstools/releases/tag/0.77)   
   - ICES R Libraries: SFDSAR,  ICESVocab, icesConnect, icesVMS, icesSharePoint
   - R CRAN Libraries : sf, data.table, raster, terra, mapview, Matrix, dplyr, doBy, mixtools, tidyr, glue, gt, progressr, geosphere, purrr,  tidyverse, units, tcltk, lubridate, here


## 0_global.R

### Changes

- Add code to install of SFDSAR and ICESVMS Pacakges latest  versions from ICES R LIBRARIES ENVIROMENT instead the CRAN with obsolete versions. 
- Updated  the  trip_assign function to use EFLALO haul details only if available in the countries data ( majority do not have haul level information) .
- Removed "predict_gearwidthmod" function and restore the use of SFDSAR "predict_gearwidth"   latets version   in "add_gearwidth" function. "predict_gearwidthmod" errors provided only the default gearwidth average from BENTHSI table , instead the modelled gear width. 
- Update "add_gearwidht" function:
   - Use SFDSAR "predict_gearwidth" function instead  "predict_gearwidthmod"
   - Outputs of model gear width from  "predict_gearwidth" are in meters. The function was updated to transform the width in meters to KKilometers to keep consistency.
   - Change of the field name for the user supply their own gear width. Previous version use "avg_gearwidth" field , now replaced to "LE_GEARWIDTH" for clarity
   - Documentation on the hierarchy of assign gear width in the function. 1) Check if there is width supplied by users in "LE_GEARWIDTH". 2) If not supplied by user, model the gear width 3) If not possible to model width takes the average gear width by metier form BENTHIS lookup table

## 1_eflalo_preprocessing.R

### Changes


## 2_eflalo_tacsat_analysis.R

### Changes

- Replace SplitAmongPings2 with the VMSTools - SplitAmongPings 0.77 function. 
- Updated the the  sections headers  and numbering  to  align with the Workflows documentation in ICES SFD WG Report / Benchmark Document  [link]
- Updated the calculate gear width functions call and the calculation of Swept Area to be all calcualted in KM and  area in KM2
- Updated the call to trip_assign function parameters to align with change in the funciton in 0_global.R

## 3_data_submission.R

### Changes

- Updated the aggregation code to call to the correct Swept Area in KM2 fields and aggregate them accordinly to DATSU requirements. 
 

# [1.0.0] - 27-04-2024

The workflow script largely follows the same process and approach as the original. Changes have been made to improve consistency and transparency, remove deprecated libraries and functions, and add further quality assurance stages. The changes to the steps of the workflow are as follows.

The changes are listed by the R Workflow block files. [0.global, 1.Eflalo Tacsat Preprocessing, 2.Eflalo Tacsat Analysis , 3.Data Submissions ]

## 0_global.R

The code has been revised to include more setup steps, data checks, helper functions and some additional analyses compared to the original code. The core workflow is generally similar, but the new code is more extensive and robust.

### Changed

1. The new code uses the `pacman` package to install and load required packages, avoiding the need to load the packages individually using `library()`.

2. The script now creates directories for storing scripts, data, results and plots if they don't already exist.

### Added

3. The new script downloads and extracts valid metiers from an online CSV file, maintained by the RDBES.

4. The new code loads bathymetry and habitat layers from RDS files for use later in the workflow.

5. The new code defines many additional utility functions, some of which were previously declared within the workflow. These include as:

   - Functions for converting data to `sf` format
   - Functions related to calculating species bounds and outliers
   - Functions for calculating intervals in tacsat data
   - Functions for gear width prediction
   - Functions for checking and cleaning tacsat and eflalo data
   - More complex implementations of some of the original functions like `intvTacsat`, `splitAmongPings` etc.
   - An updated version of `tacsatInHarbour` to determine if vessel positions are inside harbors.

## 1_eflalo_tacsat_preprocessing.R

The updated version incorporates additional data cleaning steps, using newer and more advanced functions and packages, and provides more detailed information about the cleaning process compared to the original version.

1. Cleaning TACSAT data:

   - The new version exclusively uses the `sf` package for spatial operations.
   - The new version calculates the percentage of remaining records after each cleaning step, which is not done in the original version.
   - The new version uses the `tacsatInHarbour` function to identify points in the harbor, while the original version uses the `pointInHarbour` function.
   - The new version removes the `inHarbour` column from the TACSAT data frame after filtering out points in the harbor, which is not done in the original version.
   - The original version includes an additional step to remove points on land using the `point.in.polygon` function, which is not present in the new version, as the ICES area shapefile already clips data with a highly resolved coastline.

2. Cleaning EFLALO data:

   - The new version includes a step to warn for outlying catch records using the `get_indices`, `get_species`, `get_bounds`, and `replace_outliers` functions, which are not present in the original version.
   - The new version uses a different approach to remove non-unique trip numbers by creating a unique trip identifier using the `create_trip_id` function, while the original version uses duplicated on a combination of columns.
   - The new version modifies the code to remove trips starting before January 1st.
   - The new version uses a different approach to remove records with arrival dates before departure dates by directly subsetting the data frame, while the original version creates a temporary data frame and then subsets it.
   - The new version includes a more complex logic to handle overlapping trips using `data.table` operations, while the original version uses a combination of `lapply`, `split`, and `apply` functions.

## 2_eflalo_tacsat_analysis.R

The updates to the script new code includes several additional data processing, analysis and visualization steps, uses some different functions, and has a more modular and detailed approach compared to the original code.

1. The new code uses a loop to assign gear and length related columns from eflalo to tacsatp, while the original code assigns them individually.

2. The new code provides feedback on the percentage of tacsat data that did not merge with eflalo. This is not present in the original.

3. The new code has an additional step to divide pings to the right gear/metier etc. for multi gear/metier trips using the `trip_assign` function. This is not present in the original.

4. In defining activity, the new code:

   - Uses the `intvTacsat` function instead of `intervalTacsat`
   - Assumes a normal interval value for pings with NA in INTV
   - Uses `ggplot2` to create a more detailed and customizable histogram of speeds by gear
   - Formats the level 5 metier correctly before creating the speedarr dataframe
   - Uses the `ac.tac.anal` function for analyzing activity if visualInspection is TRUE
   - Prints a summary table of min and max fishing speeds by metier
   - Handles cases where there are no non-missing values for "s" or "f" states when checking results

5. In dispatching landings, the new code:

   - Filters out invalid metier level 6 codes
   - Calculates total KG and EURO only if those columns don't already exist in eflalo
   - Uses the `splitAmongPings2` function to distribute landings among pings
   - Saves the updated eflalo dataframe

6. The new code has an additional section (2.3) to add information to tacsatEflalo, including:

   - Adding habitat and bathymetry data
   - Calculating c-square, year, month, kilowatt-hour, etc.
   - Adding calculated gear width and swept area
   - Saving the updated tacsatEflalo dataframe for each year

7. The new code does some housekeeping at the end by removing unnecessary objects.

## 3_data_submission.R

The new data submission code follows a similar structure to the original code but includes additional data processing steps, vocabulary checks, and creates some formatted QC reports. The new code also utilizes more `dplyr` functions for data manipulation.

1. Aggregating and summarizing Table1 and Table2:

   - The new code includes additional fields in the aggregation, such as habitat and depth for Table 1.
   - The new code calculates swept area in Table 1, which is not present in the original code.
   - The new code renames the columns using a predefined vector, while the original code renames them individually.

2. ICES DATSU vocabulary checks:

   - Both versions perform similar checks for various fields against DATSU vocabularies using the `icesVocab` package.
   - The new code includes additional checks for habitat and depth fields in Table 1.
   - The new code uses a loop to calculate the number of remaining rows and percentage after each vocabulary check, storing the results in matrices (remrecsTable1 and remrecsTable2), which is not present in the original code.

3. Data QC report (optional):

   - The workflow generates a QC report to check the number of NAs and record types for each field in Table 1 and Table 2.
   - The revised code uses the `gt` package to create formatted tables for this QC report.

## 4_plot_data.R

The new fourth code block in the new version of the script is used to create quality assurance maps using the `leaflet` package to visualize the spatial distribution of value, effort, and swept area ratio for different fleet segments over multiple years, based on the data in Table 1. The maps are saved as HTML files for each fleet segment, allowing for easy viewing and exploration of the data.

1. The script starts by loading the necessary packages using the `pacman` package.

2. It defines a custom function `%!in%` to check if an element is not in a vector.

3. The script loads the Table 1 data from an RData file using the load function.

4. It calculates the longitude and latitude coordinates for each c-square in table1 using the `CSquare2LonLat` function and rounds the values to 2 decimal places.

5. The script converts table1 to a data.table object named t1.

6. It creates a new column `LE_SEG` in t1 by extracting the first two elements of the `LE_MET` column, which represent the fleet segments from level 5 metiers.

7. The script groups some metiers into level 4 by modifying the `LE_SEG` column based on certain patterns.

8. It then iterates over each unique fleet segment (`LE_SEG`) to create two types of maps: value maps and effort maps.

9. For value maps:

   - It subsets t1 based on the current fleet segment, non-missing `LE_EURO_TOT` values, and non-zero `LE_EURO_TOT` values.
   - It converts the data to wide format using the dcast function, with longitude and latitude as the key variables and year as the column variable, summing the `LE_EURO_TOT` values.
   - It converts the data to an sf object (pts) and sets the CRS to 4326.
   - It rasterizes the points using st_rasterize function, specifying the grid size and extent.
   - It calculates the cell size of the raster in meters.
   - It converts the stars object to a raster object (r) and then to a RasterBrick object (rb).
   - It defines the value bins and color palette for the map.
   - It creates a leaflet map with a grayscale base map and adds the raster layers for each year.
   - It adds a layer control and a legend to the map.
   - Finally, it saves the map as an HTML file using `htmlwidgets::saveWidget`.

10. For effort maps:

    - The process is similar to the value maps, but it uses the `INTV` column instead of `LE_EURO_TOT`.
    - The effort values are converted to minutes per square kilometer.

11. Additionally, the script creates SAR (Swept Area Ratio) maps:

    - It follows a similar process as the value and effort maps but uses the `SA_M2` column.
    - The SAR values are calculated by dividing the swept area by the cell size.

This script allows data submitters to explore their data, prior to submission, and identify any errors which might only be apparent to them.
