# ICES VMS & Logbook Data Call: Preparation and Submission Guidelines

## Table of Contents
1. [Installation of Required Software](#part1-installation-of-required-software)
   - [Step 1: Installation of R](#step-1-installation-of-r)
   - [Step 2: Installation of RStudio](#step-2-installation-of-rstudio)
   - [Step 3: Installing Required R Libraries](#step-3-installing-required-libraries)
   - [Step 4: Installing ICES R Packages](#step-4-installing-ices-r-packages)
2. [Workflow Overview](#part-2-workflow-overview)
   - [Block 0: Global settings and functions](#block-0-global-settings-and-functions)
   - [Block 1: Data preprocessing](#block-1-data-preprocessing)
   - [Block 2: Data analysis](#block-2-data-analysis)
   - [Block 3: Data submission](#block-3-data-submission)
   - [Supplementary visualization script](#supplementary-visualization-script)
3. [Contacts](#contacts)
4. [Changelog](#changelog)

## Appendices
- [Annex 1: Format specification for VMS data (VE)](#annex-1-format-specification-for-vms-data-ve)
- [Annex 2: Format specification for Logbook data (LE)](#annex-2-format-specification-for-logbook-data-le)
- [Annex 3: TACSAT and EFLALO formats](#annex-3-tacsat-and-eflalo-formats)

# Part 1. Installation of Required Software

This document provides guidance for processing VMS (Vessel Monitoring System) and logbook data according to the ICES data call requirements. The workflow uses freely available software (R and RStudio) and several specialised R packages.

## Step 1: Installation of R

If you already have R version 4.1.0 or later installed, you can proceed to Step 2.

If you need administrator privileges for software installation, please consult your IT support team.

1. Download the latest R version from [The Comprehensive R Archive Network](https://cran.r-project.org/bin/windows/base/)

2. Run the installer and follow these steps:
   - Select your preferred language
   - Click **Next** on the R installation Wizard menu
   - On the destination folder screen, click **Browse** and create a directory like `C:\R-4.3.3` instead of using the default location. This will allow you to install packages without administrator privileges.
   - Click **Next** through the remaining menus to complete the installation

## Step 2: Installation of RStudio

If you already have RStudio installed, move to Step 3.

1. Download RStudio from [the RStudio website](https://posit.co/download/rstudio-desktop/)

2. Run the installer:
   - Click **Run** if prompted by a security warning
   - Accept the default settings and click **Next** until the installation completes

3. Start RStudio to prepare for library installation

## Step 3: Installing Required Libraries

The workflow script handles most library installation and loading. However, you'll need to first install the `devtools` package to access libraries not available on CRAN.

1. Install `devtools` by running this command in your R console:
   ```r
   install.packages("devtools")
   ```

2. Load the `devtools` package:
   ```r
   library(devtools)
   ```
Note: If you're behind a firewall that blocks GitHub connections, you may need to configure proxy settings or ask your IT department for assistance.

## Step 4: Installing ICES R Packages

Several ICES-specific packages are required for this workflow:

```r
# Install ICES packages from their R-universe
install.packages(c("icesVMS", "icesVocab", "icesConnect", "icesSharePoint", "sfdSAR"),
                 repos = "https://ices-tools-prod.r-universe.dev")
```

# Part 2. Workflow Overview

The workflow consists of four main R script blocks, each handling different aspects of the data processing pipeline:

## Block 0: Global Settings and Functions

**File**: `0_global.R`

This script sets up the environment by:
- Installing and loading required packages
- Setting paths for data storage
- Defining thresholds for analysis (fishing speeds, time intervals, etc.)
- Loading support data (harbour locations, ICES areas)
- Defining utility functions used throughout the workflow

Important parameters to review:
- `yearsToSubmit`: Years for which data will be processed
- `autoDetectionGears`: Gear types for which automatic fishing activity detection will be used
- `visualInspection`: Whether to visually analyse speed histograms (TRUE/FALSE)

## Block 1: Data Preprocessing

**File**: `1_eflalo_tacsat_preprocessing.R`

This script cleans both VMS (TACSAT) and logbook (EFLALO) data. For each dataset:

**TACSAT cleaning**:
- Removes points outside ICES areas
- Removes duplicate records
- Removes impossible coordinates
- Removes pseudo-duplicates (pings too close in time)
- Removes points in harbours

**EFLALO cleaning**:
- Identifies and handles outlier catch records
- Removes non-unique trip numbers
- Removes records with impossible timestamps
- Removes trips starting before January 1st
- Removes trips with arrival date before departure date
- Removes overlapping trips
- Verifies metier codes against ICES vocabulary standards

## Block 2: Data Analysis

**File**: `2_eflalo_tacsat_analysis.R`

This script:
1. Merges cleaned VMS and logbook data
2. Assigns fishing activity status to VMS points
3. Calculates fishing effort and swept area
4. Adds additional spatial information (c-squares, ICES rectangles)
5. Distributes logbook landings among VMS pings

Key sections:
- **Speed thresholds**: Defines vessel speed ranges associated with fishing for different gear types
- **Activity detection**: Determines if a vessel is fishing or steaming based on speed and patterns
- **Data merging**: Links VMS positions with logbook catch data
- **Spatial assignment**: Allocates fishing activity to standardised spatial grid cells

## Block 3: Data Submission

**File**: `3_data_submission.R`

This script:
1. Creates Table 1 (VMS data) and Table 2 (logbook data) according to ICES requirements
2. Performs validation checks against ICES vocabularies
3. Anonymises vessel identifiers
4. Aggregates data as required by the submission format
5. Saves the final output files in the correct format for submission

Important sections:
- **ICES vocabulary checks**: Validates data against required code lists (gear types, metiers, etc.)
- **Data aggregation**: Summarises data at required resolutions
- **Quality control**: Generates reports on data completeness and consistency

## Supplementary Visualisation Script

**File**: `resources/make_htm_sar_effort_and_landings.R`

This supplementary script creates interactive HTML maps to visualise:
- Fishing effort distribution
- Catch value distribution
- Swept area ratio (SAR)

These visualisations help identify potential errors or anomalies before data submission.

# Using the Workflow

1. **Initial Setup**:
   - Install R and RStudio
   - Install required libraries
   - Set up directory structure

2. **Data Preparation**:
   - Format your raw VMS and logbook data into TACSAT and EFLALO formats (see Annex 3)
   - Save files with the naming convention: `tacsat_YYYY.RData` and `eflalo_YYYY.RData`
   - Place these files in your designated data directory

3. **Running the Workflow**:
   - Run `0_global.R` first to set up the environment
   - Run `1_eflalo_tacsat_preprocessing.R` to clean the data
   - Run `2_eflalo_tacsat_analysis.R` to process and merge the data
   - Run `3_data_submission.R` to prepare the final submission files
   - Optionally run the visualisation script in the resources folder to create interactive maps

4. **Validation and Submission**:
   - Review the quality control reports
   - Check the interactive HTML maps for anomalies
   - Submit the final tables to ICES

# Important Notes

- The workflow scripts contain numerous parameters that may need adjustment based on your specific data characteristics.
- Understanding fishing behaviour for different gears in your fleet is crucial for accurate activity determination.
- Always review the results carefully, especially the activity detection, which is a critical step.
- Pay attention to units: swept area is calculated in square kilometres (km²). Submission of this field is optional - if
  you have better information on the width of gears used in your national fleets, use them here. If not, the workflow
  will use the Benthis gear models to calculate width. If you leave this field blank, this will be done in the ICES database.
- Do not use dummy values (e.g. 999, -999999) for any field. Step 3.7 of the workflow specifies `na = ""` in the .csv
  files exported for upload. The data submission process will not accept `"na"` entries in numeric fields, but will accept
  blanks. If you have issues, please contact the Secretariat for help. 

# Contacts

For assistance with this workflow, please contact:

- Neil Campbell: [neil.campbell@ices.dk](mailto:neil.campbell@ices.dk)
- ICES Data Center: [accessions@ices.dk](mailto:accessions@ices.dk)

# Changelog

| **Date**          | **Change**                                | **Prepared by**             |
|-------------------|-------------------------------------------|----------------------------|
| March 2016        | Initial version created                   | Rui Catarino, ICES         |
| 1 February 2019   | Update                                    | Lara Salvany, ICES         |
| 1 February 2021   | Update                                    | Lara Salvany, ICES         |
| 15 February 2022  | Update                                    | Roi Martinez, UK           |
| 22 February 2022  | Update                                    | Lara Salvany, ICES         |
| 13 February 2023  | Update                                    | Cecilia Kvaavik, ICES      |
| 24 February 2023  | Update                                    | Neil Campbell, ICES        |
| 2 April 2025      | Rewritten for updated workflow            | Neil Campbell, ICES        |

# Annex 1. Format Specification for VMS Data (VE)

The VMS data should be submitted as a table with the following fields:

| Field                | Type         | Required | Description                                                |
|----------------------|--------------|----------|------------------------------------------------------------|
| RecordType           | char(2)      | Yes      | "VE" for VMS records                                       |
| CountryCode          | char(3)      | Yes      | ISO 3166-1 alpha-3 code                                    |
| Year                 | char(4)      | Yes      | Year of data                                               |
| Month                | int(2)       | Yes      | Month of data                                              |
| NoDistinctVessels    | int(5)       | Yes      | Number of distinct vessels                                 |
| AnonymizedVesselID   | nvarchar(500)| Yes      | Anonymised vessel IDs (e.g., "ESP001;ESP003")             |
| Csquare              | nvarchar(15) | Yes      | C-square reference (0.05° × 0.05°)                         |
| MetierL4             | char(25)     | Yes      | Gear type level 4                                          |
| MetierL5             | char(50)     | Yes      | Target assemblage                                          |
| MetierL6             | char(40)     | Yes      | Metier level 6                                             |
| VesselLengthRange    | char(20)     | Yes      | Vessel length class                                        |
| Habitat              | nvarchar     | No       | MSFD Habitat type                                          |
| Depth                | nvarchar     | No       | Depth range                                                |
| No_Records           | int          | Yes      | Number of VMS records                                      |
| AverageFishingSpeed  | float(15)    | Yes      | Average speed during fishing (knots)                       |
| FishingHour          | float(15)    | Yes      | Fishing hours                                              |
| AverageInterval      | float(10)    | Yes      | Average polling interval (minutes)                         |
| AverageVesselLength  | float(15)    | Yes      | Average vessel length (m)                                  |
| AveragekW            | float(15)    | Yes      | Average engine power (kW)                                  |
| kWFishingHour        | float(15)    | Yes      | kW × fishing hours                                         |
| SweptArea            | float        | No       | Swept area (km²)                                           |
| TotWeight            | float(15)    | Yes      | Total catch weight (kg)                                    |
| TotValue             | float(15)    | No       | Total catch value (EUR)                                    |
| AverageGearWidth     | float(15)    | Yes      | Average gear width (km)                                    |

# Annex 2. Format Specification for Logbook Data (LE)

The logbook data should be submitted as a table with the following fields:

| Field               | Type         | Required | Description                                                |
|---------------------|--------------|----------|------------------------------------------------------------|
| RecordType          | char(2)      | Yes      | "LE" for logbook records                                   |
| CountryCode         | char(3)      | Yes      | ISO 3166-1 alpha-3 code                                    |
| Year                | char(4)      | Yes      | Year of data                                               |
| Month               | int(2)       | Yes      | Month of data                                              |
| NoDistinctVessels   | int(5)       | Yes      | Number of distinct vessels                                 |
| AnonymizedVesselID  | nvarchar(500)| Yes      | Anonymised vessel IDs (e.g., "ESP001;ESP003")             |
| ICESrectangle       | char(4)      | Yes      | ICES statistical rectangle                                 |
| MetierL4            | char(25)     | Yes      | Gear type level 4                                          |
| MetierL5            | char(50)     | Yes      | Target assemblage                                          |
| MetierL6            | char(40)     | Yes      | Metier level 6                                             |
| VesselLengthRange   | char(20)     | Yes      | Vessel length class                                        |
| VMSEnabled          | char         | No       | Whether vessel has VMS ("Y"/"N")                           |
| FishingDays         | float(15)    | Yes      | Number of fishing days                                     |
| kWFishingDays       | float(15)    | No       | kW × fishing days                                          |
| TotWeight           | float(15)    | Yes      | Total catch weight (kg)                                    |
| TotValue            | float(15)    | No       | Total catch value (EUR)                                    |

# Annex 3. TACSAT and EFLALO Formats

Before using the workflow, your data must be formatted according to the TACSAT2 and EFLALO2 formats.

## TACSAT2 Format

| Variable            | Code         | Format/Unit                             |
|---------------------|--------------|----------------------------------------|
| Vessel ID           | VE_REF       | 20-character text string               |
| Latitude            | SI_LATI      | Decimal degrees                        |
| Longitude           | SI_LONG      | Decimal degrees                        |
| Date                | SI_DATE      | DD/MM/YYYY                             |
| Time                | SI_TIME      | HH:MM (UTC)                            |
| Instantaneous speed | SI_SP        | Knots                                  |
| Instantaneous heading | SI_HE      | Degrees                                |

## EFLALO2 Format

| Type                | Variable      | Code         | Format/Unit                        |
|---------------------|---------------|--------------|-----------------------------------|
| Vessel              | Vessel ID     | VE_REF       | 20-character string                |
|                     | Fleet         | VE_FLT       | DCF regulation                     |
|                     | Home country  | VE_COU       | ISO 3166-1 alpha-3 codes          |
|                     | Vessel length | VE_LEN       | Overall length (m)                 |
|                     | Vessel power  | VE_KW        | kW                                 |
|                     | Tonnage       | VE_TON       | GT (optional)                      |
| Fishing trip        | Trip reference| FT_REF       | 20-character string                |
|                     | Departure country | FT_DCOU  | ISO 3166-1 alpha-3 codes          |
|                     | Departure harbor | FT_DHAR   | UN LOCODE                         |
|                     | Departure date | FT_DDAT     | DD/MM/YYYY                        |
|                     | Departure time | FT_DTIME    | HH:MM                             |
|                     | Landing country | FT_LCOU    | ISO 3166-1 alpha-3 codes          |
|                     | Landing harbor | FT_LHAR     | UN LOCODE                         |
|                     | Arrival date  | FT_LDAT      | DD/MM/YYYY                        |
|                     | Arrival time  | FT_LTIME     | HH:MM                             |
| Log event           | Log event ID  | LE_ID        | 25-character string               |
|                     | Catch date    | LE_CDAT      | DD/MM/YYYY                        |
|                     | Gear          | LE_GEAR      | DCF metier level 4                |
|                     | Mesh size     | LE_MSZ       | mm stretched mesh                 |
|                     | ICES rectangle| LE_RECT      | ICES statistical rectangle        |
|                     | Fishing activity | LE_MET     | Metier level 6                   |
|                     | Catch - species 1 | LE_KG_SPP1 | Kg                             |
|                     | Value - species 1 | LE_EURO_SPP1 | EUR                          |
|                     | Catch - species n | LE_KG_SPPn | Kg                             |
|                     | Value - species n | LE_EURO_SPPn | EUR                          |
