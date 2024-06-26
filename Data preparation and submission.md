# Data call preparation and submission guidelines

##  [1 Installation of the required software](#part1-installation-of-required-software)

### [1.1 Step 1 Installation of R](#step-1-installation-of-r)
### [1.2 Step 2 Installation of RStudio](#step-2-installation-of-rstudio)
### [1.3 Step 3 Installing VMSTOOLS R package](#step-3-installing-vmstools)
### [1.4 Step 4: Installing ICES R packages](#step-4-installing-ices-r-packages)

##  [2 Proposed workflow R code](#part-2-proposed-workflow-r-code)

### [1  Data preprocessing](#1-data-preprocessing)
### [2  Data analysis](#2-data-analysis)
### [3  Data submission](#3-data-submission)
 
## [3  Contacts](#3-contacts) 
## [4  Changelog](#4-changelog) 

## [Annex 1 Format specification for VMS data (VE)](#ve-table-format)
## [Annex 2 Format specification for Logbook data (LE)](#le-table-format)

# Part 1. Installation of required software 

This document is designed to aid analysts streamline the processing of VMS and logbook data in accordance with requirements of the ICES data call.

Part 1 of the document provides guidelines for installing all the software necessary for data manipulation and aggregation into the requested format. The software used, R and RStudio, are freely available.

The document is designed to aid all users, regardless of their experience using R. The steps listed cover the installation of R and RStudio and detailed information will be provided to cover all stages of the installation process to ensure success. Depending on your skills you might want to jump some of the steps. To ensure consistency across all users we advise installing VMStools version 0.78. Following these steps should enable quick and simple processing of all data.

All the instructions and code below were tested in windows versions 10 and 11. However, if for any reason something is not working you can contact one of the members of our support team (emails at the end of the document).

## Step 1: Installation of R

Where a version of R is already installed, version 4.1.0 or later, move to step 2

Completion of the first two steps of these guidelines is dependent on the users computer security setting. In instances where administrator privilege is required then please ask a member of your IT/computer support team to run the first two steps for you.
 
So let's start by instaling R; click on the link below to download the latest R version.

[The Comprehensive R Archive Network - Latest R Version](https://cran.r-project.org/bin/windows/base/)

Once it is downloaded, double click on the file (.exe). Depending on your security settings you might get a pop up security warning asking if you want to Run or Cancel the installation, Click **Run**.-

- Select a language (when you select the language, bear in mind that these instructions are in English)

- At this point you should be on the R installation Wizard menu.Just click **Next**

- Here you are presented with the GNU general public license which you are most welcome to read. Click **Next**

- The menu (picture below) will appear and you will be asked to select the destination folder


<img src="https://raw.githubusercontent.com/neilcampbelll/spatial/b9816dc3fe7ce82aa8aa7996422170f03faa4ac3/Screenshot%202024-01-16%20143734.png" alt="drawing" width="400"/>
 

Rather than accepting the default(C:Files-4.3.3) you should click on the browse button and create a directory C:-4.3.3 which will allow you to install packages without having administrator privileges. Once you have changed the folder just click **Next**

- Click **Next** all the menus until the end of instalation, and that's it; R is now installed on thecomputer.

  
## Step 2: Installation of RStudio

(Where Rstudio is already installed move to step 3)

If you don't have administrator privilege on your computer you will need to call your IT/computer support to install R Studio.

First download RStudio by [clicking on this link](https://download1.rstudio.org/electron/windows/RStudio-2022.12.0-353.exe).

Once downloaded, double click on it. Depending on your security settings you might get a popup security warning asking if you want to Run or Cancel the installation, Click on **Run**. At this point you should be on the RStudio installation Wizard menu. RStudio is very easy to install so just accept all defaults and click **Next** in all the menus until the end.

That's it. Step 2 is complete, RStudio is now installed.

Now start Rstudio to install all the necessary R packages that vmstools depends on. 

  
## Step 3: Installing Libraries

If you are following the workflow script, the installation and loading of most libraries is now handled within the script. We still need to download and install the libraries which are not available on CRAN. To do this, we will need the **Devtools** library. Devtools provides functions that allow you to install packages from GitHub. If you haven’t installed it yet, you can do so by running the following command in your R console:

Paste:
  

```r 
install.packages("devtools")
```

into the console and press enter. It should start installing the Devtools package. This might take two or three minutes. Please contact your system administrator if you have issues installing these packages.

Once installed, you need to load the devtools package. You can do this with the library function:

```r
library(devtools)
```
Now you can install the specific version of the package from GitHub using the **install_github** function. You need to provide the repository name in the format username/repo@version. In this case, the repository we want to install is *nielshintzen/vmstools* and the version is 0.78. Here’s how you can install it:

```r
install_github("nielshintzen/vmstools@0.78")
```
After the package is installed, you can load it using the library function:

```r
library(vmstools)
```
Please note that the install_github function might not work if you’re behind a firewall that blocks connections to GitHub.

If you are following the recommended workflow, the required libraries will be installed (if they not currently available on your computer) and loaded in lines 16-23. If you are not following the workflow, you will need to manually install these libraries, which are available on CRAN.

That's it; you now have all you need to process your data.


# Part 2. Proposed workflow R code 

Part one of these guidelines have guided you through the installation of all the software needed to process your data into the formats specified in the data call. Now, part two will focus on guiding you through the blocks that comprise the workflow: 

- [0_global.R](https://github.com/ices-eg/wg_WGSFD/blob/test-workflow/VMS-datacall/0_global.R)
- [1_eflalo_tacsat_preprocessing.R](https://github.com/ices-eg/wg_WGSFD/blob/test-workflow/VMS-datacall/1_eflalo_tacsat_preprocessing.R)
- [2_eflalo_tacsat_analysis_R](https://github.com/ices-eg/wg_WGSFD/blob/test-workflow/VMS-datacall/2_eflalo_tacsat_analysis_R)
- [3_data_submission.R](https://github.com/ices-eg/wg_WGSFD/blob/test-workflow/VMS-datacall/3_data_submission.R)
- [4_plot_data.R](https://github.com/ices-eg/wg_WGSFD/blob/test-workflow/VMS-datacall/4_plot_data.R)
  
The key things to note are that using the workflow blocks 0, 1 and 2 to process your data **is not mandatory**, but they are a very good idea. If you have your data in a bespoke national database and feel comfortable outputting it in the required format, please feel free to do so. Using block 3 to process the submission of data to ICES is, however, very strongly urged. Block 4 is a voluntary step which is not required for data processing and submission, but will help you visualise your data after it has been processed, and identify errors before uploading.

VMS and logbook data are highly resolved data which carry a lot of information about the behaviours and incomes of individuals. As we are using this data for scientific purposes, and in an international context, we obviously want to protect individual anonymity as much as possible. To this end, we combine VMS data (spatio-temporal information on individual vessels) and logbook records (landed weights and values by fishing operation) and then aggregate them by metier over a 0.05-degree grid, using the "c-squares" notation system. 

We will assume that you are starting with files of VMS and logbook data, formatted into the [tacsat and eflalo formats](https://github.com/ices-eg/ICES-VMS-and-Logbook-Data-Call/blob/main/EFLALO%20%26%20TACSAT%20Formats.md).

The aim is to get your data converted into the format specified in ICES DATSU: [http://datsu.ices.dk/web/selRep.aspx?Dataset=145](http://datsu.ices.dk/web/selRep.aspx?Dataset=145)

The work flow was developed in R, the principal objective being to facilitate the submission of data in the specified format by providing all country institutions with a standardised tool for data extraction. This will make data outputs more comparable and easier to work with.

Many of the specific functions that this workflow uses to extract and process VMS and landings data are part of the open-source vmstools package. This package was specifically developed to work with VMS and landings data. If you want to learn more about this tool you will find plenty of information and tutorials in the linkbelow.

[https://github.com/nielshintzen/VMStools/wiki](https://github.com/nielshintzen/VMStools/wiki)

Before initiating the workflow, it is first necessary that you have all VMS and landings data in the tacsat2 and eflalo2 formats respectively. If you need more information about either format you can download a document with the detailed specifications for both in the linkbelow.

[https://github.com/nielshintzen/VMStools/releases/tag/0.0/Exchange_EFLALO2_v2-1.doc](https://github.com/nielshintzen/VMStools/releases/tag/0.0/Exchange_EFLALO2_v2-1.doc)

If you are experiencing difficulties and your data is not yet in the tacsat and/or eflalo format please get in touch with one of the contacts at the bottom of these guidelines. Someone will get back to you and help you to rearrange your data into the specified formats allowing you to move on with the analysis.

The proposed workflow is not a one-size-fits-all solution and there are parts of the script that may need to be adapted to allow for the specific nature of the fisheries data from each country. Throughout this document all parts of code that need adjustment will be highlighted and explained so you should have a good understanding of what is happening at all times.

The code is divided in 3 main blocks and a number of sections. We will explain briefly what each section and block does and its purpose within the code. The idea of the guidelines is not to explain in detail what each line of code does but to give an overview of what is happening at each stage. The script itself is well annotated, so if you are familiar with R and the vmstools package, you probably won't need to follow these guidelines as closely.

Let's begin:

House keeping

Open 'Rstudio' and load the workflow. Before making any changes save the script with a different name. This will allow you to quickly refer back to the original code in case anything unexpected happens.

Just a quick note, which may be very obvious for all of those using R, but not so much for someone just trying to follow the guidelines. Anything in the code after a hashtag (#)sign is not code and it won't be read by the program. The # sign is used to add sections and block headers or general annotations. 
As such, the first line of code starts on line 23.

To make it easier to follow the guidelines we will explain the code, referencing the sections, headers (numbered) and blocks by highlighting them in bold.

# - Clear workspace

This code will just clear your work space to allow you to start afresh. Also, the packages that will be needed to run the code will be loaded into the session. If you followed the instructions in part one these should already be installed and loading them shouldn't be a problem.

# - Settings paths

At this point you need to replace the paths shown in the code with your own ones.The approach used in the code is one of best practice, as everything will be in one main folder 'VMSdatacall'. This will make it very easy to navigate between folders and to backup. So, for your own convenience it is recommended that you use the default paths as listed in the code. However, it is possible to change these by simply specifying your personal destination folders and defining the chosenpath.

# - Settings and specific thresholds

The thresholds here defined will be used later in different processes throughout the code. These will include, data cleaning or definition of vessel state (i.e. fishing/not fishing). The values set for the thresholds are considered to be reasonable and unless there are particularities in your data there shouldn't be a need to change these values.

#- Re-run all years as we have new field for no. vessels and anonymization of less than three vessels

#- Set the gear names for which automatic fishing activity is wanted

#- It is important to fill out the gears you want to apply auto detection for

#- Decide if you want to visually analyse speed-histograms to identify fishing activity

#- peaks or have prior knowledge and use the template provided 

#- Specify how landings should be distributed over the VMS pings: By day, ICES rectangle, trip basis or otherwise

# - Functions

Next we load in a number of helper functions which will be used later in the workflow. This helps ensure consistency in how the data is processed.

* ```'%!in%'```
* ```transform_to_sf```
* ```get_spec_bounds```
* ```get_species_indices```
* ```kgeur```
*  ```create_trip_id```
*  ```convert_to_datetime```
*  ```remove_before_jan```
*  ```intvTacsat```
*  ```sfsortTacsat```
*  ```callNumberPeak```
*  ```ac.tac.anal```
*  ```act.tac```
*  ```trip_assign```
*  ```predict_gear_width_mod```
*  ```add_gearwidth```
*  ```tacsat_clean```
*  ```eflalo_clean```
*  ```get_indices``` 
*  ```get_species``` 
*  ```get_bounds``` 
*  ```replace_outliers``` 
*  ```tacsatInHarbour```
*  ```splitAmongPings2```

## 1 Data preprocessing 
(1_eflalo_tacsat_preprocessing.r)

### Load the data

####  1.1 Load VMStools underlying data

This will load in to the session support data such as a map of Europe, list of harbours and ICES areas that will be used throughout the code.


####  1.2 Clean the tacsat and eflalo data
#####  Looping through the data years

The next line of code has a 'for' loop which means that all the code within the loop will run at the same time. This particular loop stretches from line 17 to line 363 leaving only a couple of lines at the end of the code. However, before running the 'for' loop there are a few things that need to be changed/adapted to your case.

In order to ensure that everything is working properly and to have a better understanding of what the code is doing inside the loop we will run one single year as a test. If we are able to run one year of data without coming across errors then we can run the code for all the years at once.

So for the moment we will ignore this block (#-1.2) **Looping**** through ****the**** data** and copy the line below into the console:


```{r}

yearsToSubmit <- sort(2009:2009)

```

#####  1.2.1 Load tacsat and eflalo data from file

In your 'Data' folder you should have all your tacsat and eflalo files in the .RDATA format. In the code it is expected that your files have the following naming convention 'tacsat\_ XXXX" i.e. tacsat\_2009; tacsat\_2010, etc. The same naming convention is applied to the eflalo files. This will allow the code to load the files as they are needed during the 'for' loop. Failing to correctly name the files will result in an error.

Since we have just copied **year <-**** 2009** to the console, when we run this block only the 2009 year data will beloaded.

Now that you have just loaded both tacsat and eflalo for 2009 the next two lines will just change the name of your objects to 'tacsat' and 'eflalo'. Depending on what your objects are called you may need to change the code. (tip: If at any stage you don't remember what the names are just type 'ls()' and a list of all objects already loaded will appear). If your object names are not 'tacsat' and 'eflalo' you will need to adapt the code. However, this is a simple process. Below are two examples for changing objects names in this case the names are '2009Tacsat' and another called 'Tac09'


```r

tacsat <- get(paste(year,"Tacsat",sep=""))
tacsat <- get(paste("Tac",substr(year,3,4),sep=""))

```

If the objects are already called tacsat and eflalo then, you don't need to run those two lines and you can add an # at the beginning of each line. However, make sure that whatever you have called your objects the naming structure is consistent across all years otherwise the "For" loop won't run.

#- Make sure data is in right format

It just ensures that your files are formatted properly.

#- Take only VMS pings and eflalo records in the ICES areas

This block of code will identify all VMS pings (tacsat) within the ICES areas.

##### 1.2.2 Clean the tacsatdata

This section will focus on 'cleaning' the data in the tacsatfile. The information in the tacsat(vms) comes from an electronic system that uses GPS information to collect the data on board the vessel and uses a satellite link to send the data to the database. Despite the reliability of this system conditions at sea are not always the best. There are two main opportunities for errors to occur, when receiving or sending data from the GPS and to the database. The code in this section will look to the most common errors and try to identify all of them. The code will not only delete the errors but also keep a record of what was deleted allowing you to keep track of how much data you have lost due to errors.

###### 1.2.2.1 Keep track of removed points

This section will check for five common types of errors. At each of these checks errors will be removed from the tacsat object. However, the data removed will be kept and saved in the 'Results'folder so you can verify the errors. Also, the volume for each of errors for each of the five checks is recorded the 'remrecsTacsat'object. This object will tell you percentage wise how much you have lost in relation to the original tacsat object.

###### 1.2.2.1 Remove points outside the ICES area

###### 1.2.2.2 Remove duplicate records

###### 1.2.2.3 Remove points that cannot be possible

###### 1.2.2.4 Remove points which are pseudo duplicates as they have an interval rate < x minutes 

###### 1.2.2.5 Remove points on land/in harbour

All of the above are self-explanatory and each of the blocks will check for a particular type of error, remove them where they occur and store the removed entries in the 'Results' folder and will quantify the number of values removed.

![image](https://github.com/ices-eg/ICES-VMS-and-Logbook-Data-Call/assets/16877549/a667d306-8155-4207-86d4-e9f5332c44c8)

###### Save the remrecsTacsat file

The file is now saved and by typing "remrecsTacsat" into the console you will get an overview of how much data was lost due to errors.

###### Save the cleaned tacsat file

Now you have your file cleaned and saved so no need to repeat the process in future analysis.

## 1.2.3 Clean the eflalo data

This section, like the previous one also focuses on "cleaning" the data. This time the target is the eflalo file. The types of errors are of a different nature but once again the code tries to account for the most commons errors. As in the previous section, the code will keep track of what data has been removed and how much. All these files can be found in the 'Results' folder. One should spend a bit of time looking at the data removed as it can be very useful to understand why and where problems occur.

###### 1.2.3.1 Keep track of removed points

The 'remrecsEflalo' object will keep you informed of how much data has been removed.

###### 1.2.3.2 Warn for outlying catch records

Basically this block looks for outliers. For each species, it generates a data-driven outlier threshold. If any outliers are found, these will be converted into 'NA' values. You can check in the 'Results' folder for the files containing all the outliers and you can double check if they are correct or not. If they were correct then you can run the code again but the code will need some adjustments.

###### First get the species names in your eflalo dataset

###### Define per species what the maximum allowed catch is

###### (larger than that value you expect it to be an error / outlier

###### Make a list of the species names and the cut-off points / error / outlier point

###### Put these values to zero

###### Get the index (column number) of each of the species

###### If landing > cut-off turn it into an 'NA';

###### Turn all other NAs in the eflalo dataset in KG and EURO columns to zero

###### 1.2.3.3 Remove non-unique trip numbers

###### 1.2.3.4 Remove impossible time stamp records 

###### 1.2.3.5 Remove trip starting before 1st Jan

###### 1.2.3.6 Remove trip with overlap with another trip

###### 1.2.3.7 Remove records with arrival date before departure date

The above block headers are self-explanatory and the code in each of the blocks is just identifying those common errors and removing them from the eflalo object.

###### Save the remrecsEflalo file #- Save the cleaned eflalo file

The 'remrecsEflalo' file is saved for future reference. So is the cleaned eflalo file which, like the tacsat, will be ready to use in the future.


## 2 Data analysis 
(2_eflalo_tacsat_analysis.R)


### 2.1 Merge the tacsat and eflalo data together

In section four we bring the tacsat and eflalo together by merging to create a new object. This will enable us to relate the landings component (eflalo) to the vessel activity

i.e. VMS (tacsat)

###### Merge eflalo and tacsat

The files tacsat and eflalo will be combined using some very clever algorithms that use the vessel identifier and date and time in both data sets to relate the landings to the corresponding VMS data for the same trip.

###### Assign gear and length to tacsat 

The new object tacsatp is now a merged version of the cleaned tacsat and eflalo objects. However, the new object hasn't inherited all the fields from eflalo due to reasons of processing speed and work ability. At this point we will extract some data from the eflalo dataset to populate the corresponding tacsatp fields. The data we are interested in are data that will be used later on in the code to populate the final data tables. Things like gear; kw; metiers, etc.

###### Assign gear and length to tacsat

###### Save not merged tacsat data

Not all vessel activity is associated with fishing events; quite often vessels may be testing equipment or chartered to do jobs other than fishing. So, the merge executed in the previous block only includes VMS data that can be linked to corresponding landings records. As such, it will not be possible to merge all tacsat data for allocation to the tacsatp object. This block will save both the merged and non-merged data into the 'Results' folder.

### 2.2 Define activity

This is a crucial section, as vessel activity will be defined here. Also, this is the section that needs the most customization for which some knowledge of fisheries activities will be needed. In this section we will try to explain the steps in more detail and incorporate some reproducible examples as well. The first couple of lines in this section will calculate time interval between points. The time values and the interval threshold will be paramount in identifying vessel activity lateron.

###### Calculate time interval between points

###### Remove points with NA's in them in critical places

This block gets rid of any rows in the tacsatp for which critical information (vessel reference, latitude, longitude, speed, date and time) is missing. If this data wasn't removed it would most likely lead to errors.

###### Define speed thresholds associated with fishing for gears

###### Investigate speed pattern through visual inspection of histograms #

The code in this block creates a very useful plot of speed frequency by gear. This plot will be saved in the "Results" folder and before you run any further code you should look closely at the output plot.

###### Create speed threshold object

The three last lines of this block will create a threshold object. However, your input and knowledge of the relevant fisheries will be needed at this stage. The threshold object will hold the minimum and maximum speed of fishing for each of the gears (i.e. the minimum and maximum speeds at which the specific fishing activity is thought to occur). To help you with this task you should look at the previous speed frequency plot to help distinguish steaming from fishing events.

By running the third last line in this block you create an object "speedarr" with all the different gears in your data. The second and third lines will fill in column 2 and 3 of the 'speedarr' object with the minimum and maximum fishing speeds. These values are set to 1kt and 6kt by default. At this stage you will need to set up the upper and lower limits for each of the gears. Although there are several ways of accomplishing this, we will demonstrate one of them here. In the example below we use a list of 5 gears(DRB;PTB;OTT;GN;FPO) although you are likely to have many more gears in your dataset so you will need to extend the code to accommodate all gears accordingly.

To create our example, copy the code below into the console:

```r
  
  speedarr <-
    as.data.frame(
      cbind(
        LE_GEAR = sort(unique(tacsatp$LE_GEAR)),
        min = NA,
        max = NA),
      stringsAsFactors = FALSE)
  speedarr$min <- rep(1, nrow(speedarr)) # It is important to fill out the personally inspected thresholds here!
  speedarr$max <- rep(6, nrow(speedarr))
  

```


So, in the example above you can easily see how each line applies to one gear and on the left you have the minimum values on the right the maximum. Make sure when you copy and paste the lines you change the gears and values on both sides.

If your data varies from year to year you might want to check if you have different gears in different years. Before running the full code, you should make sure that all gears are included in the code above.

###### Analyse activity automated for common gears only. Use the speedarr for the other gears

This block allows you to select some gears for which the detection can be done automatically. This is another functionality of VMStools which applies for the most common gears. So, in this block you will need to choose which gears to want to apply auto detection. You can add or delete gears in the first line of code in this block.

The remainder of the code in the block will split the tacsatp object in two depending on whether gears will be detected automatically or whether the thresholds need to be user defined according to the code from the previous block. The remaining lines in the block don't need to bechanged.

###### Fill the storeScheme values based on analyses of the pictures

###### Define mean values of the peaks and the number of peaks when they are different from 5 #

In this block the speed histogram plot created previously will be used once more. You will have to identify the peaks in the plot for the gears for which you want the activity to be automatically detected (bear in mind that the algorithm was developed with trawling in mind).

So, first of all make sure you have a line for each of the gears (see line below) changing the gears and mean speedsaccordingly.

```r

storeScheme$means[which(storeScheme$analyse.by == 'SSC')] <- c('-9 0 9')

```

Now using the plot, identify where the peaks are and use this to change the code. Make sure you follow the same nomenclature as the example provided. Also, for the algorithm to perform better, we need to create a mirror image of the peaks and with 0 (zero) in the middle .If the number of peaks for a particular gear is greater or less than 5 you will need to add a line (like the one below) with the true number of peaks observed. In the example above there were three peaks -9, 0, 9 so we would need to add the line below to thecode.

```r

storeScheme$means[which(storeScheme$analyse.by == 'SSC')] <- 3

```

The second half of the block, checks the results of the auto detection; if they are not satisfactory the analysis is run once more; this time using fixed peaks.However, in this workflow we will not be using fixed peaks so no need to worry about this.

###### Check results, and if results are not satisfactory, run analyses again but now with fixed peaks #

###### Assign for visually inspected gears a simple speed rule classification

This block deals with all the other gears that are not automatically detected. The code simply applies the upper and lower limits defined previously to define if vessel activity as either steaming orfishing.

###### Combine the two dataset together again

Now that all gears have had their activity defined, the code in this block is just putting it all back together in one object. As usual the object will be saved in the "Results" folder.

###### Set fishing sequences with hauling in the middle to "f"  

### 2.3 Dispatch landings of merged eflalo at the pingscale

This section calculates the total daily landings (weight and value) and splits the values equally among the daily fishing pings.

#- There are several options, specify at the top of this script what type of linking you require

### 2.4 Assign c-square, year, month, quarter, area and create table1

We are at final stage and the code in this block will generate one of the tables requested by the data call. The first part of the code pulls together all the fields needed to create the table. The second part deals with the aggregation by CSquare.

### 2.5 Assign year, month, quarter, area and create table2

We have reached the last section. As in the previous one, the first part of the code will create all the fields needed for table 2 as requested in the data call. The second part of the code deals with the aggregation into CSquares.

Running the last two lines saves the data into the 'Results' folder. Don't forget to check the outputs to make sure that everything iscorrect.

If you are happy with all the results then you can proceed running the entire code. Make sure you double check the names of the input files, ensuring they follow the convention (as in the example) and run the entire code.

## 3 Data submission 
(3_data_submission.r)

This code was developed by WGSFD experts and ICES Secretariat to prepare the data for submission and follows the format specified in ICESDATSU:([http://datsu.ices.dk/web/selRep.aspx?Dataset=145](http://datsu.ices.dk/web/selRep.aspx?Dataset=145)).

Once prepared, the data can be submitted  to  ICES using icesDATSU:([https://cran.r-project.org/package=icesDatsu](https://cran.r-project.org/package=icesDatsu)) 
and icesConnect:([https://cran.r-project.org/web/packages/icesConnect/index.html](https://cran.r-project.org/web/packages/icesConnect/index.html)). 


###  3.1 Load TABLE 1 (VMS) and TABLE 2 (LOGBOOK)

### 3.2 Replace vessel id by an anonymized id column

#### New field added since the 2020 datacall including distinct vessels id's  #
This vessel id is used to calculate the number of distinct vessels in a c-square 

### 3.3 Assign the vessel length category based in DATSU vocabulary

Use of the "icesVocab" ICES developed R package that fetch the DATSU vocabulary values for a given vocabulary theme #

install.packages("icesVocab", repos = "https://ices-tools-prod.r-universe.dev")


### 3.4 Aggregate and summarise TABLE 1 and TABLE2

### 3.5 ICES DATSU VOCABULARY CHECKS BEFORE DATA SUBMISSION

The submission will not progress if the format used does not confirm to the required format and the submitter will be asked to fix the errors in order to proceed (DATSU check). 
This piece of code performs a DATSU check (c-squares are within ICES ecoregions, vessel length categories are accepted, metier L4, L5 and L6 are valid, country codes are correct,...).

Running this code will allow you to fix all the format issues ahead of data submission via an API web service:([https://datsu.ices.dk/web/screen.aspx](https://datsu.ices.dk/web/screen.aspx)) and/or manual data upload in ICES Data portal:([https://data.ices.dk/vms/manage/fileUpload](https://data.ices.dk/vms/manage/fileUpload))

For a description of metiers and workflow for the assignment of metiers please follow:([https://github.com/ices-eg/RCGs/tree/master/Metiers](https://github.com/ices-eg/RCGs/tree/master/Metiers)

### 3.6 DATA QC REPORT (OPTIONAL)

Data submitters are welcome to run a QC report to compare data submitted through the years. 

#### WEB API SUBMISSION (OPTIONAL)

Data submitters can choose to submit data to ICES using functions from the packages icesDATSU:([https://cran.r-project.org/package=icesDatsu](https://cran.r-project.org/package=icesDatsu))
and icesConnect:([https://cran.r-project.org/web/packages/icesConnect/index.html](https://cran.r-project.org/web/packages/icesConnect/index.html)). 




# 3 Contacts

Patrik Jonsson:[patrik.jonsson@slus.se](mailto:patrik.jonsson@slu.se)

Josefine Egekvist:[jsv@aqua.dtu.dk](mailto:jsv@aqua.dtu.dk)

Roi Martinez:[roi.martinez@cefas.co.uk](mailto:roi.martinez@cefas.co.uk)

Neil Campbell:[neil.campbell@ices.dk](mailto:neil.campbell@ices.dk)

Josefine Egekvist:[jsv@aqua.dtu.dk](mailto:jsv@aqua.dtu.dk)





# 4 Changelog

| **Date**          | **Change**             | **Prepared by**         |
| ---               |    ---                 | ---                     |
|      March 2016   |Initial version created | Rui Catarino, ICES      |
| 1 February 2019   | Update                 | Lara Salvany, ICES      |
| 1 February 2021   | Update                 | Lara Salvany, ICES      |
| 15 February 2022  | Update                 | Roi Martinez, UK        |
| 22 February 2022  | Update                 | Lara Salvany, ICES      |
| 13 February 2023  | Update                 | Cecilia Kvaavik, ICES   |  
| 24 February 2023  | Update                 | Neil Campbell, ICES     |


# Annex 1. Format specification of VMS data (VE)
The specification of the format for VMS can be found here: [datsu.ices.dk/web/selRep.aspx?Dataset=145](https://datsu.ices.dk/web/selRep.aspx?Dataset=145)
and in the table below


## VE table format
| Start | FieldCode           | Datatype      | Code List                                                 | Mandatory | Description                                                                                                                                         |
|-------|---------------------|---------------|-----------------------------------------------------------|-----------|--------------------------------------------------------------------------------------------------------------------------------------------------- |
| 1     | RecordType          | char(2)       |                                                           | M         | RecordType field consists of a 2-character code,  which defines the record type and thus the layout of the data fields included on that record.    |
| 2     | CountryCode         | char(3)       | [ISO_3166](http://vocab.ices.dk/?ref=337)                 | M         | ISO 3166-1 alpha-3 codes. The flag country  of the vessel.                                                                                         |
| 3     | Year                | char(4)       | [Year](http://vocab.ices.dk/?ref=362)                     | M         | Year                                                                                                                                               |
| 4     | Month               | int(2)        |                                                           | M         | Month                                                                                                                                              |
| 5     | NoDistinctVessels   | int(5)        |                                                           | M         | Number of distinct vessels                                                                                                                         |
| 6     | AnonymizedVesselID  | nvarchar(500) |                                                           | M         | Anonymized vessel ID: Country code + 3 digits and semicolon separated.  For example: ESP001; ESP003;                                               |
| 7     | C-square            | nvarchar(15)  |                                                           | M         | 0.05x0.05 degree,  C-square reference XXXX:XXX:XXX:X                                                                                               |
| 8     | MetierL4            | char(25)      | [GearType](https://vocab.ices.dk/?codetypeguid=6f65b819-57ca-4904-a961-2d55699008d5)              | M         | Metier level 4                                                                                                                                     |
| 9     | MetierL5            | char(50)      | [TargetAssemblage](http://vocab.ices.dk/?ref=1499)        | M         | Metier level 5                                                                                                                                     |
| 10    | MetierL6            | char(40)      | [Metier6_FishingActivity](http://vocab.ices.dk/?ref=1647) | M         | Metier level 6                                                                                                                                     |
| 11    | VesselLengthRange   | char(20)      | [VesselLengthClass](http://vocab.ices.dk/?ref=1725)       | M         | Length class for vessels used in data sampling                                                                                                     |
| 12    | Habitat             | nvarchar      |  [ADD TO DATSU]                                           |           | MSFD Habitat type                                                                                                                                       |
| 13    | Depth               | nvarchar      |  [ADD TO DATSU]                                           |          | 200m Depth range                                                                                                                                        |
| 14    | No_Records          | int           |                                                           | M          | Number of records included in row (required for correct calculation of average speed and gear width when aggregated to c-square                                                                                                   |
| 15    | AverageFishingSpeed | float(15)     |                                                           | M         | Average fishing speed within the aggregation: year, month, c-square, vessel length category, gear code and DCF métier                              |
| 16    | FishingHour         | float(15)     |                                                           | M         | Fishing hour calculated from VMS data (excluding non-fishing activity)                                                                             |
| 17    | AverageInterval     | float(10)     |                                                           | M         | Average polling frequency calculated from VMS data                                                                                                 |
| 18    | AverageVesselLength | float(15)     |                                                           | M         | Average vessel length within the aggregation: year,  month, c-square, gear code and DCF metier                                                     |
| 19    | AveragekW           | float(15)     |                                                           | M         | Average vessel power( kW) within the aggregation: year,  month, c-square, gear code and DCF metier                                                 |
| 20    | kWFishingHour       | float(15)     |                                                           | M         | kW*Fishing hours                                                                                                                                   |
| 21    | SweptArea           | float         |                                                           |           | Swept area                                                                                                                                         |
| 22    | TotWeight           | float(15)     |                                                           | M         | Total landings of all species caught in **kg**                                                                                                     |
| 23    | TotValue            | float(15)     |                                                           |           | Total Value of all species caught in **Euro**                                                                                                      |
| 24    | AverageGearWidth    | float(15)     |                                                           | M         | Average Gear width                                                                          
M = mandatory


# Annex 2. Format specification of Logbook data (LE)
The specification of the format for VMS can be found here: [datsu.ices.dk/web/selRep.aspx?Dataset=145](https://datsu.ices.dk/web/selRep.aspx?Dataset=145)
and in the table below


## LE table format

| **Start** | **FieldCode**      | **Datatype**  | **Code List**                                             | **Mandatory** | **Description**                                                                                                                                         |
|-----------|--------------------|---------------|-----------------------------------------------------------|---------------|---------------------------------------------------------------------------------------------------------------------------------------------------------|
| 1         | RecordType         | char(2)       |                                                           | M             | RecordType field consists of a 2-character code,  which defines the record type and thus the layout of the data fields included on that record.         |
| 2         | CountryCode        | char(3)       | [ISO_3166](http://vocab.ices.dk/?ref=337)                 | M             | ISO 3166-1 alpha-3 codes. The flag country  of the vessel.                                                                                              |
| 3         | Year               | char(4)       | [Year](http://vocab.ices.dk/?ref=362)                     | M             | Year                                                                                                                                                    |
| 4         | Month              | int(2)        |                                                           | M             | Month                                                                                                                                                   |
| 5         | NoDistinctVessels  | int(5)        |                                                           | M             | Number of distinct vessels                                                                                                                              |
| 6         | AnonymizedVesselID | nvarchar(500) |                                                           | M             | Anonimyzed vessel ID: Country code + 3 digits and semicolon separated.  For example: ESP001; ESP003;                                                    |
| 7         | ICESrectangle      | char(4)       | [StatRec](http://vocab.ices.dk/?ref=107)                  | M             | ICES Statistical Rectangle                                                                                                                              |
| 8         | MetierL4           | char(25)      | [GearTypeL4](http://vocab.ices.dk/?ref=1498)              | M             | Metier level 4                                                                                                                                          |
| 9         | MetierL5           | char(50)      | [TargetAssemblage](http://vocab.ices.dk/?ref=1499)        | M             | Metier level 5                                                                                                                                          |
| 10        | MetierL6           | char(40)      | [Metier6_FishingActivity](http://vocab.ices.dk/?ref=1647) | M             | Metier level 6                                                                                                                                          |
| 11        | VesselLengthRange  | char(20)      | [VesselLengthClass](http://vocab.ices.dk/?ref=1725)       | M             | Length class for vessels used in data sampling                                                                                                          |
| 12        | VMSEnabled         |               | [YesNoFields](http://vocab.ices.dk/?ref=316)              |               | Yes/No                                                                                                                                                  |
| 13        | FishingDays        | float(15)     |                                                           | M             | Number of fishing days by ICES rectangle. If a  vessel fished in several ICES squares one day, the day will be divided by the number of ICES rectangles |
| 14        | kWFishingDays      |               |                                                           |               | kW\*FishingDays                                                                                                                                         |
| 15        | TotWeight          | float(15)     |                                                           | M             | Total landings of all species caught in **kg**                                                                                                          |
| 16        | TotValue           | float(15)     |                                                           |               | Total value of all species caught in **Euro**                                                                                                           |

M = mandatory



