Costs of human-ignited wildfires in the U.S. (2001-2015)-from the Wildland-Urban Interface to wildlands  =======================================================================================================

All R code to run the analysis can be found in `src/R/analysis`

1. Run `1_create_folder.R` 

2.  Run `2_get_data.R` to download, unpack, and organize all neccessary files.  The spatial data that are fetched are:
-   Contiguous US state boundaries
-   Level 3 ecoregions
-   Wildland-urban interface
-   Fire Program Analysis Fire-Occurrence Database
-   Monitoring trends in burned severity

2. Run `helper_functions.R`.  This will load all of the neccessary functions used throughout the data cleaning and plotting applications.

3. Run `clean_data.R`.  All spatial database processing is accomplished in this R script.  The end results of this script are the basis for all figure creation.  This particular script is extremely memory intensive due to the large WUI shapfiles and FPA-FOD database.  Because of this it can only be run on a 122 GB RAM EC2 instance on the Amazon Web Services. 

