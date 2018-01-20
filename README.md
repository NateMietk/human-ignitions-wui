Costs of human-ignited wildfires in the U.S. (2001-2015)-from the Wildland-Urban Interface to wildlands  =====================================================================================
All R code to run the analysis can be found in `src/R/analysis`

1. Run `1_create_folder.R` 

2. Run `2_get_data.R` to download, unpack, and organize all neccessary files.  The spatial data that are fetched are:
-   Contiguous US state boundaries
-   Level 1,2,3 ecoregions
-   Wildland-urban interface (2010)
-   Fire Program Analysis Fire-Occurrence Database (1992-2015)
-   Monitoring trends in burned severity (1984-2015)

3. Run `3_clean_data.R`. 
  All spatial database processing is accomplished. This particular script is extremely memory intensive due to the large WUI shapfiles and FPA-FOD database.  Because of this it can only be run on a 122 GB RAM EC2 instance on the Amazon Web Services. 

4.  
