#!/bin/bash

# execute R script to download shapefiles
Rscript --vanilla src/R/get_data.R

# execute R script to digest in and clean shapefiles for processing
Rscript --vanilla src/R/clean_data.R

# push cleaned shapefiles to AWS S3
aws s3 cp ../data/processed/gpkg s3://earthlab-gridmet/pet --recursive
aws s3 cp ../data/processed/pr s3://earthlab-gridmet/pr --recursive
aws s3 cp ../data/processed/tmmx s3://earthlab-gridmet/tmmx --recursive
aws s3 cp ../data/processed/vs s3://earthlab-gridmet/vs --recursive
