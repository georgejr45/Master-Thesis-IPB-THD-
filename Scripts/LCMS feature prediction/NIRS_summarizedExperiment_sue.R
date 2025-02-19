# R script to read and create an se object from NIRS data (Sue)

library(SummarizedExperiment)
library(FieldSpectra)

#Call the function "read_summarizedExperiment_asd" function make an SE object

#source the script
source("/Users/methungeorge/Desktop/IPB/all_scripts/read_summarizedExperiment_asd.R")
# source("/Users/methungeorge/Desktop/nearspectRa/R/read_summarizedExperiment_asd.R")

#call function

path <- "/Users/methungeorge/Desktop/IPB/Sue_marr/NIRS/MTBLS1224_MacBeSSt_FieldSpec"
NIRS_se <- read_summarizedexperiment_asd(path, output_dir = "/Users/methungeorge/Desktop/IPB/out")
NIRS_se