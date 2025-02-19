# This Script creates a SummarizedExperiment object of LCMS data upon calling. 


# Load the libraries

library(SummarizedExperiment)
library(metabolighteR)

# Read the file and create data frame
file_path <- "/Users/methungeorge/Desktop/IPB/Sue_marr/LCMS/MTBLS1224/m_MTBLS1224_LC-MS_positive_reverse-phase_metabolite_profiling_v2_maf.tsv"

maf <- read.MAF(file_path)
LCMS_data <- maf[, c(6,10, 22:531)]

# Remove rows with NA values
LCMS_data <- na.omit(LCMS_data)

# Make it into a summarizedExperiment
## Prepare the assay data by extracting the feature intensities ( excluding the m/z and RT columns )

assay_data <- as.matrix(LCMS_data[,-(1:2)])

# Combine m/z and RT into rownames
rownames(assay_data) <- paste0("m/z_", LCMS_data$mass_to_charge, "_RT_", LCMS_data$retention_time)

# Create the row data
## Feature meta data
rowData <- DataFrame(mz = LCMS_data$mass_to_charge, rt = LCMS_data$retention_time)
rownames(rowData) <- rownames(assay_data)

# Generate the colData
file_names <- colnames(assay_data)

# Create metadata from the filenames ("pos_025_2018_H_PHLPRA_C133_a_1.E.8_01_12663") 
## Create a function to extract the metadata from file names
extract_metadata <- function(file_name) {
  matches <- strsplit(file_name, "_")[[1]]
  species <- matches[5]
  replicate <- matches[7]
  campaign <- matches[3]
  season <- matches[4]
  
  return(list(species = species, replicate = replicate, campaign = campaign, season = season))
}

# Apply the function to each file and store the metadata as a list
col_metadata <- lapply(file_names, extract_metadata)

# Convert the list to a dataframe
col_metadata_df <- do.call(rbind, col_metadata)
col_metadata_df <- as.data.frame(col_metadata_df)

# Set file names as rownames for matching
rownames(col_metadata_df) <- file_names

colData <- DataFrame(col_metadata_df)
rownames(rowData) <- rownames(assay_data)


# Combine into summarizedExperiment
LCMS_se <- SummarizedExperiment( assays = list(intensities = as.matrix(assay_data)),
                                 rowData = rowData,
                                 colData = colData)

print(LCMS_se)










