## This script creates a summarizedexperiment object upon calling. This is a modified version of read_summarizedexperiment_asd in "nearspectRa" package


# libraries
library(FieldSpectra)
library(SummarizedExperiment)

# Function to read ASD data and create a SummarizedExperiment object

read_summarizedexperiment_asd <- function(input_path, output_dir) {
  
  # Check and create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Identify ASD files: supports both single files and directories with recursion
  if (length(input_path) == 1 && file.info(input_path)$isdir) {
    asd_files <- list.files(path = input_path, 
                            pattern = "3\\.asd$", 
                            full.names = TRUE, 
                            recursive = TRUE) 
  } else {
    asd_files <- input_path
  }
  
  
  # lists for assay data and metadata
  assay_data <- list()
  asd_metadata_list <- list()
  # Additional metadata from file names
  file_metadata_list <- list() 
  
  # Loop over each ASD file
  for (file in asd_files) {
    asd_data <- read.asd(file.dir = file, out.dir = output_dir, spec.file.ext = ".asd")
    wavelengths <- asd_data$Wavelength
    spectra <- asd_data$Spectra
    
    # Store spectra in the assay data list with the file name as key
    file_base <- basename(file)
    assay_data[[file_base]] <- spectra
    
    # Function to extract the metadata from filename
    #. "101_2018_E_FESRUB_A064_b.asd."
    
    extract_metadata_from_filename <- function(file_base) {
      parts <- strsplit(file_base, "_")[[1]]
      return(data.frame(
        species = parts[4],
        replicate = parts[6],
        campaign = parts[2],
        season = parts[3]
      ))
    }
    
    # Extract metadata
    asd_metadata <- extract.metadata(file.dir = file, spec.file.ext = ".asd", 
                                     out.dir = output_dir, 
                                     instrument = "ASD")
    file_name <- asd_metadata$Spectra_File_Name
    spectral_time <- asd_metadata$Spectrum_Time_UTC
    # Extract metadata from file name
    file_metadata <- extract_metadata_from_filename(file_base)
    
    # Combine metadata
    combined_metadata <- data.frame(file_name = file_name, 
                                    spectral_time = spectral_time)
    asd_metadata_list <- append(asd_metadata_list, list(combined_metadata))
    file_metadata_list <- append(file_metadata_list, list(file_metadata))
  }
  
  # Combine all metadata into a single data frame
  col_metadata <- do.call(rbind, asd_metadata_list)
  file_metadata <- do.call(rbind, file_metadata_list)
  
  # Merge ASD metadata with file name metadata
  col_metadata <- cbind(col_metadata, file_metadata)
  
  # Combine all spectra into the assay matrix
  assay_matrix <- do.call(cbind, assay_data)
  wavelengths <- asd_data$Wavelength  
  
  # Assign row names (wavelengths) to the assay matrix
  rownames(assay_matrix) <- wavelengths
  
  # Create colData (sample-level metadata)
  colData <- DataFrame(samplenames = col_metadata$file_name,
                       spectral_time = col_metadata$spectral_time,
                       species = col_metadata$species,
                       replicate = col_metadata$replicate,
                       campaign = col_metadata$campaign,
                       season = col_metadata$season)
  
  # cat("dim", dim(colData))
  # cat("length", length(colnames(assay_matrix)))
  # cat("fertig")
  rownames(colData) <- colnames(assay_matrix)
  
  # Create rowData (wavelengths metadata)
  rowData <- DataFrame(Wavelengths = rownames(assay_matrix))
  
  # Create SummarizedExperiment object
  se <- SummarizedExperiment(assays = list(counts = as.matrix(assay_matrix)),
                             colData = colData,
                             rowData = rowData)
  
  return(se)
}
