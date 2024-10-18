# create_directories.R

# Load necessary libraries
library(fs)

# Define the paths
metadata_dir <- "./model-metadata/"
output_dir <- "./model-output/"

# Create output directory if it doesn't exist
if (!dir_exists(output_dir)) {
  dir_create(output_dir)
}

# Get the list of YAML files in the model-metadata directory
yaml_files <- dir_ls(metadata_dir, regexp = "\\.yaml$")

# Create directories for each YAML file if they don't already exist
for (yaml_file in yaml_files) {
  # Get the name of the YAML file without extension
  file_name <- path_ext_remove(path_file(yaml_file))
  
  # Define the target directory in model-output
  target_dir <- path(output_dir, file_name)
  
  # Create the directory if it doesn't exist
  if (!dir_exists(target_dir)) {
    dir_create(target_dir)
    cat("Created directory:", target_dir, "\n")
  } else {
    cat("Directory already exists:", target_dir, "\n")
  }
}
