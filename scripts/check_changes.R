# R script to check changes in a GitHub pull request

# Get environment variables for base and head commits
base_commit <- Sys.getenv("BASE_COMMIT")
head_commit <- Sys.getenv("HEAD_COMMIT")

# Get the list of changed files using git diff
changed_files <- system(paste("git diff --name-only", base_commit, head_commit), intern = TRUE)

# Print changed files (for debugging)
cat("Changed files:\n")
print(changed_files)

# Define allowed directories
allowed_directories <- c("model-metadata/", "model-output/")

# Check if any file is outside the allowed directories
unallowed_changes <- changed_files[!sapply(changed_files, function(file) {
  any(startsWith(file, allowed_directories))
})]

# If there are unallowed changes, print them and exit with an error
if (length(unallowed_changes) > 0) {
  cat("Error: The following files are outside the allowed directories:\n")
  print(unallowed_changes)
  quit(status = 1)  # Exit with an error
} else {
  cat("All changes are within allowed directories.\n")
}
