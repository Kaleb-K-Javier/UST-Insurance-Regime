# Project-specific R library setup
# Works on both local Windows and Linux server

# Create library folder if it doesn't exist
if (!dir.exists("renv_library")) {
  dir.create("renv_library", recursive = TRUE)
}

# Set library path (project library first)
.libPaths(c(file.path(getwd(), "renv_library"), .libPaths()))

# Confirmation message
cat("✓ Using project library:", .libPaths()[1], "\n")
cat("✓ To install packages: source('install_packages.R')\n")
