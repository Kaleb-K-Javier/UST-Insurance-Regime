# R Environment Setup

This project uses a project-specific R library to avoid dependency conflicts.

## First-Time Setup (Both Local & Server)

1. Clone the repo
2. Open R in the project directory
3. Run: `source('install_packages.R')`

The `.Rprofile` file will automatically configure the library path.

## Verifying Setup

`r
.libPaths()  # Should show ./renv_library first
`

## Adding New Packages

1. Install normally: `install.packages("newpackage")`
2. Update `install_packages.R` to include the new package
3. Commit and push changes
