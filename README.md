# pfsh.dive

## Overview
`pfsh.dive` contains the data and code used in Adams et al. 2018. 

## Installation
```r
# pfsh.dive is on GitHub, not CRAN, so use the following code for installation
install.packages('devtools')
devtools::install_github('mczapanskiy-usgs/pfsh.dive')_
```

## Usage
```r
# Load metadata
metadata_path <- system.file('extdata',
  'MOC2015PFSHmetadata.csv',
  package = 'pfsh.dive')
metadata <- readr::read_csv(metadata_path)

# Read CEFAS output
tdr_path_ <- system.file('extdata',
  paste0(metadata$TDR_filename[1], '.CSV'),
  package = 'pfsh.dive')
tdr <- read_cefas(example_tdr,
  metadata$Deployed[1],
  metadata$Recovered[1])
  
# Calibrate TDR data
calib_tdr <- calibrate_tdr(tdr, metadata$DeployID[1])

# Summarize dives in a table
summarize_dives(calib_tdr)

# Plot a dive
plot_dive(calib_tdr, 4)
```
