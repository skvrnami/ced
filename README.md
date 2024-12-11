# Czech Election Data

## Content

- `_targets.R` - script containing the whole pipeline for processing data
- `data/` - folder with data
  - `EP2004` - European Parliament elections
  - `KV1994` - municipal elections
  - `KZ2000` - regional elections
  - `PS1996` - parliamentary elections (Chamber of Deputies, lower chamber)
  - `SE1996_2022` - parliamentary elections (Senate, upper chamber)
- `donation_data/` - data and scripts related to donations
- `R/` - folder with helper functions for processing data
- `_targets/`

## Replication

To replicate matching, run the following code: 
```
# TODO: install dependencies

# source the pipeline
source("_targets.R")
# run the pipeline
targets::tar_make()
```

