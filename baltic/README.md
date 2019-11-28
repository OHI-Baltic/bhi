# baltic

<br/>

#### Folders

**`conf`**

Configuration folder containing pressure and resilience matrices and category tables which specify relative importance of different pressures and resilience components, `configuration.R` which specifies weightings of index dimensions and some other configuration details, and a web folder.

**`layers`**

This folder contains all data files to be used for layers in calculating the index scores. These layers are created in the data preparation step, in the `bhi-prep` repo; registering these files in `layers.csv` links the layer with the particular filename.

**`reports`**

Figures and documents subfolders contain content generated for reports, including flowerplots generated by `ohicore`.

**`temp`**

Files generated on the fly by various `ohicore` functions, or in other analyses.

**`testing`**

A folder for conducting additional analyses within a given assessment year, including scenario testing and sensitivity analysis. See `README.md` within the folder for more information. 

<br/>

#### Files

**`calculate_scores.Rmd`**

**`layers.csv` and `layers_metadata.csv`**

**`scores.csv`**