# ========================
# Accessible Patch Analysis Script
# For Shorea leprosula and Shorea parvifolia
# Author: Alexander Hazekamp
# ========================

# --- Libraries ---
library(sf)
library(dplyr)

# --- Paths ---
base_path <- "D:/BioCon/ArcGIS_Borneo_SeedSourcing"     # Data folder that contains needed layers
working_dir <- "C:/Users/HAZEKAMA/Downloads"            # Temporary R session dir
setwd(working_dir)

# --- Global Settings ---
options(sf_use_s2 = FALSE)  # Disable spherical geometry for planar ops

# --- Load Full Roads Layer & Reproject ---
roads_path <- file.path(base_path, "OSM_Borneo_Combined_Roads.shp")
roads_all <- st_read(roads_path)
roads_proj <- st_transform(roads_all, 3395)  # EPSG:3395 - World Mercator

# --- Create Road Buffers ---
roads_buffer_1km <- st_buffer(roads_proj, dist = 1000)
roads_buffer_5km <- st_buffer(roads_proj, dist = 5000)

# ====================================================================
# SECTION 1: ACCESSIBLE PATCHES - R. leprosula
# ====================================================================

# --- 1A. 1km Omission Patch Filtering ---
# First one done alone to test if script works and find any problems
omission_leprosula <- st_read(file.path(base_path, "BreedPatch_Omission_bySeedZone.shp"))
omission_proj <- st_transform(omission_leprosula, 3395) |> st_make_valid() |> 
  subset(!st_is_empty(.))

# Crop buffer to extent of omission patches (to avoid reading 2,000,000+ roads in Borneo)
# Cropped to 10% Omission, as this is the lowest Habitat Suitability threshold used
# Therefore all other thresholds are included within this crop
# This can be changed depending what threshold in your study is lowest 
roads_crop_1km <- st_crop(roads_buffer_1km, st_bbox(omission_proj))

# Intersect and extract accessible omission patches
intersects_omit <- lengths(st_intersects(omission_proj, roads_crop_1km)) > 0
accessible_omit_leprosula <- omission_proj[intersects_omit, ]

# Save accessible omission patches
st_write(accessible_omit_leprosula,
         file.path(base_path, "Accessible_Rleprosula_Omission_Patches.shp"),
         delete_dsn = TRUE)

# --- 1B. Remaining Thresholds (1km) ---
thresholds_leprosula <- c("EqSS", "DanumPlot", "High08")
patch_files_leprosula <- paste0("BreedPatch_", thresholds_leprosula, "_bySeedZone.shp")
out_dir_lep_1km <- file.path(base_path, "Accessible_Patches_Rleprosula")
if (!dir.exists(out_dir_lep_1km)) dir.create(out_dir_lep_1km)

for (i in seq_along(thresholds_leprosula)) {
  threshold <- thresholds_leprosula[i] # Get the current threshold name (e.g., "EqSS", "DanumPlot", etc.)
  patch_path <- file.path(base_path, patch_files_leprosula[i]) # Construct full file path for the shapefile
  patches <- st_read(patch_path) |> # Read in the shapefile
    st_transform(3395) |> # Reproject to EPSG:3395 (World Mercator)
    st_make_valid() |> # Ensure geometries are valid
    subset(!st_is_empty(.)) # Remove any empty geometries
  
  intersects <- lengths(st_intersects(patches, roads_crop_1km)) > 0 # Check which patches intersect with 1 km road buffer
  accessible <- patches[intersects, ] # Subset only those patches that intersect (i.e., are accessible)
  
  st_write(accessible,
           file.path(out_dir_lep_1km, paste0("Accessible_Rleprosula_", threshold, "_Patches.shp")),
           delete_dsn = TRUE)
}

# --- 1C. All Thresholds (5km) ---
thresholds_leprosula_all <- c("Omission", "EqSS", "DanumPlot", "High08")
patch_files_leprosula_all <- paste0("BreedPatch_", thresholds_leprosula_all, "_bySeedZone.shp")
buffer_crop_5km_lep <- st_crop(roads_buffer_5km, st_bbox(omission_proj))
out_dir_lep_5km <- file.path(base_path, "Accessible_Patches_leprosula_5km")
if (!dir.exists(out_dir_lep_5km)) dir.create(out_dir_lep_5km)

for (i in seq_along(thresholds_leprosula_all)) {
  threshold <- thresholds_leprosula_all[i]
  patch_path <- file.path(base_path, patch_files_leprosula_all[i])
  patches <- st_read(patch_path, quiet = TRUE) |> 
    st_transform(3395) |> 
    st_make_valid() |> 
    subset(!st_is_empty(.))
  
  intersects <- lengths(st_intersects(patches, buffer_crop_5km_lep)) > 0
  accessible <- patches[intersects, ]
  
  st_write(accessible,
           file.path(out_dir_lep_5km, paste0("Accessible_leprosula_", threshold, "_Patches_5km.shp")),
           delete_dsn = TRUE)
}

# ====================================================================
# SECTION 2: ACCESSIBLE PATCHES - R. parvifolia
# ====================================================================

# --- 2A. Load Parvifolia Omission for Cropping ---
omission_parvi <- st_read(file.path(base_path, "Rparvifolia_Breed_Omission_bySeedZone.shp")) |> 
  st_transform(st_crs(roads_buffer_1km)) |> 
  st_make_valid()

# --- 2B. Create Cropped Buffers (1km + 5km) ---
buffer_crop_parvi_1km <- st_crop(roads_buffer_1km, st_bbox(omission_parvi))
buffer_crop_parvi_5km <- st_crop(roads_buffer_5km, st_bbox(omission_parvi))

# --- 2C. Process All Thresholds ---
thresholds_parvi <- c("Omission", "EqSS", "DanumPlot", "High08")
patch_files_parvi <- paste0("Rparvifolia_Breed_", thresholds_parvi, "_bySeedZone.shp")

# 1 km results
out_dir_parvi_1km <- file.path(base_path, "Accessible_Patches_parvifolia")
if (!dir.exists(out_dir_parvi_1km)) dir.create(out_dir_parvi_1km)

for (i in seq_along(thresholds_parvi)) {
  threshold <- thresholds_parvi[i]
  patch_path <- file.path(base_path, patch_files_parvi[i])
  patches <- st_read(patch_path) |> 
    st_transform(3395) |> 
    st_make_valid() |> 
    subset(!st_is_empty(.))
  
  intersects <- lengths(st_intersects(patches, buffer_crop_parvi_1km)) > 0
  accessible <- patches[intersects, ]
  
  st_write(accessible,
           file.path(out_dir_parvi_1km, paste0("Accessible_parvifolia_", threshold, "_Patches.shp")),
           delete_dsn = TRUE)
}

# 5 km results
out_dir_parvi_5km <- file.path(base_path, "Accessible_Patches_parvifolia_5km")
if (!dir.exists(out_dir_parvi_5km)) dir.create(out_dir_parvi_5km)

for (i in seq_along(thresholds_parvi)) {
  threshold <- thresholds_parvi[i]
  patch_path <- file.path(base_path, patch_files_parvi[i])
  patches <- st_read(patch_path, quiet = TRUE) |> 
    st_transform(3395) |> 
    st_make_valid() |> 
    subset(!st_is_empty(.))
  
  intersects <- lengths(st_intersects(patches, buffer_crop_parvi_5km)) > 0
  accessible <- patches[intersects, ]
  
  st_write(accessible,
           file.path(out_dir_parvi_5km, paste0("Accessible_parvifolia_", threshold, "_Patches_5km.shp")),
           delete_dsn = TRUE)
}

# ============================
# END OF SCRIPT
# ============================