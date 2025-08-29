# ======================================
# Hybrid Accessible Patch Analysis (Clipped + Dissolved Buffers)
# For Rubroshorea leprosula & R. parvifolia
# Author: Alexander Hazekamp
# ======================================

# --- Libraries ---
library(sf)
library(dplyr)

# --- Paths ---
# NOTE: update base_path depending on whether you are running locally or in the computer room
base_path <- "D:/BioCon/ArcGIS_Borneo_SeedSourcing"
setwd(base_path)

# --- Global Settings ---
options(sf_use_s2 = FALSE)  # planar geometry (avoids s2 errors)

# --- Load Roads Layer, Reproject & Simplify ---
roads <- st_read(file.path(base_path, "OSM_Roads/OSM_Borneo_Combined_Roads.shp"))
roads_proj <- st_transform(roads, 3395) %>%
  st_simplify(dTolerance = 50)   # simplify for speed

# --- Species & Thresholds ---
species_list <- list(
  Rlep = list(
    thresholds = c("Omission", "EqSS", "DanumPlot", "High08"),
    prefix = "Rleprosula",
    folder = file.path(base_path, "Rleprosula_ViablePatches_bySeedZone"),
    code = "Rlep"
  ),
  Rparv = list(
    thresholds = c("Omission", "EqSS", "DanumPlot", "High08"),
    prefix = "Rparvifolia",
    folder = file.path(base_path, "Rparvifolia_ViablePatches_bySeedZone"),
    code = "Rparv"
  )
)

# --- Helper: Get shapefile path for a given species & threshold ---
get_patch_file <- function(species_code, threshold, folder) {
  threshold_folder <- paste0(species_code, "_ViablePatch_SeedZone_", threshold)
  shp_file <- if (species_code == "Rlep") {
    paste0("BreedPatch_", threshold, "_bySeedZone.shp")
  } else {
    paste0("Rparvifolia_Breed_", threshold, "_bySeedZone.shp")
  }
  file.path(folder, threshold_folder, shp_file)
}

# --- Core Hybrid Workflow Function ---
process_hybrid <- function(species, buffer_dist_km = 1) {
  
  buffer_m <- buffer_dist_km * 1000
  cat(paste0("\n>>> Starting hybrid ", buffer_dist_km, " km workflow for ", species$prefix, "\n"))
  
  # --- Load Omission threshold patches to set extent ---
  omission_file <- get_patch_file(species$code, "Omission", species$folder)
  omission <- st_read(omission_file, quiet = TRUE) %>%
    st_transform(3395) %>%
    st_make_valid() %>%
    subset(!st_is_empty(.))
  
  # --- Create dissolved road buffer (cropped to bbox + buffer margin) ---
  bbox_ext <- st_as_sfc(st_bbox(omission) + c(-buffer_m, -buffer_m, buffer_m, buffer_m))
  roads_crop <- st_crop(roads_proj, bbox_ext)
  roads_buffer <- st_buffer(roads_crop, buffer_m)
  roads_buffer_dissolved <- roads_buffer %>%
    summarise(geometry = st_union(geometry))
  
  # --- Output folder ---
  out_dir <- file.path(base_path,
                       paste0("Accessible_", species$prefix, "_", buffer_dist_km, "km_clipped_dissolved"))
  if (!dir.exists(out_dir)) dir.create(out_dir)
  
  # --- Process each threshold ---
  for (th in species$thresholds) {
    cat("\n--- Processing threshold:", th, "---\n")
    
    # Load patches
    patch_file <- get_patch_file(species$code, th, species$folder)
    patches <- st_read(patch_file, quiet = TRUE) %>%
      st_transform(3395) %>%
      st_make_valid() %>%
      subset(!st_is_empty(.))
    
    # Crop patches to buffer bbox (faster than intersecting all)
    patches_cropped <- st_crop(patches, st_bbox(roads_buffer_dissolved))
    
    # Clip patches to dissolved road buffer
    accessible <- st_intersection(patches_cropped, roads_buffer_dissolved)
    
    # Dissolve by ZoneID and calculate area
    accessible_dissolved <- accessible %>%
      group_by(ZoneID) %>%
      summarise(geometry = st_union(geometry), .groups = "drop")
    accessible_dissolved$Area_ha <- st_area(accessible_dissolved) / 10000
    
    # Write output
    out_file <- file.path(out_dir,
                          paste0("Accessible_", species$prefix, "_", th, "_", buffer_dist_km, "km_dissolved.shp"))
    st_write(accessible_dissolved, out_file, delete_dsn = TRUE)
    
    cat("✔ Clipped & Dissolved:", species$prefix, th, buffer_dist_km, "km\n")
  }
  
  cat(paste0("\n>>> Hybrid ", buffer_dist_km, " km workflow for ", species$prefix, " completed!\n"))
}

# --- MAIN EXECUTION ---
# R. leprosula: only needs 5 km (what did in computer room)
process_hybrid(species_list$Rlep, buffer_dist_km = 5)

# R. parvifolia: both 1 km and 5 km
process_hybrid(species_list$Rparv, buffer_dist_km = 1)
process_hybrid(species_list$Rparv, buffer_dist_km = 5)

# ============================
# END OF SCRIPT
# ============================