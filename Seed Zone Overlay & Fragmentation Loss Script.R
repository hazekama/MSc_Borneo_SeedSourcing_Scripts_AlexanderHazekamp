# ======================================
# Seed Zone Overlay & Fragmentation Loss Script
# For: R. leprosula and R. parvifolia
# Author: Alexander Hazekamp
# ======================================

# --- Load Libraries ---
library(terra)     # For raster operations
library(sf)        # For vector operations
library(dplyr)     # For data wrangling
library(tidyr)     # For filling NAs

# --- Set Working Directory ---
base_path <- "C:/Users/Alex/Downloads/MSc Spring Semester/Research Project/ArcGIS_Borneo_SeedSourcing"
setwd(base_path)

# --- Global Settings ---
options(sf_use_s2 = FALSE)  # Disable strict spherical geometry engine to avoid intersection errors

# --- Load Seed Zones ---
# This is the dissolved version with each zone represented as a polygon with unique ID (1–24)
seed_zones_poly <- st_read("seed_zones_Borneo_24_poly_Dissolve.shp")
zone_ids <- data.frame(ZoneID = sort(unique(seed_zones_poly$ZoneID)))

# =====================================================================
# SECTION 1: INTERSECT PATCHES BY SEED ZONE (LEPROSULA & PARVIFOLIA)
# =====================================================================

# Define file paths to your filtered patch layers (after MVP applied)
threshold_names <- c("Omission", "EqSS", "DanumPlot", "High08")

# One combined list for both species
patch_info <- list(
  Rlep = list(
    files = paste0("BreedPatch_", threshold_names, "_bySeedZone.shp"),
    prefix = "Rlep"
  ),
  Rparv = list(
    files = paste0("Rparvifolia_Breed_", threshold_names, "_bySeedZone.shp"),
    prefix = "Rparv"
  )
)

# Create summaries for each species
for (species in names(patch_info)) {
  files <- patch_info[[species]]$files
  prefix <- patch_info[[species]]$prefix
  summary_list <- list()
  
  for (i in seq_along(threshold_names)) {
    threshold <- threshold_names[i]
    shp <- st_read(files[i])  # Read in the patch shapefile
    
    # Identify the correct area column name
    area_col <- names(shp)[tolower(names(shp)) %in% tolower(c("Area_ha", "Area_Ha", "Are_ha"))][1]  # Try to match case-variant column names
    if (is.na(area_col)) {
      warning(paste("\u26a0\ufe0f No area column found in", files[i]))
      next  # Skip to next threshold
    }
    
    # Drop geometry for speed and summarise area and patch count
    summary_tbl <- shp %>%
      st_drop_geometry() %>%
      filter(!is.na(ZoneID)) %>%  # Remove geometries without a seed zone
      group_by(ZoneID) %>%
      summarise(
        !!paste0(threshold, "_Total_Ha") := sum(.data[[area_col]], na.rm = TRUE),  # Total area per zone
        !!paste0(threshold, "_Patch_Count") := n(),                                # Patch count per zone
        .groups = "drop"
      )
    
    summary_list[[threshold]] <- summary_tbl
    cat("\u2714\ufe0f Finished summarising", prefix, threshold, "\n")
  }
  
  # Merge all summaries together by ZoneID
  final_summary <- zone_ids
  for (tbl in summary_list) {
    final_summary <- left_join(final_summary, tbl, by = "ZoneID")
  }
  
  # Replace NAs with 0s for clean summary matrix
  final_summary[is.na(final_summary)] <- 0
  
  # Save output
  write.csv(final_summary, paste0("Summary_SeedZone_ByThreshold_", prefix, ".csv"), row.names = FALSE)
}

# =====================================================================
# SECTION 2: FRAGMENTATION LOSS ANALYSIS (LEPROSULA & PARVIFOLIA)
# =====================================================================

# Define paths to unfiltered and filtered patch layers
fragmentation_inputs <- list(
  Rlep = list(
    all = c("Polygon_Patch_Rlep_Omission.shp", "Polygon_Patch_Rlep_EqSS.shp", "Polygon_Patch_Rlep_Danum.shp", "Polygon_Patch_Rlep_High08.shp"),
    filtered = paste0("BreedPatch_", threshold_names, "_bySeedZone.shp"),
    out_csv = "Fragmentation_Loss_Wide_BySeedZone_Rlep.csv"
  ),
  Rparv = list(
    all = c("Rpav_Poly_Omission_Patch.shp", "Rpav_Poly_EqSS_Patch.shp", "Rpav_Poly_Danum_Patch.shp", "Rpav_Poly_High08_Patch.shp"),
    filtered = paste0("Rparvifolia_Breed_", threshold_names, "_bySeedZone.shp"),
    out_csv = "Fragmentation_Loss_Wide_BySeedZone_Rparv.csv"
  )
)

# Loop through both species
for (species in names(fragmentation_inputs)) {
  all_patch_paths <- fragmentation_inputs[[species]]$all
  filtered_patch_paths <- fragmentation_inputs[[species]]$filtered
  wide_summary_list <- list()
  
  for (i in seq_along(threshold_names)) {
    threshold <- threshold_names[i]
    all_patches <- st_read(all_patch_paths[[i]])  # Unfiltered patches (all suitable habitat)
    filtered_patches <- st_read(filtered_patch_paths[[i]])  # Filtered by MVP (e.g., >= 120.43 ha)
    
    # Ensure CRS match
    if (!st_crs(all_patches) == st_crs(seed_zones_poly)) {
      all_patches <- st_transform(all_patches, st_crs(seed_zones_poly))
    }
    
    # Detect area column in each dataset
    area_col_all <- names(all_patches)[tolower(names(all_patches)) %in% tolower(c("Area_ha", "Area_Ha", "Are_ha"))][1]  # fallback for inconsistent column names
    area_col_filtered <- names(filtered_patches)[tolower(names(filtered_patches)) %in% tolower(c("Area_ha", "Area_Ha", "Are_ha"))][1]
    
    if (is.na(area_col_all) || is.na(area_col_filtered)) {
      warning(paste("\u26a0\ufe0f Area column missing for threshold:", threshold))
      next
    }
    
    # Intersect all habitat patches with seed zones
    all_patches_with_zones <- st_intersection(st_make_valid(all_patches), seed_zones_poly) %>%
      filter(!is.na(.data[[area_col_all]]), !is.na(ZoneID)) %>%
      st_drop_geometry() %>%
      group_by(ZoneID) %>%
      summarise(!!paste0("Total_Ha_All_", threshold) := sum(.data[[area_col_all]], na.rm = TRUE))
    
    # Summarise filtered (viable) patches
    filtered_sum <- filtered_patches %>%
      st_drop_geometry() %>%
      filter(!is.na(ZoneID)) %>%
      group_by(ZoneID) %>%
      summarise(!!paste0("Total_Ha_Filtered_", threshold) := sum(.data[[area_col_filtered]], na.rm = TRUE))
    
    # Join both and calculate % fragmentation loss per seed zone
    summary <- full_join(all_patches_with_zones, filtered_sum, by = "ZoneID") %>%
      mutate(
        !!paste0("Fragmentation_Loss_Percent_", threshold) :=
          ifelse(
            get(paste0("Total_Ha_All_", threshold)) > 0,  # Avoid divide-by-zero
            100 * (1 - get(paste0("Total_Ha_Filtered_", threshold)) /
                     get(paste0("Total_Ha_All_", threshold))),
            NA  # If no habitat in zone, return NA
          )
      )
    
    wide_summary_list[[threshold]] <- summary
    cat("\u2714\ufe0f Processed:", species, threshold, "\n")
  }
  
  # Merge all summaries by ZoneID
  final_summary <- Reduce(function(x, y) full_join(x, y, by = "ZoneID"), wide_summary_list)
  
  # Merge with full list of seed zones
  final_summary <- left_join(zone_ids, final_summary, by = "ZoneID")
  
  # Replace NA with 0 in area columns only (not for % loss columns)
  cols_total <- grep("Total_Ha", names(final_summary), value = TRUE)
  final_summary[cols_total] <- lapply(final_summary[cols_total], function(x) replace_na(x, 0))
  
  # Export to CSV
  write.csv(final_summary, fragmentation_inputs[[species]]$out_csv, row.names = FALSE)
}

# ============================
# END OF SCRIPT
# ============================