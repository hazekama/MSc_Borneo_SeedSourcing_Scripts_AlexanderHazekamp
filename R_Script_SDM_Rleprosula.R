# SDM for Rubroshorea leprosula in Borneo
# A very simplified version of a Habitat Suitability model
# based on script authored by Dr. Tobias Fremout, along with Dr. Hannes Gaisberger
# of The Alliance of Bioversity International and CIAT

# - LOAD REQUIRED PACKAGES -
library(terra)       # for spatial raster handling
library(dismo)       # for species distribution modelling using MaxEnt
library(sf)          # for vector spatial data handling
library(viridis)     # for colours
library(rJava)       # required for MaxEnt Java interface

# - SET JAVA MEMORY ALLOCATION -
options(java.parameters = "-Xmx4g")  # Reduce RAM usage to 4GB due to 8GB total of my laptop memory

# - FILE PATHS -
basedir <- getwd()  # base directory is current working directory
occ_file <- file.path(basedir, "Shorea_and_Rubroshore_leprosula_GBIF_XY_Borneo_ExportTable.csv")  # path to GBIF occurrence CSV file
borneo_mask <- file.path(basedir, "raster_Borneo.tif")          # raster for Borneo boundary
clim_dir <- file.path(basedir, "Climate_layers")              # folder with Bioclim and aridity layers
soil_dir <- file.path(basedir, "Soil_layers")                 # folder with soil and elevation layers

# - Local output folder within working directory -
output_dir <- file.path(basedir, "SDM_Outputs_Rleprosula")  # create local folder in working directory
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# - LOAD OCCURRENCE DATA -
presence <- read.csv(occ_file)  # read in occurrence CSV
presence <- na.omit(presence[, c("decimalLongitude", "decimalLatitude")])  # remove rows with NA
colnames(presence) <- c("lon", "lat")  # rename columns to lon/lat
# Had error later on in script, so convert to terra format
presence_vect <- vect(presence, geom = c("lon", "lat"), crs = "EPSG:4326")

# - LOAD BORNEO MASK RASTER -
borneo_r <- rast(borneo_mask)  # read Borneo extent raster

# - LOAD AND STACK ENVIRONMENTAL LAYERS -
# Load only a few, as test to see if laptop can handle this
clim_vars <- c("bio_1.tif", "bio_4.tif", "bio_12.tif", "aridity_index.tif")  # mean temp, seasonality, precipitation, aridity
soil_vars <- c("elevation_WorldClim.tif", "ph.tif")  # terrain and soil ph

clim_files <- file.path(clim_dir, clim_vars)
soil_files <- file.path(soil_dir, soil_vars)

env_all <- rast(c(clim_files, soil_files))  # stack layers

# - CROP AND MASK TO BORNEO -
env_borneo <- crop(env_all, borneo_r)  # crop layers to Borneo extent
env_borneo <- mask(env_borneo, borneo_r)  # mask to Borneo boundary

# - GENERATE BACKGROUND POINTS -
set.seed(2025)  # for reproducibility
background <- spatSample(env_borneo[[1]], size = 5000, method = "random", na.rm = TRUE, xy = TRUE)  # creates background points as I have only provided presence points
# 5000 instead of suggested 10000 by Hannes and Tobias, as not sure how well laptop will run so many points
colnames(background) <- c("lon", "lat")


# - EXTRACT ENVIRONMENTAL VALUES AT PRESENCE AND BACKGROUND POINTS -
presence_vect <- vect(presence) # Convert your presence object to a SpatVector for terra::extract()
pres_vals <- terra::extract(env_borneo, presence_vect)

# Convert background point coordinates to SpatVector
background_vect <- vect(background, geom = c("lon", "lat"), crs = "EPSG:4326") # same co-ordinates as WGS84 but what GBIF uses
background_vect <- vect(background) # same for background 
bg_vals <- terra::extract(env_borneo, background_vect)


pres_df <- data.frame(pa = 1, pres_vals[, -1])  # label presences
bg_df <- data.frame(pa = 0, bg_vals[, -1])       # label backgrounds
mod_data <- na.omit(rbind(pres_df, bg_df))       # combine and remove NAs

# - RUN MAXENT MODEL -
model <- maxent(x = mod_data[, -1], p = mod_data$pa)  # train MaxEnt model

# - MODEL PERFORMANCE -
eval <- evaluate(p = pres_df[, -1], a = bg_df[, -1], model = model)  # calculate AUC
print(eval)  # show results
# class          : ModelEvaluation 
# n presences    : 262 
# n absences     : 4894 
# AUC            : 0.7693445 
# cor            : 0.2483468 
# max TPR+TNR at : 0.5827817 
write.csv(data.frame(threshold(eval), auc = eval@auc), file.path(output_dir, "model_evaluation.csv")) # This is to create a file to of model evaluation

# - PREDICT HABITAT SUITABILITY -
names(env_borneo) <- colnames(mod_data[, -1]) # change names of env_borneo to be the same as mod_data
pred <- predict(env_borneo, model, type = "cloglog", na.rm = TRUE)  # predict habitat suitability map - na.rm = TRUE tells R to skip missing cells
writeRaster(pred, file.path(output_dir, "suitability_map.tif"), overwrite = TRUE)  # save output in my folder

# - PLOT SUITABILITY MAP -
plot(pred, col = viridis(100), main = "Habitat Suitability - Rubroshorea leprosula")

# - TO CREATE A BASIC MODEL EVALUATION -
# Extract thresholds
thresholds <- threshold(eval)

# Extract specific thresholds of interest
thresh_10pct <- thresholds[["spec_sens"]]  # Equal sensitivity and specificity
thresh_10om <- thresholds[["10pct"]]       # 10% training omission rate

# Print threshold values
cat("10% omission threshold:", thresh_10om, "\n")
cat("Equal Sensitivity and Specificity threshold:", thresh_10pct, "\n")

# Save thresholds and AUC to CSV
evaluation_summary <- data.frame(
  AUC = eval@auc,
  Threshold_10pct = thresh_10om,
  Threshold_EqualSensSpec = thresh_10pct
)
write.csv(evaluation_summary, file.path(output_dir, "model_eval_thresholds.csv"), row.names = FALSE)

# ============================
# END OF SCRIPT
# ============================