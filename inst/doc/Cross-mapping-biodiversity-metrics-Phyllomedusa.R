## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  warning  = FALSE,
  message  = FALSE,
  fig.height = 5,
  fig.width  = 8,
  fig.align  = "center"
)

## -----------------------------------------------------------------------------
library(letsR)

# Occurrences and climate
data("Phyllomedusa"); data("prec"); data("temp")
prec <- terra::unwrap(prec); temp <- terra::unwrap(temp)

# Geographic PAM (keep empty cells)
PAM  <- lets.presab(Phyllomedusa, remove.cells = FALSE)

# Keep terrestrial landmasses for plotting and binning consistency
wrld_simpl <- get(utils::data("wrld_simpl", package = "letsR"))
PAM <- lets.pamcrop(PAM, terra::vect(wrld_simpl))

# Environmental variables matrix (per geographic cell)
envs <- lets.addvar(PAM, c(temp, prec), onlyvar = TRUE)
colnames(envs) <- c("Temperature", "Precipitation")

# Environmental-space PAM (envPAM)
env_obj <- lets.envpam(PAM, envs, n_bins = 30, remove.cells = FALSE)

# Plot
lets.plot.envpam(env_obj, world = TRUE)

## -----------------------------------------------------------------------------
# Compute env-space descriptors (centrality, border, isolation, etc.)
env_cells <- lets.envcells(env_obj, perc = 0.2)

head(env_cells)

## -----------------------------------------------------------------------------
# Species-level summaries (e.g., mean across occupied env cells)
env_by_species <- lets.summaryze.cells(env_obj, env_cells, func = mean)

head(env_by_species)

## -----------------------------------------------------------------------------
set.seed(123)
sp_vec   <- env_by_species$Species                    # species present in PAM
n_sp     <- length(sp_vec)
trait_a  <- rnorm(n_sp)
trait_b  <- trait_a * 0.2 + rnorm(n_sp)               # correlated trait

attr_df  <- data.frame(Species = sp_vec,
                       trait_a  = trait_a,
                       trait_b  = trait_b)

# Attribute-space PAM (AttrPAM)
attr_obj <- lets.attrpam(attr_df, n_bins = 5)

# Richness map in attribute space
lets.plot.attrpam(attr_obj)

## -----------------------------------------------------------------------------
# Align env-cell descriptors to the order of geographic rows
env_cells_geo <- env_cells[ env_obj$Presence_and_Absence_Matrix_geo[, 1], ]

# Template = geographic richness raster
map_richatt2 <- env_obj$Geo_Richness_Raster
terra::values(map_richatt2) <- NA

# Fill geographic cells with the env-space metric (species-level NOT needed here)
terra::values(map_richatt2)[ env_obj$Presence_and_Absence_Matrix_geo[, 2] ] <-
  env_cells_geo$`Weighted Mean Distance to midpoint`

# Palette and plot
colfunc <- grDevices::colorRampPalette(c("#fff5f0", "#fb6a4a", "#67000d"))
plot(map_richatt2, col = colfunc(200), box = FALSE, axes = FALSE,
     main = "Env centrality (env-space) mapped to geography")

## -----------------------------------------------------------------------------
met_env <- env_by_species$`Weighted Mean Distance to midpoint` 
sp_names <- env_by_species$Species

attr_map <- lets.maplizer.attr(attr_obj, y = met_env, z = sp_names, func = mean)

# Visualize
lets.plot.attrpam(attr_map, mar = c(4,4,4,4))

## -----------------------------------------------------------------------------
# Attribute-space descriptors per cell
attr_cells <- lets.attrcells(attr_obj, perc = 0.2)

# Species-level summaries of attribute-space descriptors
attr_by_species <- lets.summaryze.cells(attr_obj, attr_cells, func = mean)

# Example: map an attribute-space centrality back to geography
met_attr <- attr_by_species$`Weighted Mean Distance to midpoint`
geo_from_attr <- lets.maplizer(PAM, y = met_attr, z = attr_by_species$Species, ras = TRUE)

plot(geo_from_attr$Raster, main = "Attr centrality mapped to geography", col = colfunc(200))

