## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  fig.height = 5, 
  fig.width = 8,
  fig.align = 'center'
)

## ----message = FALSE, warning = FALSE, r, fig.width = 4-----------------------
# Load the package
library(letsR)

# Load species occurrences
data("Phyllomedusa")

# Load and unwrap environmental rasters
data("prec")
data("temp")
prec <- unwrap(prec)
temp <- unwrap(temp)

## -----------------------------------------------------------------------------
# Generate a geographic PAM
pam <- lets.presab(Phyllomedusa, remove.cells = FALSE)

# Crop the PAM to the world's landmasses
data("wrld_simpl", package = "letsR")
pam <- lets.pamcrop(pam, terra::vect(wrld_simpl))


## -----------------------------------------------------------------------------
# Extract environmental values
envs <- lets.addvar(pam, c(temp, prec), onlyvar = TRUE)
colnames(envs) <- c("Temperature", "Precipitation")

## -----------------------------------------------------------------------------
# Transform PAM into environmental space
res <- lets.envpam(pam, envs)

## -----------------------------------------------------------------------------
res$Presence_and_Absence_Matrix_env[1:5, 1:5]

## -----------------------------------------------------------------------------
res$Presence_and_Absence_Matrix_geo[1:5, 1:5]

## -----------------------------------------------------------------------------
lets.plot.envpam(res,
            world = TRUE)

## -----------------------------------------------------------------------------
lets.plot.envpam(res, species = "Phyllomedusa atelopoides")

## -----------------------------------------------------------------------------
data("IUCN")
# Map mean description year
res_map <- lets.maplizer.env(res, 
                             y = IUCN$Description_Year, 
                             z = IUCN$Species)


## -----------------------------------------------------------------------------
# Plotting trait maps
lets.plot.envpam(res_map)

## -----------------------------------------------------------------------------
out <- lets.envcells(res)  # perc controls the robust border metric
head(out)

## -----------------------------------------------------------------------------
lets.plot.envcells(res, out)

## -----------------------------------------------------------------------------
ras_list <- lets.plot.envcells(res, out, ras = TRUE, plot_ras = FALSE)

## -----------------------------------------------------------------------------
centrality <- out[["Weighted Mean Distance to midpoint"]]   # larger = more central
rich_env   <- rowSums(res$Presence_and_Absence_Matrix_env[, -(1:3), drop = FALSE])

# Mantain cells without zero
keep <- res$Presence_and_Absence_Matrix_env[, 1]
centrality <- centrality[keep]


# Plot relationship
plot(centrality, rich_env,
     xlab = "Centrality (inverse distance to weighted midpoint)",
     ylab = "Species richness",
     pch  = 19)
abline(lm(rich_env ~ centrality), lwd = 2)

