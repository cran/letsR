## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  fig.height = 5,
  fig.width  = 8,
  fig.align  = "center"
)

## -----------------------------------------------------------------------------
# Load the package
library(letsR)

## -----------------------------------------------------------------------------
set.seed(123)
n <- 2000
Species  <- paste0("sp", 1:n)
trait_a  <- rnorm(n)
trait_b  <- trait_a * 0.2 + rnorm(n)
df       <- data.frame(Species, trait_a, trait_b)

# Build the attribute-space PAM
attr_obj <- lets.attrpam(df, n_bins = 30)

## -----------------------------------------------------------------------------
lets.plot.attrpam(attr_obj)

## -----------------------------------------------------------------------------
attr_desc <- lets.attrcells(attr_obj, perc = 0.2)
head(attr_desc)

## -----------------------------------------------------------------------------
lets.plot.attrcells(attr_obj, attr_desc)

## -----------------------------------------------------------------------------
attr_desc_by_sp <- lets.summaryze.cells(attr_obj, attr_desc, func = mean)
head(attr_desc_by_sp)

## -----------------------------------------------------------------------------
data("PAM")

n <- length(PAM$Species_name)
Species <- PAM$Species_name
trait_a <- rnorm(n)
trait_b <- trait_a * 0.2 + rnorm(n)
df <- data.frame(Species, trait_a, trait_b)

x <- lets.attrpam(df, n_bins = 4)

cell_desc_geo <- lets.attrcells(x, y = PAM)
head(cell_desc_geo)

## -----------------------------------------------------------------------------
lets.plot.attrcells(x, cell_desc_geo)

