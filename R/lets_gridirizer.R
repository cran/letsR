#' Fits a grid into a PresenceAbsence object
#'
#' @author Bruno Vilela
#'
#' @description This function creates a grid in shapefile format and adds its
#'   cells' IDs to the presence-absence matrix. The function was created to
#'   facilitate the use of the PresenceAbsence object for the ones who prefer to
#'   work with a grid in shapefile format.
#'
#' @param x A \code{\link{PresenceAbsence}} object.
#'
#' @return The result is a list of two objects. The first is a grid in shapefile
#'   format; the second is a presence-absence matrix with an aditional column
#'   called SP_ID (shapefile cell identifier).
#'
#'
#' @seealso \code{\link{plot.PresenceAbsence}}
#' @seealso \code{\link{lets.presab.birds}}
#'
#' @examples \dontrun{
#' data(PAM)
#' PAM.grid <- lets.gridirizer(PAM)
#' names(PAM.grid)
#' # Grid in polygon format (can be saved in shapefile)
#' PAM.grid$Grid
#' # Presence-absence matrix (beggining only)
#' head(PAM.grid$Presence[, 1:5])
#' 
#' }
#'
#' @export

lets.gridirizer <- function(x) {
  if (!methods::is(x, "PresenceAbsence")) {
    stop("x is not a PresenceAbsence object")
  } else {
    x <- .check_pam(x)
  }
  grid <- terra::as.polygons(x[[2]], dissolve = FALSE)
  r <- terra::rasterize(grid, x[[2]])
  terra::values(r) <- 1:nrow(grid)
  SP_ID <- terra::extract(r, x[[1]][, 1:2])
  resultado <- cbind(SP_ID, x[[1]])
  names(grid) <- "Species_Richness"
  grid$Species_Richness <- terra::values(x[[2]])
  return(list("Grid" = grid, "Presence_Absence_Matrix" = resultado))
  
}
