pretty_label_column <- function(string) {
  switch(
    EXPR = string,
    catch_kg_km2 = "Catch-Per-Unit-Effort (kg*(km^2)^{-1})",
    depth_m = "Depth (m)",
    latitude_dd = "Latitude (\u00B0N)",
    longitude_dd = "Longitude (\u00B0W)"
  )
}
