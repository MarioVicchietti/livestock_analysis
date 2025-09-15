dataDir <- "~/ProjetoFGVMagPIE/colrow"
biomes <- system.file("extdata/shape", "br_biomes.shp", package = "colrow") %>% sf::read_sf()
map <- readRDS("C:/Users/Work/Desktop/MAgPIE/magpie/output/default_2025-05-15_10.23.38/clustermap_rev4.117_c200_67420_h12.rds")
BRA <- subset(map, country == 'BRA')
coords <- strsplit(BRA$cell, "\\.")  
mat <- do.call(rbind, coords) 
BRA$lon <- mat[,1]
BRA$lat <- mat[,2]
BRA$iso <- mat[,3]
BRA$lon <- as.numeric(gsub("p", ".", BRA$lon))
BRA$lat <- as.numeric(gsub("p", ".", BRA$lat))

r <- rasterFromXYZ(cbind(BRA[,c("lon","lat")], z=1), 
                   res = c(min(diff(sort(unique(BRA$lon)))),
                           min(diff(sort(unique(BRA$lat))))))
polys <- rasterToPolygons(r, dissolve = FALSE)
crs(polys) <- CRS("+init=EPSG:4326")
cells_sf  <- st_as_sf(polys) %>%
  st_transform(st_crs(biomes))
biomes <- st_transform(biomes, st_crs(cells_sf))
cells_sf  <- st_make_valid(cells_sf)
biomes <- st_make_valid(biomes)
int_sf <- st_intersection(
  cells_sf  %>% mutate(cell_id = row_number()),
  biomes %>% dplyr::select(CD_LEGEN1))
int_sf <- int_sf %>%
  filter(!st_is_empty(.)) %>%
  st_collection_extract("POLYGON")
int_sf <- int_sf %>%
  mutate(area = st_area(geometry))
majoritario <- int_sf %>%
  group_by(cell_id) %>%
  slice_max(area, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  dplyr::select(cell_id, CD_LEGEN1)
cells_bioma <- cells_sf %>%
  mutate(cell_id = row_number()) %>%
  left_join(
    st_set_geometry(majoritario, NULL),
    by = "cell_id"
  ) %>%
  dplyr::select(-cell_id)
cells_bioma$CD_LEGEN1 <- 
  iconv(cells_bioma$CD_LEGEN1, from = "latin1", to = "UTF-8")

cells_bioma <- cells_bioma %>%
  mutate(CD_LEGEN1 = factor(CD_LEGEN1,
                            levels = sort(unique(CD_LEGEN1))))

BRA <- BRA %>%
  mutate(ord = row_number(),
         key_bra = sprintf("%.6f_%.6f", lon, lat))
cent <- st_centroid(cells_sf)
xy   <- st_coordinates(cent)
cells_bioma <- cells_bioma %>%
  mutate(lon = xy[,1], lat = xy[,2],
         key_cell = sprintf("%.6f_%.6f", lon, lat))

cells_bioma <- cells_bioma %>%
  dplyr::left_join(BRA %>% dplyr::select(key_bra, ord),
                   by = c("key_cell" = "key_bra")) %>%
  dplyr::arrange(ord) %>%
  dplyr::select(-ord, -lon, -lat, -key_cell)

biomes <- st_make_valid(biomes)
if (st_crs(biomes) != st_crs(cells_bioma)) {
  biomes <- st_transform(biomes, st_crs(cells_bioma))
}

biomes_border <- biomes %>%
  group_by(CD_LEGEN1) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

sf::sf_use_s2(FALSE)
pts_bra <- st_as_sf(
  BRA,
  coords = c("lon", "lat"),
  crs = 4326
)
pts_bra <- st_transform(pts_bra, st_crs(cells_sf))
cent <- st_centroid(cells_sf)
idx <- st_nearest_feature(pts_bra, cent)
cells_bioma_ord <- cells_bioma[idx, ]
