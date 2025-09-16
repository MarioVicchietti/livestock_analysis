sf::st_crs(bovinos)
sf::st_crs(myCR)

myCR <- sf::st_transform(myCR, sf::st_crs(bovinos))

sf::sf_use_s2(FALSE)
BRA <- subset(map, country == 'BRA')
BRA$cell_index <- seq_len(nrow(BRA))
cells_bioma$cell_index <- BRA$cell_index
myCR <- cells_bioma

process <- function(shp){
  lapply(1:5565, function(i){
    print(i)
    inters <- sf::st_intersection(myCR, bovinos[i,])
    
    if (nrow(inters) > 0){
      result <- inters %>%
        dplyr::mutate(inter_area = sf::st_area(.)) %>%
        dplyr::mutate(area = sf::st_area(bovinos[i,])) %>%
        as.data.frame() %>%
        dplyr::select(cell_index, code_muni, value, inter_area, area)
      
      return(result)
    }
    
    return(NULL)
  })
}

myCR <- sf::st_transform(myCR, sf::st_crs(bovinos))
result <- process(bovinos)

final <- do.call(rbind, result) %>% 
  dplyr::mutate(prop = inter_area / area) %>% 
  dplyr::mutate(prop = units::drop_units(prop)) %>% 
  dplyr::mutate(mvalue = value * prop) %>% 
  dplyr::filter(!is.infinite(mvalue)) %>% 
  dplyr::group_by(cell_index) %>%
  dplyr::summarise(mvalue = sum(mvalue, na.rm = TRUE))

PPM <- rep(0, nrow(BRA))
final$cell_index <- as.integer(final$cell_index)
PPM[final$cell_index] <- final$mvalue / 1e6
cells_bioma$v <- PPM