# checar funcoes
?occ_data

# baixar ocorrencias
crass_gbif <- occ_data(scientificName = "Eichhornia crassipes", 
                      hasCoordinate = TRUE,
                      hasGeospatialIssue=FALSE)

# dimensoes
dim(crass_gbif)

dim(crass_gbif$data)

# checar campos
crass_gbif$data %>% names

# checar problemas reportados
issues_gbif <- crass_gbif$data$issues %>% 
  unique() %>% 
  strsplit(., "[,]") %>% 
  unlist()

gbif_issues() %>% 
  data.frame() %>% 
  filter(code %in% issues_gbif)


#selecao de variaveis uteis para validacao dos dados
crass_gbif1 <- crass_gbif$data %>%
  dplyr::select(scientificName, acceptedScientificName, decimalLatitude, decimalLongitude,
                issues, waterBody, basisOfRecord, occurrenceStatus, rightsHolder, 
                datasetName, recordedBy, depth, locality, habitat) 


#selecao de variaveis unicas
dori_gbif1 <- dori_gbif1 %>% 
  distinct() 

#impressao do numero de linhas e colunas
dim(dori_gbif1)  

# checar niveis dos fatores
lapply(dori_gbif1, unique)

# checar coordenadas válidas
check_pf <- 
  bdc::bdc_coordinates_outOfRange(
    data = dori_gbif1,
    lat = "decimalLatitude",
    lon = "decimalLongitude")

# checar coordenadas válidas e próximas a capitais (muitas vezes as coordenadas são erroneamente associadas a capitais dos países)

cl <- dori_gbif1 %>%
  CoordinateCleaner::clean_coordinates(species = "acceptedScientificName",
                                       lat = "decimalLatitude",
                                       lon = "decimalLongitude",
                                       tests = c("capitals", 
                                                 "centroids","equal", 
                                                 "gbif", "institutions", 
                                                 "outliers", 
                                                 "zeros"))

# verificar coordenadas com flags

# capitais (padrão é um raio de 10km)
ggplot() +
  borders("world", fill = "lightgray") +
  geom_point(data = crass_gbif1, aes(x = decimalLongitude, y = decimalLatitude)) +
  coord_quickmap() +
  theme_classic()


library(ggmap)
library(maps)
library(mapdata)

world <- map_data('world')

# checar pontos

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = crass_gbif1, aes(x = decimalLongitude, y = decimalLatitude), color = "red") +
  labs(x = "longitude", y = "latitude", title = expression(italic("Eichhornia crassipes")))

#3.3.1
## OBIS - nao fazer