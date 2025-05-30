iris <- read.csv("C:/Users/jorca/OneDrive/Área de Trabalho/AtividadeCienciaAberta_Arquivounico2.csv", header = T)

lapply(iris, unique)

iris %>% 
  select(Species, Sepal.Length:Petal.Width) %>% 
  pivot_longer(cols = -Species, names_to = "variavel", values_to = "valores") %>% 
  ggplot(aes(x = valores, fill = Species)) +
  geom_histogram() +
  facet_wrap(~ variavel, scales = 'free_x') +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(x = "tamanho (mm)") +
  scale_fill_discrete(
    expression(bold("Species:")),
    labels = c(expression(italic("Iris setosa")), 
               expression(italic("Iris versicolor")), 
               expression(italic("Iris virginica"))))

rules <- validator(in_range(lat, min = -90, max = 90),
                   in_range(lat, min = -180, max = 180),
                   is.character(site),
                   is.numeric(date),
                   all_complete(iris))

out   <- confront(iris, rules)
summary(out)

# check taxa
species <- iris %>% 
  distinct(Species) %>% 
  pull() %>% 
  c("Iris murchosa", .) %>% # inserimos uma espécie fictícia para teste
  filter_name(., provider = "itis") %>% 
  data.frame() %>% 
  bind_cols(Species = iris %>% 
              distinct(Species) %>% 
              pull())

iris_1 <- iris %>% 
  dplyr::mutate(eventID = paste(site, date, sep = "_"), # create indexing fields 
                occurrenceID = paste(site, date, amostra, sep = "_")) %>% 
  left_join(species %>% 
              select(Species, acceptedNameUsageID, scientificName)) %>% # add species unique identifier
  dplyr::rename(decimalLongitude = long, # rename fields according to DwC 
                decimalLatitude = lat,
                eventDate = date) %>% 
  mutate(geodeticDatum = "WGS84", # and add complimentary fields
         verbatimCoordinateSystem = "decimal degrees",
         georeferenceProtocol = "Random coordinates obtained from Google Earth",
         locality = "Gaspe Peninsula",
         recordedBy = "Edgar Anderson",
         taxonRank = "Species",
         organismQuantityType = "individuals",
         basisOfRecord = "Human observation")


## create eventCore
eventCore <- iris_1 %>% 
  select(eventID, eventDate, decimalLongitude, decimalLatitude, locality, site,
         geodeticDatum, verbatimCoordinateSystem, georeferenceProtocol) %>% 
  distinct() 


## create occurrence
occurrences <- iris_1 %>% 
  select(eventID, occurrenceID, scientificName, acceptedNameUsageID,
         recordedBy, taxonRank, organismQuantityType, basisOfRecord) %>%
  distinct() 

## create measurementsOrFacts
eMOF <- iris_1 %>% 
  select(eventID, occurrenceID, recordedBy, Sepal.Length:Petal.Width) %>%  
  pivot_longer(cols = Sepal.Length:Petal.Width,
               names_to = "measurementType",
               values_to = "measurementValue") %>% 
  mutate(measurementUnit = "cm",
         measurementType = plyr::mapvalues(measurementType,
                                           from = c("Sepal.Length", "Sepal.Width", "Petal.Width", "Petal.Length"), 
                                           to = c("sepal length", "sepal width", "petal width", "petal length")))


# check if all eventID matches
setdiff(eventCore$eventID, occurrences$eventID)

setdiff(eventCore$eventID, eMOF$eventID)

setdiff(occurrences$eventID, eMOF$eventID)

# check NA values
eMOF %>%
  filter(is.na(eventID))

occurrences %>%
  filter(is.na(eventID))

rm(list = setdiff(ls(), c("eventCore", "occurrences", "eMOF")))

files <- list(eventCore, occurrences, eMOF) 
data_names <- c("DF_eventCore","DF_occ","DF_eMOF")
dir.create("Dwc_Files")


for(i in 1:length(files)) {
  path <- paste0(getwd(), "/", "DwC_Files")
  write.csv(files[[i]], paste0(path, "/", data_names[i], ".csv"))
}

