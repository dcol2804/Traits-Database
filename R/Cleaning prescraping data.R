library(stringr)
library(dplyr)

g = read.csv("190225prescrapingDB.csv", stringsAsFactors = F)

colnames(g)[3] = "species"
colnames(g)[2] = "date_sourced"

# fix up a couple of manual data entry errors
str(g)
g$value[g$value == "yes"] = "Yes"
g$value[g$value == "minni-ritchi"] = "minniritchi"
g$value[g$value == "weed"] = "possible_weed"
g$value[g$value == "variagations"] = "variagated"

# Take out the blank traits
g = g[!g$value == "",]
# We also need to look at the sources in this part of the script
library(readxl)
sourcelist <- read_excel("Species lists/sourcelist.xlsx")
unique(g$study[!g$study %in% sourcelist$Name])

# Here we go:

g$study[g$study == "PLANTnet"] = "PlantNET"
g$study[g$study == "plantNET"] = "PlantNET"
g$study[g$study == "PLantNET"] = "PlantNET"
g$study[g$study == "PlanTNET"] = "PlantNET"
g$study[g$study == "StirlingWA"] = "Stirlingwa"
g$study[g$study == "weeds_QL"] = "Weeds_QL"
g$study[g$study == "learn2grow"] = "Learn2grow"
g$study[g$study == "iplantz"] = "Iplantz"
g$study[g$study == "magneticisland"] = "Magneticisland"
g$study[g$study == "Rarefruitclub"] = "rarefruitclub"
g$study[g$study == "Noosanativeplants"] = "noosanativeplants"
g$study[g$study == "Yarraranges3"] = "Yarraranges2"
g$study[g$study == "Yarraranges4"] = "Yarraranges2"
g$study[g$study == "Saveourwaterwaysnow"] = "saveourwaterwaysnow"
g$study[g$study == "plantfileonline"] = "Plantfileonline"
g$study[g$study == "PLantmark"] = "Plantmark"
g$study[g$study == "waverlycouncil"] = "Waverlycouncil"
g$study[g$study == "conifers"] = "Conifers"
g$study[g$study == "Worldwidewattle"] = "worldwidewattle"
g$study[g$study == "noosalandcare"] = "Noosalandcare"
g$study[g$study == "usefultempplants"] = "Usefultempplants"
g$study[g$study == "missouriBG"] = "MissouriBG"

unique(g$study[!g$study %in% sourcelist$Name])

blanks = g[g$study == "",]

unique(blanks$species)

# I'm going to manually check each of these as I have to access the sources etc. It should take all day.. 
unique(g$trait_name)
takeaway = c("heat_tolerance", "pollution_tolerance", "shade_tolerance", "multistem_development",
             "leaf_description", "flower_description", "fruit_type", "country_of_origin", "maintenance_level", 
             "maintenance_activities", "pestdisease_risk", "pests_diseases", "synonyms", "shade", "continent_of_origin"
             , "rootdisturbance_tolerance", "dimensions", "climate_of_origin")
blanks = blanks[!blanks$trait_name %in% takeaway,]
blanks = blanks[!blanks$value == "none",]
unique(blanks$species)
###################################

# now make sure the columns are appropriate for the future code:
g$species = gsub("[[:punct:]]", "", g$species)
g$species = str_c(g$species, " ", g$variety, " ", g$Cultivar)
g$species = str_squish(g$species)
unique(g$species[str_count(g$species, "[[:upper:]]")> 1])


g$species = gsub("Magnolia grandiflora Little Gem Little Gem", "Magnolia grandiflora Little Gem",  g$species)
g$species = gsub("Tristaniopsis laurina Luscious Luscious", "Tristaniopsis laurina Luscious",  g$species)
g$species = gsub("Fraxinus angustifolia Raywoodii Raywoodii", "Fraxinus angustifolia Raywoodii",  g$species)
g$species = gsub("Prunus cerasifera Nigra Nigra", "Prunus cerasifera Nigra",  g$species)
g$species = gsub("Ulmus parvifolia Todd Todd", "Ulmus parvifolia Todd",  g$species)

g = g %>% select(- variety, -Cultivar, - comments, - X)
write.csv(g, "190225prescrapingDB.csv", row.names = F)