# Set the working directory ot Riyadh and import RIYv1.csv
RIYv1 = read.csv("./Scraped data/Riyadh/Riyadhraw.csv", stringsAsFactors = F)
# first clean the species column
RIYv1$species = gsub(" [Ss]yn.*", "", RIYv1$species)

RIYv1$trait_name_original = RIYv1$trait_name
RIYv1$value_original = RIYv1$value

# RIYv1$species = gsub("[[:punct:]]", "", RIYv1$species)
# RIYv1$species = gsub("U[0-9]{4}", "", RIYv1$species)
# RIYv1$species = gsub(" ssp ", " ", RIYv1$species)
# RIYv1$species = gsub(" spp ", " ", RIYv1$species)
# RIYv1$species = gsub(" subspecies ", " ", RIYv1$species)
# RIYv1$species = gsub(" [Xx] ", "", RIYv1$species)
# RIYv1$species = gsub(" var ", " ", RIYv1$species)
RIYv1$species = gsub("\\s+ ", " ", RIYv1$species)
unique(RIYv1$species)



unique(RIYv1$trait_name)
# the \\1 stands for what is in bracket one. Incredible!  so replace Toxicity and anyhing after it with toxicity
RIYv1$trait_name = gsub("(Toxicity).*", "\\1", RIYv1$trait_name)
unique(RIYv1$trait_name)
library(tidyverse)
# Now get rid of the funny symbols.
library(stringr)

# x2 = gsub(" ", "5", ACTplantselector$Common_name)
x2 = str_match_all(RIYv1$value, "[[:alnum:]|[:blank:]|[:punct:]]" )
x2[lengths(x2) == 0] <- NA_character_



z = character()

for (i in 1:length(RIYv1$value) ){
  y = str_c(x2[[i]], collapse = "")
  
  z =  append(z, y)
}
RIYv1$value = z
#and for species
x2 = str_match_all(RIYv1$species, "[[:alnum:]|[:blank:]|[:punct:]]" )
x2[lengths(x2) == 0] <- NA_character_



z = character()

for (i in 1:length(RIYv1$species) ){
  y = str_c(x2[[i]], collapse = "")
  
  z =  append(z, y)
}
RIYv1$species = z

#Now we chase down the syn. and the var.
RIYv1$species = gsub("\\s+ ", " ", RIYv1$species)
# all g, ready to start the process


unique(RIYv1$trait_name)
# 1. Family Name
RIYv1[RIYv1$trait_name == "Family Name",]$trait_name = "family"
#2. Origin
unique(RIYv1[RIYv1$trait_name == "Origin",]$value)
OR = RIYv1[RIYv1$trait_name == "Origin",]
OR = separate(OR, col = value, into = c("generic", "generic1", "generic2", "generic3","generic4", "generic5", "generic6", "generic7"), sep = ", ")
OR = OR %>% gather(key = "trait_name", value = "value", generic:generic7, na.rm = T)
OR$value = str_trim(OR$value)
unique(OR$value)
OR$value = str_to_lower(OR$value)
OR$value = gsub(" ", "", OR$value)
OR$trait_name = "climate_of_origin"
RIYv1 = rbind(RIYv1[!RIYv1$trait_name == "Origin",], OR)
# 3. Rainfall
# Gonna add these into climate of origin
unique(RIYv1[RIYv1$trait_name == "Rainfall",]$value)
Rain = RIYv1[RIYv1$trait_name == "Rainfall",]
Rain = separate(Rain, col = value, into = c("generic", "generic1", "generic2", "generic3","generic4", "generic5", "generic6", "generic7"), sep = ", ")
Rain = Rain %>% gather(key = "trait_name", value = "value", generic:generic7, na.rm = T)
Rain$value = str_trim(Rain$value)
Rain$value = str_to_lower(Rain$value)
Rain = Rain[!Rain$value == "u0631u0637u0628u0629",]
Rain[Rain$value == "extremely arid",]$value = "arid"
Rain[Rain$value == "very arid",]$value = "arid"
Rain[Rain$value == "extremely humid",]$value = "tropical"
Rain[Rain$value == "very humid",]$value = "tropical"
Rain[Rain$value == "semi humid",]$value = "subtropical"
Rain[Rain$value == "semi arid",]$value = "semiarid"
unique(Rain$value)
Rain$trait_name = "climate_of_origin"
RIYv1 = rbind(RIYv1[!RIYv1$trait_name == "Rainfall",], Rain)

# 4. Propagation - get rid of it
unique(RIYv1[RIYv1$trait_name == "Propagation",]$value)
RIYv1 =     RIYv1[!RIYv1$trait_name == "Propagation",]   
# 5. Maintenance
unique(RIYv1[RIYv1$trait_name == "Maintenance",]$value)
RIYv1$value[RIYv1$value == "Moderate"] = "medium"
RIYv1$value[RIYv1$value == "Moderate , Low"] = "medium"
RIYv1[RIYv1$trait_name == "Maintenance",]$trait_name = "maintenance_level"
RIYv1$value[RIYv1$trait_name == "maintenance_level"] = str_to_lower(RIYv1$value[RIYv1$trait_name == "maintenance_level"])
# 6. Urban Climate
unique(RIYv1[RIYv1$trait_name == "Urban Climate",]$value)
heat = RIYv1[RIYv1$trait_name == "Urban Climate",]
heat$trait_name = "heat_tolerance"
heat[heat$value == "Resistant",]$value = "Yes"
heat[heat$value == "Vulnerable",]$value = "No"


# 7. Dessication
unique(RIYv1[RIYv1$trait_name == "Desiccation",]$value)
sdrought = RIYv1[RIYv1$trait_name == "Urban Climate",]
sdrought$trait_name = "severedrought_tolerance"
sdrought[sdrought$value == "Resistant",]$value = "Yes"
sdrought[sdrought$value == "Vulnerable",]$value = "No"

# 8. Drought
unique(RIYv1[RIYv1$trait_name == "Drought",]$value)

drought = RIYv1[RIYv1$trait_name == "Drought",]
drought$trait_name = "drought_tolerance"
drought[drought$value == "Resistant",]$value = "Yes"
drought[drought$value == "Vulnerable",]$value = "No"
RIYv1 = rbind(RIYv1[!RIYv1$trait_name == "Urban Climate",], heat)
RIYv1 = rbind(RIYv1[!RIYv1$trait_name == "Desiccation",], sdrought)
RIYv1 = rbind(RIYv1[!RIYv1$trait_name == "Drought",], drought)
# 9. Irrigation - need to split but should be easy
unique(RIYv1[RIYv1$trait_name == "Irrigation",]$value)
Irr = RIYv1[RIYv1$trait_name == "Irrigation",]
Irr = separate(Irr, col = value, into = c("generic", "generic1", "generic2", "generic3","generic4", "generic5", "generic6", "generic7"), sep = ", ")
Irr = Irr %>% gather(key = "trait_name", value = "value", generic:generic7, na.rm = T)
Irr$value = str_trim(Irr$value)
Irr$value = str_to_lower(Irr$value)
unique(Irr$value)
Irr$trait_name = "supp_watering"
RIYv1 = rbind(RIYv1[!RIYv1$trait_name == "Irrigation",], Irr)
# 10. Salinity
unique(RIYv1[RIYv1$trait_name == "Salinity",]$value)
Sal = RIYv1[RIYv1$trait_name == "Salinity",]
Sal$value[which(Sal$value == "Low  1,000 ppm")] = "No"
Sal$value[which(Sal$value == "Moderate  3,000 ppm")] = "No"
Sal$value[which(Sal$value == "High  5,000 ppm")] = "Yes"
Sal$value[which(Sal$value == "Very High  5,000 ppm")] = "Yes"
Sal$trait_name = "salinity_tolerance"
RIYv1 = rbind(RIYv1[!RIYv1$trait_name == "Salinity",], Sal)
# 11. Hardiness Zones
unique(RIYv1[RIYv1$trait_name == "Hardiness Zones",]$value)
frost = RIYv1[RIYv1$trait_name == "Hardiness Zones",]
frost$trait_name = "frost_tolerance"
frost$value[which(frost$value == "6C")] = "No"
frost$value[which(frost$value == "3C")] = "No"
frost$value[which(frost$value == "-35C")] = "Yes"
frost$value[which(frost$value == "-25C")] = "Yes"
frost$value[which(frost$value == "-15C")] = "Yes"
frost$value[which(frost$value == "-21C")] = "Yes"
frost$value[which(frost$value == "-18C")] = "Yes"
frost$value[which(frost$value == "-12C")] = "Yes"
frost$value[which(frost$value == "-9C")] = "Yes"
frost$value[which(frost$value == "-25C , Heat Tolerant")] = "Yes"
frost$value
lfrost = frost[which(frost$value == "-6C"|frost$value == "-3C"|frost$value == "0C"),]
lfrost$value = "Yes"
lfrost$trait_name = "lightfrost_tolerance"
frost = frost[which(frost$value == "Yes"|frost$value == "No"),]
RIYv1 = rbind(RIYv1[!RIYv1$trait_name == "Hardiness Zones",], frost, lfrost)
# 12. Vigour
unique(RIYv1[RIYv1$trait_name == "Vigour",]$value)
grate = RIYv1[RIYv1$trait_name == "Vigour",]
grate = separate(grate, col = value, into = c("generic", "generic1", "generic2", "generic3","generic4", "generic5", "generic6", "generic7"), sep = ", ")
grate = grate %>% gather(key = "trait_name", value = "value", generic:generic7, na.rm = T)
grate$value = str_trim(grate$value)
grate$value = str_to_lower(grate$value)
unique(grate$value)
grate$value[which(grate$value == "normal growth rate")] = "medium"
grate$value[which(grate$value == "fairly fast growing")] = "fast"
grate$value[which(grate$value == "fast growing")] = "fast"
grate$value[which(grate$value == "slow growing")] = "slow"
grate$value[which(grate$value == "very slow growing")] = "slow"

grate$trait_name = "growth_rate"

RIYv1 = rbind(RIYv1[!RIYv1$trait_name == "Vigour",], grate)
# 13. Height
unique(RIYv1[RIYv1$trait_name == "Height",]$value)
height = RIYv1[RIYv1$trait_name == "Height",]
height1 = height[grepl("To", height$value) == F,]
height1$value = as.numeric(gsub("[[:alpha:]]", "", height1$value))
height1$value = str_trim(height1$value)
height1$trait_name = "height"
#wow all of them!
height = height[grepl("To", height$value) == T,]
height = separate(height, col = value, into = c("min_height", "max_height"), sep = "To")
height = height %>% gather(key = "trait_name", value = "value", min_height:max_height, na.rm = T)

height$value = as.numeric(gsub("[[:alpha:]]", "", height$value))
height$value = str_trim(height$value)
RIYv1 = rbind(RIYv1[!RIYv1$trait_name == "Height",], height, height1)

# 14. Expansion
width = RIYv1[RIYv1$trait_name == "Expansion",]
width1 = width[grepl("To", width$value) == F,]
width1$value = as.numeric(gsub("[[:alpha:]]", "", width1$value))
width1$value = str_trim(width1$value)
width1$trait_name = "width"
#wow all of them!
width = width[grepl("To", width$value) == T,]
width = separate(width, col = value, into = c("min_width", "max_width"), sep = "To")
width = width %>% gather(key = "trait_name", value = "value", min_width:max_width, na.rm = T)

width$value = as.numeric(gsub("[[:alpha:]]", "", width$value))
width$value = str_trim(width$value)
RIYv1 = rbind(RIYv1[!RIYv1$trait_name == "Expansion",], width, width1)
# 15. Foliage - need to expand out
unique(RIYv1[RIYv1$trait_name == "Foliage",]$value)
Foliage = RIYv1[RIYv1$trait_name == "Foliage",]
Foliage = separate(Foliage, col = value, into = c("generic", "generic1", "generic2", "generic3","generic4", "generic5", "generic6", "generic7"), sep = ", ")
Foliage = Foliage %>% gather(key = "trait_name", value = "value", generic:generic7, na.rm = T)
Foliage$value = str_trim(Foliage$value)
Foliage$value = str_to_lower(Foliage$value)
unique(Foliage$value)
Foliage$value[which(Foliage$value == "semi evergreen")] = "semi_deciduous"
Foliage$trait_name = "leaf_loss"
RIYv1 = rbind(RIYv1[!RIYv1$trait_name == "Foliage",], Foliage)
# 16. Colour (Flower I'm pretty sure) - expand
unique(RIYv1[RIYv1$trait_name == "Colour",]$value)
Colour = RIYv1[RIYv1$trait_name == "Colour",]
Colour = separate(Colour, col = value, into = c("generic", "generic1", "generic2", "generic3","generic4", "generic5", "generic6", "generic7"), sep = ", ")
Colour = Colour %>% gather(key = "trait_name", value = "value", generic:generic7, na.rm = T)
Colour$value = str_trim(Colour$value)
Colour$value = str_to_lower(Colour$value)
Colour = Colour[!Colour$value == "olive",]
unique(Colour$value)
Colour$trait_name = "flower_colour"
# take out everything after the space
Colour$value = gsub("\\s.*", "", Colour$value)

RIYv1 = rbind(RIYv1[!RIYv1$trait_name == "Colour",], Colour)
# 17. Size of flower take this one out
unique(RIYv1[RIYv1$trait_name == "Size",]$value)
RIYv1 = RIYv1[!RIYv1$trait_name == "Size",]
# 18. Flower Period - take this out. The Riyadh climate won't be relevant for us
unique(RIYv1[RIYv1$trait_name == "Flowering Period",]$value)
RIYv1 = RIYv1[!RIYv1$trait_name == "Flowering Period",]
# 19. Smell - simplify to fragrant and fragrant foliage
unique(RIYv1[RIYv1$trait_name == "Smell",]$value)
Smell = RIYv1[RIYv1$trait_name == "Smell",]
Smell = separate(Smell, col = value, into = c("generic", "generic1", "generic2", "generic3","generic4", "generic5", "generic6", "generic7"), sep = ", ")
Smell = Smell %>% gather(key = "trait_name", value = "value", generic:generic7, na.rm = T)
Smell$value = str_trim(Smell$value)
Smell$value = str_to_lower(Smell$value)
unique(Smell$value)
Smell$trait_name = "other_feature"
Smell$value[Smell$value == "flower"] = "fragrant_flowers"
Smell$value[Smell$value == "leaf"] = "fragrant_foliage"

Smell[Smell$value == "malodorous",]$trait_name = "risk"
Smell = Smell[Smell$value == "fragrant_flowers"|Smell$value == "fragrant_foliage"|Smell$value == "malodorous",]
RIYv1 = rbind(RIYv1[!RIYv1$trait_name == "Smell",], Smell)
# 20. Type Of Fruit - will take some cleaning
unique(RIYv1[RIYv1$trait_name == "Type Of Fruit",]$value)
fruit = RIYv1[RIYv1$trait_name == "Type Of Fruit",]
fruit$value = str_to_lower(fruit$value)
fruit$value[fruit$value == "berry"] = "berries"
fruit$value[fruit$value == "nutlet , cones"] = "cone"
fruit$value[fruit$value == "drupe(fleshy/juicy)"] = "drupe"
fruit$value[fruit$value == "cones"] = "cone"
fruit$value[fruit$value == "nutlet , loment"] = "nutlet"
fruit$value[fruit$value == "cones"] = "cone"
fruit$value[fruit$value == "drupe (leathery/fibrous)"] = "drupe"
fruit =fruit[!fruit$value == "schizocarp",]
fruit =fruit[!fruit$value == "silique",]
fruit = fruit[!fruit$value == "star shaped fruit",]
unique(fruit$value)
fruit$trait_name = "fruit_type"
RIYv1 = rbind(RIYv1[!RIYv1$trait_name == "Type Of Fruit",], fruit)
# 21. Location of Use - Placement - needs expanding
unique(RIYv1[RIYv1$trait_name == "Location of Use",]$value)
placement = RIYv1[RIYv1$trait_name == "Location of Use",]
placement = separate(placement, col = value, into = c("generic", "generic1", "generic2", "generic3","generic4", "generic5", "generic6", "generic7", "generic8", "generic9", "generic10", "generic11", "generic12", "generic13", "generic14", "generic15", "generic16", "generic17"), sep = ", ")
placement = placement %>% gather(key = "trait_name", value = "value", generic:generic17, na.rm = T)
placement$value = str_trim(placement$value)
placement$value = str_to_lower(placement$value)
unique(placement$value)
placement$trait_name = "placement"
placement$value[placement$value == "open country"] = "park"
placement$value[placement$value == "public open space"] = "park"
placement$value[placement$value == "pedestrian precinct"] = "amenity"
placement$value[placement$value == "home plant"] = "garden"
placement$value[placement$value == "swimming pool planting"] = "featuretropical"
placement$trait_name[placement$value == "featuretropical"] = "purpose"
placement$value[placement$value == "container planting"] = "potplant"
placement = placement[!placement$value == "wadi plant",]
placement = placement[!placement$value == "wadi farm garden",]
placement = placement[!placement$value == "rowda plant",]
placement = placement[!placement$value == "linear planting",]
placement$value[placement$value == "rock garden"] = "garden"
placement$value[placement$value == "pond edge"] = "wet"
placement$value[placement$value == "pond"] = "wet"
placement$value[placement$value == "street neighbour planting"] = "street"
placement$value[placement$value == "park planting"] = "park"
placement$value[placement$value == "massed planting"] = "massplanting"
placement$trait_name[placement$value == "massplanting"] = "purpose"
placement$value[placement$value == "grove"] = "park"
placement$value[placement$value == "cars park"] = "street"
placement$value[placement$value == "steppe garden"] = "garden"
placement$value[placement$value == "hillside planting"] = "park"
placement$value[placement$value == "undergrowth"] = "groundcover"
placement$trait_name[placement$value == "groundcover"] = "purpose"
placement$value[placement$value == "private garden"] = "garden"
placement$value[placement$value == "small pond"] = "wet"
placement$value[placement$value == "hillside stabilization"] = "erosion"
placement$trait_name[placement$value == "erosion"] = "purpose"
placement$value[placement$value == "highway planting"] = "street"
placement$value[placement$value == "stream edge"] = "wet"
placement$value[placement$value == "specimen"] = "feature"
placement$trait_name[placement$value == "feature"] = "purpose"
placement$value[placement$value == "herb"] = "edible"
placement$trait_name[placement$value == "edible"] = "purpose"
placement = placement[!placement$value == "roof garden",]
placement = placement[!placement$value == "extensive roof garden",]
placement$value[placement$value == "hedge"] = "hedging_possible"
placement$value[placement$value == "high hedge"] = "screen"
placement$trait_name[placement$value == "screen"] = "purpose"
placement$value[placement$value == "flowering hedge"] = "hedging_possible"
placement$value[placement$value == "low hedge"] = "hedging_possible"
placement$trait_name[placement$value == "hedging_possible"] = "habit"
placement$value[placement$value == "grove planting"] = "park"
placement$value[placement$value == "urban area"] = "street"
placement$value[placement$value == "edging plants"] = "border"
placement$trait_name[placement$value == "border"] = "purpose"
placement$value[placement$value == "swamp"] = "wet"
placement = placement[!placement$value == "suitable for commercial use",]
placement$value[placement$value == "tree grille"] = "street"
RIYv1 = rbind(RIYv1[!RIYv1$trait_name == "Location of Use",], placement)
# 22. Other choices - purpose - needs expanding
unique(RIYv1[RIYv1$trait_name == "Other choices",]$value)
purpose = RIYv1[RIYv1$trait_name == "Other choices",]
purpose = separate(purpose, col = value, into = c("generic", "generic1", "generic2", "generic3","generic4", "generic5", "generic6", "generic7", "generic8", "generic9", "generic10", "generic11", "generic12", "generic13", "generic14", "generic15", "generic16", "generic17"), sep = ", ")
purpose = purpose %>% gather(key = "trait_name", value = "value", generic:generic17, na.rm = T)
purpose$value = str_trim(purpose$value)
purpose$value = str_to_lower(purpose$value)
unique(purpose$value)
purpose = purpose[!purpose$value == "environmental consolidation",]

purpose$value[purpose$value == "attractive foliage"] = "foliage"
purpose$trait_name[purpose$value == "foliage"] = "other_feature"

purpose$value[purpose$value == "grouped planting"] = "massplanting"
purpose$trait_name[purpose$value == "massplanting"] = "purpose"

purpose = purpose[!purpose$value == "propagation by runners",]
purpose = purpose[!purpose$value == "mythological",]
purpose = purpose[!purpose$value == "plant used by ada",]

purpose$value[purpose$value == "ground cover"] = "groundcover"
purpose$trait_name[purpose$value == "groundcover"] = "purpose"

purpose$value[purpose$value == "cut flower"] = "cutflowers"
purpose$trait_name[purpose$value == "cutflowers"] = "purpose"

purpose$value[purpose$value == "wind damage"] = "No"
purpose$trait_name[purpose$value == "No"] = "wind_tolerance"

purpose$value[purpose$value == "shoreline stabilization"] = "erosion"
purpose$trait_name[purpose$value == "erosion"] = "purpose"

purpose$value[purpose$value == "noise protection"] = "screen"
purpose$trait_name[purpose$value == "screen"] = "purpose"

purpose$value[purpose$value == "attractive fruits"] = "showey_fruit"
purpose$trait_name[purpose$value == "showey_fruit"] = "other_feature"

purpose$value[purpose$value == "good scent"] = "fragrance"
purpose$trait_name[purpose$value == "fragrance"] = "other_feature"

purpose$value[purpose$value == "attractive to birds"] = "bird"
purpose$trait_name[purpose$value == "bird"] = "ecological_services"

purpose$value[purpose$value == "attractive to bees"] = "bee"
purpose$trait_name[purpose$value == "bee"] = "ecological_services"

purpose$value[purpose$value == "attractive flower"] = "showey_flower"
purpose$trait_name[purpose$value == "showey_flower"] = "other_feature"

purpose$value[purpose$value == "shelter"] = "windbreak"
purpose$trait_name[purpose$value == "windbreak"] = "purpose"

purpose$value[purpose$value == "attractive shape"] = "growth_form"
purpose$trait_name[purpose$value == "growth_form"] = "other_feature"

purpose = purpose[!purpose$value == "nativ in riyadh",]

purpose$value[purpose$value == "crop plant"] = "edible"
purpose$trait_name[purpose$value == "edible"] = "purpose"

purpose$value[purpose$value == "invasive roots"] = "invasive"
purpose$trait_name[purpose$value == "invasive"] = "invasive_roots"

purpose = purpose[!purpose$value == "wall coping",]

purpose$value[purpose$value == "fruit drop"] = "fruitfall"
purpose$trait_name[purpose$value == "fruitfall"] = "risk"

purpose$value[purpose$value == "pollen allergy"] = "allergen"
purpose$trait_name[purpose$value == "allergen"] = "risk"

purpose$value[purpose$value == "dominant plant"] = "possible_weed"
purpose$trait_name[purpose$value == "possible_weed"] = "risk"

purpose$value[purpose$value == "medium hedge"] = "hedging_possible"
purpose$trait_name[purpose$value == "hedging_possible"] = "habit"

purpose$value[purpose$value == "topiary plant"] = "pruning"
purpose$trait_name[purpose$value == "pruning"] = "maintenance_activities"
unique(purpose$value)
purpose = purpose[!purpose$value == "seasonal display",]

purpose$value[purpose$value == "invasive habit"] = "possible_weed"
purpose$trait_name[purpose$value == "possible_weed"] = "risk"

purpose = purpose[!purpose$value == "medical plant",]
purpose = purpose[!purpose$value == "colonisers",]
purpose = purpose[!purpose$value == "wild fruiting plant",]
purpose = purpose[!purpose$value == "cultivar",]
purpose = purpose[!purpose$value == "dye plant",]

purpose$value[purpose$value == "cultivated fruit plant"] = "edible"
purpose$trait_name[purpose$value == "edible"] = "purpose"


RIYv1 = rbind(RIYv1[!RIYv1$trait_name == "Other choices",], purpose)

# 23. Toxicity
unique(RIYv1[RIYv1$trait_name == "Toxicity",]$value)

RIYv1[grepl("Poisonous", RIYv1$value) == T,]$value = "poison"
RIYv1$trait_name[RIYv1$value == "poison"] = "risk"
RIYv1 = RIYv1[!grepl("Edible when processed", RIYv1$value) == T,]
RIYv1 = RIYv1[!grepl("Inedible", RIYv1$value) == T,]
Tox = RIYv1[RIYv1$trait_name == "Toxicity",]
Tox$trait_name = "purpose"
Tox$value = "edible"
RIYv1 = rbind(RIYv1[!RIYv1$trait_name == "Toxicity",], Tox)
# 24. Planting Phase
unique(RIYv1[RIYv1$trait_name == "Planting Phase",]$value)
form1 = RIYv1[RIYv1$trait_name == "Planting Phase",]
form1 = separate(form1, col = value, into = c("generic", "generic1", "generic2", "generic3","generic4", "generic5", "generic6", "generic7", "generic8", "generic9", "generic10", "generic11", "generic12", "generic13", "generic14", "generic15", "generic16", "generic17"), sep = ", ")
form1 = form1 %>% gather(key = "trait_name", value = "value", generic:generic17, na.rm = T)
form1$value = str_trim(form1$value)
form1$value = str_to_lower(form1$value)
unique(form1$value)
form1$trait_name = "form"
form1[form1$value == "biennial",]$trait_name = "longevity"
RIYv1 = rbind(RIYv1[!RIYv1$trait_name == "Planting Phase",], form1)

##This is the end!

write.csv(RIYv1, "./Scraped data/Riyadh/RIYv2.csv", row.names = F)
