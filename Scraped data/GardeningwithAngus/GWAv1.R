#set working directory to Gardening with angus and import GWAv1.csv
GWAv1 = read.csv("./Scraped data/GardeningwithAngus/Gardeningwithangusraw.csv", stringsAsFactors = F)


library(dplyr)
library(tidyr)
library(stringr)

# Add the study column and reorder columns
GWAv1$study = "Gardeningwithangus"
GWAv1 = data.frame( study = GWAv1$study, species = GWAv1$Species, trait_name = GWAv1$trait_name, value = GWAv1$value, stringsAsFactors = F)

GWAv1$trait_name_original = GWAv1$trait_name
GWAv1$value_original = GWAv1$value
# Clean up the species column

# GWAv1$species = gsub("[[:punct:]]", "", GWAv1$species)
# GWAv1$species = gsub(" ssp ", " ", GWAv1$species)
# GWAv1$species = gsub(" subspecies ", " ", GWAv1$species)
# GWAv1$species = gsub(" subsp ", " ", GWAv1$species)
# GWAv1$species = gsub(" [Xx] ", " ", GWAv1$species)
# GWAv1$species = gsub(" var ", " ", GWAv1$species)
# GWAv1$species = gsub(" \\s+ ", "\\s", GWAv1$species)
# GWAv1$species = gsub(" [Xx]", "", GWAv1$species)
# unique(GWAv1$species)
# right, now get started on the data
unique(GWAv1$trait_name)

#There are 25 traits

# 1. Scientific Name
#Take out this category
GWAv1 = GWAv1[!GWAv1$trait_name == "Scientific Name",]

# 2. Family
GWAv1 = GWAv1 %>% mutate(trait_name = ifelse(trait_name == "Family", "family", trait_name))
# 3. Common Name
GWAv1 = GWAv1 %>% mutate(trait_name = ifelse(trait_name == "Common Name", "common_name", trait_name))
# 4. Other Common Names
OCN = GWAv1[GWAv1$trait_name == "Other Common Names",]
OCN = separate(OCN, col = value, into = c("common_name", "common_name1", "common_name2", "common_name3", "common_name4", "common_name5"), sep = ", ")
OCN = OCN %>% gather(key = "trait_name", value = "value", common_name:common_name5, na.rm = T)
OCN$trait_name = "common_name"
GWAv2 = rbind(GWAv1[!GWAv1$trait_name == "Other Common Names",], OCN)
# 5. Plant Type
PT = GWAv2[GWAv2$trait_name == "Plant Type",]
PT = separate(PT, col = value, into = c("form", "form1", "form2", "form3"), sep = ", ")
PT = PT %>% gather(key = "trait_name", value = "value", form:form3, na.rm = T)
PT$value = str_to_lower(PT$value)

unique(PT$value)

groundcover = PT[PT$value == "ground cover",] 
groundcover$value = "groundcover"
groundcover$trait_name = "purpose"

clumping = PT[PT$value == "clumping perennial",]
clumping$trait_name = "habit"
clumping$value = "clumping"

PT[PT$value == "clumping perennial",]$value = "perennial"
PT[PT$value == "bulb or bulb-like",]$value = "bulb"
PT[PT$value == "grass or grass-like",]$value = "grass"
PT$value = gsub(" ", "", PT$value)
PT$trait_name = "form"
GWAv2 = rbind(GWAv2[!GWAv2$trait_name == "Plant Type",], PT[!PT$value == "groundcover",], groundcover, clumping)
# 6. Height
height = GWAv2[GWAv2$trait_name == "Height",]
height$value = gsub("308", "3~8", height$value) 
height$value = gsub("[[:alpha:]]", "", height$value)

heightsplit = height[grepl("~", height$value)== T,]
heightsplit = separate(heightsplit, col = value, into = c("min_height", "max_height"), sep = "~")
heightsplit = heightsplit %>% gather(key = "trait_name", value = "value", min_height:max_height, na.rm = T)


height = height[grepl("~", height$value)== F,]
height = height[!height$value == "",]
height$trait_name = "height"

GWAv2 = rbind(GWAv2[!GWAv2$trait_name == "Height",], height, heightsplit)
# 7. Width
width = GWAv2[GWAv2$trait_name == "Width",]
width$value = gsub("[[:alpha:]]", "", width$value)

widthsplit = width[grepl("~", width$value)== T,]
widthsplit = separate(widthsplit, col = value, into = c("min_width", "max_width"), sep = "~")
widthsplit = widthsplit %>% gather(key = "trait_name", value = "value", min_width:max_width, na.rm = T)


width = width[grepl("~", width$value)== F,]
width = width[!width$value == "",]
width$trait_name = "width"

GWAv2 = rbind(GWAv2[!GWAv2$trait_name == "Width",], width, widthsplit)
# 8. Flower Colour
flowerc = GWAv2[GWAv2$trait_name == "Flower Colour",]
flowerc$value = str_to_lower(flowerc$value)
flowerc = separate(flowerc, col = value, into = c("fcolour", "fcolour1","fcolour2","fcolour3", "fcolour4", "fcolour5","fcolour6", "fcolour7"), sep = ", ")
flowerc = flowerc %>% gather(key = "trait_name", value = "value", fcolour:fcolour7, na.rm = T)
flowerc$value = gsub("gold", "golden", flowerc$value)

flowerc$trait_name = "flower_colour"

GWAv2 = rbind(GWAv2[!GWAv2$trait_name == "Flower Colour",], flowerc)
# 9. Flowering Time
season = GWAv2[GWAv2$trait_name == "Flowering Time",]

season = separate(season, col = value, into = c("season", "season1","season2","season3"), sep = ", ")
season = season %>% gather(key = "trait_name", value = "value", season:season3, na.rm = T)
season[season$value == "All year",]$value = "Any_time"
season$trait_name = "flower_period"

GWAv2 = rbind(GWAv2[!GWAv2$trait_name == "Flowering Time",], season)
# 10. Ph Level
Ph = GWAv2[GWAv2$trait_name == "Ph Level",]
Ph$value = str_to_lower(Ph$value)
Ph = separate(Ph, col = value, into = c("Ph", "Ph1","Ph2", "Ph3"), sep = ", ")
Ph = Ph %>% gather(key = "trait_name", value = "value", Ph:Ph3, na.rm = T)
Ph[Ph$value == "mildly acid",]$value = "slight_acid"
Ph[Ph$value == "strongly acidic",]$value = "acid"
Ph[Ph$value == "mildly alkaline",]$value = "slight_alkaline"
Ph$trait_name = "soil_pH"
unique(Ph$value)
GWAv2 = rbind(GWAv2[!GWAv2$trait_name == "Ph Level",], Ph)
# 11. Soil Type
Soil = GWAv2[GWAv2$trait_name == "Soil Type",]
Soil$value = str_to_lower(Soil$value)
Soil = separate(Soil, col = value, into = c("Soil", "Soil1","Soil2", "Soil3", "Soil4", "Soil5", "Soil6", "Soil8", "Soil9", "Soil10"), sep = ", ")
Soil = Soil %>% gather(key = "trait_name", value = "value", Soil:Soil10, na.rm = T)
unique(Soil$value)

saline = Soil[Soil$value == "saline",]
saline$trait_name = "salinity_tolerance"
saline$value = "Yes"

lowfertility = Soil[Soil$value == "poor soil",]
lowfertility$trait_name = "soil_fertility"
lowfertility$value = "low"

Soil = Soil[!(Soil$value == "poor soil"|Soil$value == "saline"|Soil$value == "potting mix"),]
Soil$value = gsub(" ", "_", Soil$value)
Soil$trait_name = "soil_type"

Soil$value = gsub("sandy", "sand", Soil$value)
Soil$value = gsub("loamy", "loam", Soil$value)
GWAv2 = rbind(GWAv2[!GWAv2$trait_name == "Soil Type",], Soil, lowfertility, saline)
# 12. Plant Environment
Plantenv = GWAv2[GWAv2$trait_name == "Plant Environment",]
Plantenv$value = str_to_lower(Plantenv$value)
Plantenv = separate(Plantenv, col = value, into = c("Soil", "Soil1","Soil2", "Soil3", "Soil4", "Soil5", "Soil6", "Soil7", "Soil8"), sep = ", ")
Plantenv = Plantenv %>% gather(key = "trait_name", value = "value", Soil:Soil8, na.rm = T)
unique(Plantenv$value)

Plantenv$value = gsub("low maintenance garden", "low", Plantenv$value)
Plantenv[Plantenv$value == "low",]$trait_name = "maintenance_level"

Plantenv$value = gsub("courtyard", "balcony", Plantenv$value)
Plantenv[Plantenv$value == "balcony",]$trait_name = "placement"

Plantenv$value = gsub("container growing", "potplant", Plantenv$value)
Plantenv[Plantenv$value == "potplant",]$trait_name = "placement"

Plantenv[Plantenv$value == "indoor",]$trait_name = "placement"

Plantenv$value = gsub("drought resistant", "Yes", Plantenv$value)
Plantenv[Plantenv$value == "Yes",]$trait_name = "drought_tolerance"

Plantenv$value = gsub("coastal garden", "Yes1", Plantenv$value)
Plantenv[Plantenv$value == "Yes1",]$trait_name = "coastal_tolerance"
Plantenv$value = gsub("Yes1", "Yes", Plantenv$value)

Plantenv$value = gsub("cold climate", "cool", Plantenv$value)
Plantenv[Plantenv$value == "cool",]$trait_name = "ideal_conditions"

Plantenv = Plantenv[!Plantenv$value == "poolside",]
Plantenv = Plantenv[!Plantenv$value == "cottage garden",]

Plantenv$value = gsub("flower garden", "showey_flower", Plantenv$value)
Plantenv[Plantenv$value == "showey_flower",]$trait_name = "other_feature"
unique(Plantenv$trait_name)
GWAv2 = rbind(GWAv2[!GWAv2$trait_name == "Plant Environment",], Plantenv)
# 13. Climate Zone: leave it with description
# 14. Light
light = GWAv2[GWAv2$trait_name == "Light",]

light$value = gsub("Sunny", "fullsun", light$value)
light$value = gsub("Light shade", "lightshade", light$value)
light$value = gsub("Half shade", "partshade", light$value)
light$value = gsub("Heavy shade", "fullshade", light$value)

light = separate(light, col = value, into = c("Soil", "Soil1","Soil2", "Soil3"), sep = ", ")
light = light %>% gather(key = "trait_name", value = "value", Soil:Soil3, na.rm = T)
light$trait_name = "light_level"
GWAv2 = rbind(GWAv2[!GWAv2$trait_name == "Light",], light)
# 15. Planting Season
GWAv2 = GWAv2[!GWAv2$trait_name == "Planting Season",]
# 16. Growth Habit
habit = GWAv2[GWAv2$trait_name == "Growth Habit",]

habit$value = str_to_lower(habit$value)
habit = separate(habit, col = value, into = c("Soil", "Soil1","Soil2", "Soil3", "Soil4", "Soil5", "Soil6", "Soil7", "Soil8"), sep = ", ")
habit = habit %>% gather(key = "trait_name", value = "value", Soil:Soil8, na.rm = T)
unique(habit$value)

habit[habit$value == "evergreen",]$trait_name = "leaf_loss"
habit[habit$value == "deciduous",]$trait_name = "leaf_loss"
habit[habit$value == "spreading",]$trait_name = "habit"
habit$value = gsub("open foliage", "open", habit$value)
habit[habit$value == "open",]$trait_name = "habit"
habit$value = gsub("dense foliage", "dense", habit$value)
habit[habit$value == "dense",]$trait_name = "habit"
habit[habit$value == "weeping",]$trait_name = "habit"
habit$value = gsub("mound-shaped", "rounded", habit$value)
habit[habit$value == "rounded",]$trait_name = "habit"
habit$value = gsub("variegated", "variegations", habit$value)
habit[habit$value == "variegations",]$trait_name = "foliage_colour"
habit$value = gsub("column-shaped", "columnar", habit$value)
habit[habit$value == "columnar",]$trait_name = "canopy_shape"

GWAv2 = rbind(GWAv2[!GWAv2$trait_name == "Growth Habit",], habit)
# 17. Lifespan
Lifespan = GWAv2[GWAv2$trait_name == "Lifespan",]
Lifespan = separate(Lifespan, col = value, into = c("Soil", "Soil1","Soil2", "Soil3"), sep = ", ")
Lifespan = Lifespan %>% gather(key = "trait_name", value = "value", Soil:Soil3, na.rm = T)
Lifespan$value = str_to_lower(Lifespan$value)
unique(Lifespan$value)
Lifespan = Lifespan[!Lifespan$value == "perennial",]
Lifespan$value = gsub("short lived", "short (1 - 20 years)", Lifespan$value)
Lifespan$value = gsub("long lived", "long (> 50 years)", Lifespan$value)
Lifespan$trait_name = "longevity"

GWAv2 = rbind(GWAv2[!GWAv2$trait_name == "Lifespan",], Lifespan)
# 18. Soil Moisture
Soilm = GWAv2[GWAv2$trait_name == "Soil Moisture",]
Soilm = separate(Soilm, col = value, into = c("Soil", "Soil1","Soil2", "Soil3"), sep = ", ")
Soilm = Soilm %>% gather(key = "trait_name", value = "value", Soil:Soil3, na.rm = T)
Soilm$value = str_to_lower(Soilm$value)
unique(Soilm$value)

Soilm$value = gsub("well-drained", "welldrained", Soilm$value)
Soilm[Soilm$value == "welldrained",]$trait_name = "soil_character"

Soilm$value = gsub("dry", "low", Soilm$value)
Soilm[Soilm$value == "low",]$trait_name = "supp_watering"

Soilm$value = gsub("moist moderate drainage", "medium", Soilm$value)
Soilm[Soilm$value == "medium",]$trait_name = "supp_watering"

Soilm$value = gsub("boggy poorly drained", "poorly_drained", Soilm$value)
Soilm[Soilm$value == "poorly_drained",]$trait_name = "ideal_conditions"

wet = Soilm[Soilm$value == "poorly_drained",]
wet$trait_name = "placement"
wet$value = "wet"

wltolerance =Soilm[Soilm$value == "poorly_drained",]
wltolerance$trait_name = "waterlogging_tolerance"
wltolerance$value = "Yes"

Soilm$value = gsub("needs regular watering", "high", Soilm$value)
Soilm[Soilm$value == "high",]$trait_name = "supp_watering"

GWAv2 = rbind(GWAv2[!GWAv2$trait_name == "Soil Moisture",], Soilm, wet, wltolerance)
#19. Frost Tolerance
frost = GWAv2[GWAv2$trait_name == "Frost Tolerance",]
frost = separate(frost, col = value, into = c("Soil", "Soil1"), sep = ", ")
frost = frost %>% gather(key = "trait_name", value = "value", Soil:Soil1, na.rm = T)
frost$value = str_to_lower(frost$value)
unique(frost$value)
frostl = frost[frost$value == "tolerates light frost",]
frostl$value = "Yes"
frostl$trait_name = "lightfrost_tolerance"

frost = frost[frost$value == "tolerates heavy frost"|frost$value == "not frost tolerant",]
frost[frost$value == "tolerates heavy frost",]$value = "Yes"
frost[frost$value == "not frost tolerant",]$value = "No"
frost$trait_name = "frost_tolerance"


GWAv2 = rbind(GWAv2[!GWAv2$trait_name == "Frost Tolerance",], frost, frostl)
# 20. Plant Usage
PU = GWAv2[GWAv2$trait_name == "Plant Usage",]
PU = separate(PU, col = value, into = c("Soil", "Soil1", "Soil2", "Soil3", "Soil4","Soil5","Soil6","Soil7"), sep = ", ")
PU = PU %>% gather(key = "trait_name", value = "value", Soil:Soil7, na.rm = T)
PU$value = str_to_lower(PU$value)
unique(PU$value)

PU$value = gsub("feature plant", "feature", PU$value)
PU[PU$value == "feature",]$trait_name = "purpose"

PU[PU$value == "screen",]$trait_name = "purpose"

PU[PU$value == "groundcover",]$trait_name = "purpose"

PU[PU$value == "windbreak",]$trait_name = "purpose"

PU$value = gsub("border plant", "border", PU$value)
PU[PU$value == "border",]$trait_name = "purpose"

PU$value = gsub("attractive foliage", "foliage", PU$value)
PU[PU$value == "foliage",]$trait_name = "other_feature"

PU$value = gsub("fragrant", "fragrance", PU$value)
PU[PU$value == "fragrance",]$trait_name = "other_feature"

PU$value = gsub("fire retardant", "fire_retardant", PU$value)
PU[PU$value == "fire_retardant",]$trait_name = "purpose"

PU$value = gsub("autumn foliage", "seasonalleaves", PU$value)
PU[PU$value == "seasonalleaves",]$trait_name = "other_feature"

PU$value = gsub("hedge", "hedging_possible", PU$value)
PU[PU$value == "hedging_possible",]$trait_name = "habit"


PU = PU[!PU$value == "wow factor",]
PU = PU[!PU$value == "lawn alternative",]
PU = PU[!PU$value == "topiary",]

GWAv2 = rbind(GWAv2[!GWAv2$trait_name == "Plant Usage",], PU)
# 21. Special Uses
SU = GWAv2[GWAv2$trait_name == "Special Uses",]

SU = separate(SU, col = value, into = c("Soil", "Soil1", "Soil2", "Soil3", "Soil4","Soil5","Soil6","Soil7"), sep = ", ")
SU = SU %>% gather(key = "trait_name", value = "value", Soil:Soil7, na.rm = T)
SU$value = str_to_lower(SU$value)
unique(SU$value)
SU = SU[!SU$value == "bonsai",]
SU = SU[!SU$value == "",]

SU$value = gsub("bird nesting plant", "bird", SU$value)
SU[SU$value == "bird",]$trait_name = "ecological_services"


SU[SU$value == "edible",]$trait_name = "purpose"

SU$value = gsub("erosion control", "erosion", SU$value)
SU[SU$value == "erosion",]$trait_name = "purpose"

SU$value = gsub("cut flower", "cutflowers", SU$value)
SU[SU$value == "cutflowers",]$trait_name = "purpose"

SU$value = gsub("street tree", "street", SU$value)
SU[SU$value == "street",]$trait_name = "placement"

SU$value = gsub("bog gardens", "wet", SU$value)
SU[SU$value == "wet",]$trait_name = "placement"

SU$value = gsub("decorative fruit", "showey_fruit", SU$value)
SU[SU$value == "showey_fruit",]$trait_name = "other_feature"

SU$value = gsub("fast growing", "fast", SU$value)
SU[SU$value == "fast",]$trait_name = "growth_rate"

SU$value = gsub("honey producing plant", "bee", SU$value)
SU[SU$value == "bee",]$trait_name = "ecological_services"

SU$value = gsub("pollution tolerant", "Yes", SU$value)
SU[SU$value == "Yes",]$trait_name = "pollution_tolerance"

SU[SU$value == "tea",]$trait_name = "purpose"

SU$value = gsub("fragrant oils", "fragrant_foliage", SU$value)
SU[SU$value == "fragrant_foliage",]$trait_name = "other_feature"

SU$value = gsub("playground friendly", "playgroundfriendly", SU$value)
SU[SU$value == "playgroundfriendly",]$trait_name = "purpose"

GWAv2 = rbind(GWAv2[!GWAv2$trait_name == "Special Uses",], SU)
# 22. Attracts wildlife
ecol = GWAv2[GWAv2$trait_name == "Attracts Wildlife",]
ecol = separate(ecol, col = value, into = c("Soil", "Soil1", "Soil2", "Soil3", "Soil4","Soil5","Soil6","Soil7"), sep = ", ")
ecol = ecol %>% gather(key = "trait_name", value = "value", Soil:Soil7, na.rm = T)
ecol$value = str_to_lower(ecol$value)
unique(ecol$value)

ecol$value = gsub("bees", "bee", ecol$value)
ecol[ecol$value == "bee",]$trait_name = "ecological_services"

ecol$value = gsub("seed eating birds", "bird", ecol$value)
ecol[ecol$value == "bird",]$trait_name = "ecological_services"

ecol$value = gsub("nectar eating birds", "bird", ecol$value)
ecol[ecol$value == "bird",]$trait_name = "ecological_services"

ecol$value = gsub("lizards", "lizard", ecol$value)
ecol[ecol$value == "lizard",]$trait_name = "ecological_services"

ecol$value = gsub("butterflies", "pollinator", ecol$value)
ecol[ecol$value == "pollinator",]$trait_name = "ecological_services"

ecol$value = gsub("other insects", "pollinator", ecol$value)
ecol[ecol$value == "pollinator",]$trait_name = "ecological_services"

ecol$value = gsub("mammals", "native_mammal", ecol$value)
ecol[ecol$value == "native_mammal",]$trait_name = "ecological_services"

GWAv2 = rbind(GWAv2[!GWAv2$trait_name == "Attracts Wildlife",], ecol)

unique(GWAv2$trait_name)
#get rid of duplicates.. hmmm this means I'm getting rid of all those entries that differ in cultivar name or common name but not in the botanical name entry.. 
#It halves the database
GWAv2 = unique(GWAv2)
# 23. description
write.csv(GWAv2, "./Scraped data/GardeningwithAngus/GWAv2.csv", row.names = F)

