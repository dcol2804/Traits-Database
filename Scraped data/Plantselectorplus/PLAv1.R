#set the working directory to Plantselectorplus and import the PLAv1.csv file
PLAv1 = read.csv("./Scraped data/Plantselectorplus/Plantselectorplusraw.csv", stringsAsFactors = F)
# clean up the species list
PLAv1$species = gsub(" [Ss]yn.*", "", PLAv1$species)

PLAv1$trait_name_original = PLAv1$trait_name
PLAv1$value_original = PLAv1$value
# PLAv1$species = gsub("[[:punct:]]", "", PLAv1$species)
# PLAv1$species = gsub("U[0-9]{4}", "", PLAv1$species)
# PLAv1$species = gsub(" ssp ", " ", PLAv1$species)
# PLAv1$species = gsub(" spp ", " ", PLAv1$species)
# PLAv1$species = gsub(" subspecies ", " ", PLAv1$species)
# PLAv1$species = gsub(" [Xx] ", " ", PLAv1$species)
# PLAv1$species = gsub(" var ", " ", PLAv1$species)
PLAv1$species = gsub("\\s+ ", " ", PLAv1$species)
unique(PLAv1$species)

unique(PLAv1$trait_name)
library(tidyverse)
# 1. Height
height = PLAv1[PLAv1$trait_name == "Height",]
height = height[grepl("-", height$value) == T,]

#wow all of them!
height = separate(height, col = value, into = c("min_height", "max_height"), sep = "-")
height = height %>% gather(key = "trait_name", value = "value", min_height:max_height, na.rm = T)

height$value = as.numeric(gsub("[[:alpha:]]", "", height$value))

PLAv1 = rbind(PLAv1[!PLAv1$trait_name == "Height",], height)
# 2. Spread
width = PLAv1[PLAv1$trait_name == "Spread",]
width = separate(width, col = value, into = c("min_width", "max_width"), sep = "-")
width = width %>% gather(key = "trait_name", value = "value", min_width:max_width, na.rm = T)

width$value = as.numeric(gsub("[[:alpha:]]", "", width$value))
PLAv1 = rbind(PLAv1[!PLAv1$trait_name == "Spread",], width)
# 3. Position
Pos = PLAv1[PLAv1$trait_name == "Position",]

Pos = separate(Pos, col = value, into = c("generic", "generic1", "generic2", "generic3"), sep = ", ")
Pos = Pos %>% gather(key = "trait_name", value = "value", generic:generic3, na.rm = T)
Pos$value = gsub("Full Sun", "fullsun", Pos$value) 
Pos$value = gsub("Part Shade", "partshade", Pos$value)
Pos$value = gsub("Full Shade", "fullshade", Pos$value)
Pos$trait_name = "light_level"
PLAv1 = rbind(PLAv1[!PLAv1$trait_name == "Position",], Pos)

# 4. Family
Fam = PLAv1[PLAv1$trait_name == "Family",]
Fam$trait_name = "family"
PLAv1 = rbind(PLAv1[!PLAv1$trait_name == "Family",], Fam)

# 5. Botanical Name - take this out
PLAv1 = PLAv1[!PLAv1$trait_name == "Botanical Name",]

# 6. Common Name
Cname = PLAv1[PLAv1$trait_name == "Common Name",]
Cname = separate(Cname, col = value, into = c("generic", "generic1", "generic2", "generic3"), sep = ",")
Cname = Cname %>% gather(key = "trait_name", value = "value", generic:generic3, na.rm = T)
Cname$value = str_trim(Cname$value)
Cname$trait_name = "common_name"
PLAv1 = rbind(PLAv1[!PLAv1$trait_name == "Common Name",], Cname)
# 7. Origin
OR = PLAv1[PLAv1$trait_name == "Origin",]
OR = separate(OR, col = value, into = c("generic", "generic1", "generic2", "generic3","generic4", "generic5", "generic6", "generic7"), sep = ",")
OR = OR %>% gather(key = "trait_name", value = "value", generic:generic7, na.rm = T)
OR$value = str_trim(OR$value)
unique(OR$value)

#Do this later when braindead
#SA, Vic, NSW, WA, Tas, Qld
# 8. Habit
Habit = PLAv1[PLAv1$trait_name == "Habit",]
Habit = separate(Habit, col = value, into = c("generic", "generic1", "generic2", "generic3","generic4", "generic5", "generic6", "generic7"), sep = ", ")
Habit = Habit %>% gather(key = "trait_name", value = "value", generic:generic7, na.rm = T)
Habit$value = str_trim(Habit$value)
Habit$value = str_to_lower(Habit$value)
unique(Habit$value)

#Far out, 243 different categories I have to go through
# 9. Landscape - probs take this out
LS = PLAv1[PLAv1$trait_name == "Landscape",]
LS = separate(LS, col = value, into = c("generic", "generic1", "generic2", "generic3","generic4", "generic5", "generic6", "generic7"), sep = ", ")
LS = LS %>% gather(key = "trait_name", value = "value", generic:generic7, na.rm = T)
LS$value = str_trim(LS$value)
unique(LS$value)
# 10. Soil Texture
ST = PLAv1[PLAv1$trait_name == "Soil Texture",]
ST = separate(ST, col = value, into = c("generic", "generic1", "generic2", "generic3","generic4", "generic5", "generic6", "generic7"), sep = ", ")
ST = ST %>% gather(key = "trait_name", value = "value", generic:generic7, na.rm = T)
ST$value = str_trim(ST$value)
ST$value = str_to_lower(ST$value)
ST = ST[!ST$value == "rock",]
ST$trait_name = "soil_type"
unique(ST$value)
PLAv1 = rbind(PLAv1[!PLAv1$trait_name == "Soil Texture",], ST)
# 11. pH
pH = PLAv1[PLAv1$trait_name == "pH",]
pH = separate(pH, col = value, into = c("generic", "generic1", "generic2", "generic3","generic4", "generic5", "generic6", "generic7"), sep = ", ")
pH = pH %>% gather(key = "trait_name", value = "value", generic:generic7, na.rm = T)
pH$value = str_trim(pH$value)
pH$value = str_to_lower(pH$value)
pH$trait_name = "soil_pH"
unique(pH$value)
pH$value = gsub("acidic", "acid", pH$value)
PLAv1 = rbind(PLAv1[!PLAv1$trait_name == "pH",], pH)
# 12. Tolerates
Tol = PLAv1[PLAv1$trait_name == "Tolerates",]
Tol = separate(Tol, col = value, into = c("generic", "generic1", "generic2", "generic3","generic4", "generic5", "generic6", "generic7"), sep = ", ")
Tol = Tol %>% gather(key = "trait_name", value = "value", generic:generic7, na.rm = T)
Tol$value = str_trim(Tol$value)
Tol$value = str_to_lower(Tol$value)
unique(Tol$value)
drought = Tol[Tol$value == "drought",]
drought$trait_name = "drought_tolerance"
drought$value = "Yes"
alkaline = Tol[Tol$value == "lime",]
alkaline$trait_name = "soil_pH"
alkaline$value = "alkaline"
compacteds = Tol[Tol$value == "compacted soil",]
compacteds$trait_name = "soilcompaction_tolerance"
compacteds$value = "Yes"
lfrost = Tol[Tol$value == "light frost",]
lfrost$trait_name = "lightfrost_tolerance"
lfrost$value = "Yes"
PLAv1 = PLAv1[!PLAv1$value == "fire",]
saline = Tol[Tol$value == "soil salinity",]
saline$trait_name = "salinity_tolerance"
saline$value = "Yes"
poll = Tol[Tol$value == "pollution",]
poll$trait_name = "pollution_tolerance"
poll$value = "Yes"
coast = Tol[Tol$value == "coast"| Tol$value == "salt spray",]
coast$trait_name = "coastal_tolerance"
coast$value = "Yes"
wind = Tol[Tol$value == "wind",]
wind$trait_name = "wind_tolerance"
wind$value = "Yes"
waterl = Tol[Tol$value == "flooding",]
waterl$trait_name = "waterlogging_tolerance"
waterl$value = "Yes"
extremewl = Tol[Tol$value == "water logging",]
extremewl$trait_name = "frequentwaterlogging"
extremewl$value = "Yes"
frost = Tol[Tol$value == "medium frost"| Tol$value == "heavy frost",]
frost$trait_name = "frost_tolerance"
frost$value = "Yes"
PLAv1 = rbind(PLAv1[!PLAv1$trait_name == "Tolerates",], drought, alkaline, compacteds, lfrost, saline, frost, poll, coast, wind, waterl, extremewl)
# 13. Supp watering
SP = PLAv1[PLAv1$trait_name == "Supplementary Watering",]
SP = separate(SP, col = value, into = c("generic", "generic1", "generic2", "generic3","generic4", "generic5", "generic6", "generic7"), sep = ", ")
SP = SP %>% gather(key = "trait_name", value = "value", generic:generic7, na.rm = T)
SP$value = str_trim(SP$value)
SP$value = str_to_lower(SP$value)
unique(SP$value)
length(which(SP$value == "none"))
SP$trait_name = "supp_watering"
SP$value = gsub("minimal", "low", SP$value)
SP$value = gsub("moderate", "medium", SP$value)
PLAv1 = rbind(PLAv1[!PLAv1$trait_name == "Supplementary Watering",], SP)
# 14. Flower Colour
FC = PLAv1[PLAv1$trait_name == "Flower Colour",]
FC = separate(FC, col = value, into = c("generic", "generic1", "generic2", "generic3","generic4", "generic5", "generic6", "generic7"), sep = ", ")
FC = FC %>% gather(key = "trait_name", value = "value", generic:generic7, na.rm = T)
FC$value = str_trim(FC$value)
FC$value = str_to_lower(FC$value)
unique(FC$value)
FC$value = gsub("gold", "golden", FC$value)
FC$value = gsub("insignificant", "inconspicuous", FC$value)
FC$value = gsub("none", "no", FC$value)
FC$trait_name = "flower_colour"
PLAv1 = rbind(PLAv1[!PLAv1$trait_name == "Flower Colour",], FC)
# 15. Flowering Time
FT = PLAv1[PLAv1$trait_name == "Flowering Time",]
FT = separate(FT, col = value, into = c("generic", "generic1", "generic2", "generic3","generic4", "generic5", "generic6", "generic7"), sep = ", ")
FT = FT %>% gather(key = "trait_name", value = "value", generic:generic7, na.rm = T)
FT$value = str_trim(FT$value)
unique(FT$value)
FT$trait_name = "flower_period"
PLAv1 = rbind(PLAv1[!PLAv1$trait_name == "Flowering Time",], FT)
# 16. Flower Type
FType = PLAv1[PLAv1$trait_name == "Flower Type",]
FType = separate(FType, col = value, into = c("generic", "generic1", "generic2", "generic3","generic4", "generic5", "generic6", "generic7"), sep = ", ")
FType = FType %>% gather(key = "trait_name", value = "value", generic:generic7, na.rm = T)
FType$value = str_trim(FType$value)
FType$value = str_to_lower(FType$value)
unique(FType$value)
FType$trait_name = "flower_description"
FType$value = gsub("es", "e", FType$value)
FType$value = gsub("eucalypt-type", "gum_blossom", FType$value)
FType$value = gsub("-", "_", FType$value)
FType$value = gsub("bell", "bell_shaped", FType$value)
FType$value = gsub("star", "starry", FType$value)
FType$value = gsub("clusters", "cluster", FType$value)
FType$value = gsub("pea", "pea-shaped", FType$value)
FType$value = gsub("funnel", "funnelform", FType$value)
FType$value = gsub("trumpet", "trumpet_shaped", FType$value)
FType$value = gsub("bracts", "bract", FType$value)
FType$value = gsub("grass panicle", "grass_panicle", FType$value)
FType$value = gsub("catkins", "catkin", FType$value)
FType$value = gsub("cylindrical spike", "spike", FType$value)
FType$value = gsub("terminal_spike", "spike", FType$value)
FType$value = gsub("spider/clustered", "spider-like", FType$value)
FType$value = gsub("cup", "cup_shaped", FType$value)
FType$value = gsub("corymbs", "corymb", FType$value)
FType$value = gsub("pendant", "pendulous", FType$value)
FType$value = gsub("umbels", "umble", FType$value)
unique(FType$value)
PLAv1 = rbind(PLAv1[!PLAv1$trait_name == "Flower Type",], FType)
# 17. Purpose
Pu = PLAv1[PLAv1$trait_name == "Purpose",]
Pu = separate(Pu, col = value, into = c("generic", "generic1", "generic2", "generic3","generic4", "generic5", "generic6", "generic7"), sep = ", ")
Pu = Pu %>% gather(key = "trait_name", value = "value", generic:generic7, na.rm = T)
Pu$value = str_trim(Pu$value)
Pu$value = str_to_lower(Pu$value)
unique(Pu$value)
Pu$trait_name = "purpose"
Pu$value = gsub("ornamental", "feature", Pu$value)
Pu$value = gsub("wind protection", "windbreak", Pu$value)
Pu$value = gsub("commemoration", "feature", Pu$value)
Pu$value = gsub("bush food", "edible", Pu$value)
Pu$value = gsub("food/fruit", "edible", Pu$value)
Pu$value = gsub("noise reduction", "screen", Pu$value)
Pu$value = gsub("hedge", "hedging_possible", Pu$value)
Pu[Pu$value == "hedging_possible",]$trait_name = "habit"
Pu = Pu[!Pu$value == "firewood",]
Pu = Pu[!Pu$value == "timber",]
Pu = Pu[!Pu$value == "winter sun",]
PLAv1 = rbind(PLAv1[!PLAv1$trait_name == "Purpose",], Pu)
# 18. Evergreen/dec
PLAv1 = PLAv1[!PLAv1$value == "N/A",]
Ev = PLAv1[PLAv1$trait_name == "Evergreen/Deciduous",]
Ev$value = str_to_lower(Ev$value)
unique(Ev$value)
Ev$trait_name = "leaf_loss" 
PLAv1 = rbind(PLAv1[!PLAv1$trait_name == "Evergreen/Deciduous",], Ev)
# 19. Form
Form = PLAv1[PLAv1$trait_name == "Form",]
Form = separate(Form, col = value, into = c("generic", "generic1", "generic2", "generic3","generic4", "generic5", "generic6", "generic7"), sep = ", ")
Form = Form %>% gather(key = "trait_name", value = "value", generic:generic7, na.rm = T)
Form$value = str_trim(Form$value)
Form$value = str_to_lower(Form$value)
Form$trait_name = "form"
unique(Form$value)
Form$value = gsub("grass sedge or flax", "grass", Form$value)
Form$value = gsub("medium shrub \\(usually between 1.2m & 3.6m\\)", "medshrub", Form$value)
Form$value = gsub("large shrub \\(usually exceeds 3.6m\\)", "largeshrub", Form$value)
Form$value = gsub("tall tree \\(usually exceeds 10m\\)", "largetree", Form$value)
Form[Form$value =="groundcover",]$trait_name = "purpose"
Form$value = gsub("small tree \\(up to 7m\\)", "smalltree", Form$value)
Form$value = gsub("medium tree \\(usually between 5m & 11m\\)", "medtree", Form$value)
Form$value = gsub("low shrub \\(up to 1.2m\\)", "smallshrub", Form$value)

PLAv1 = rbind(PLAv1[!PLAv1$trait_name == "Form",], Form)
# 20. description
# 21. Canopy Shape
Canopy = PLAv1[PLAv1$trait_name == "Canopy Shape",]
Canopy = separate(Canopy, col = value, into = c("generic", "generic1", "generic2", "generic3","generic4", "generic5", "generic6", "generic7"), sep = ", ")
Canopy = Canopy %>% gather(key = "trait_name", value = "value", generic:generic7, na.rm = T)
Canopy$value = str_trim(Canopy$value)
Canopy$value = str_to_lower(Canopy$value)
unique(Canopy$value)
Canopy$trait_name = "canopy_shape"
Canopy[Canopy$value == "pyramidal_d",]$value = "pyramidal"
Canopy[Canopy$value == "domed_d",]$value = "domed"
Canopy[Canopy$value == "round",]$value = "rounded"
Canopy[Canopy$value == "conical",]$value = "pyramidal"
Canopy[Canopy$value == "weeping",]$trait_name = "habit"
Canopy[Canopy$value == "palm",]$trait_name = "form"
Canopy[Canopy$value == "multitrunk",]$trait_name = "multistem_development"
Canopy[Canopy$value == "multitrunk",]$value = "Yes"
PLAv1 = rbind(PLAv1[!PLAv1$trait_name == "Canopy Shape",], Canopy)
# 22. Foliage
Foliage = PLAv1[PLAv1$trait_name == "Foliage",]
Foliage = separate(Foliage, col = value, into = c("generic", "generic1", "generic2", "generic3","generic4", "generic5", "generic6", "generic7"), sep = ", ")
Foliage = Foliage %>% gather(key = "trait_name", value = "value", generic:generic7, na.rm = T)
Foliage$value = str_trim(Foliage$value)
Foliage$value = str_to_lower(Foliage$value)
unique(Foliage$value)
Foliage$trait_name = "foliage_colour"
Foliage$value = gsub("-", "", Foliage$value)
Foliage$value = gsub("palegreen", "lightgreen", Foliage$value)
Foliage$value = gsub("bluishgreen", "bluegreen", Foliage$value)
Foliage$value = gsub("midgreen", "green", Foliage$value)
Foliage$value = gsub("brightgreen", "foliage", Foliage$value)
Foliage[Foliage$value == "foliage",]$trait_name = "other_feature"
Foliage$value = gsub("silvergrey", "silver_foliage", Foliage$value)
Foliage[Foliage$value == "silver_foliage",]$trait_name = "other_feature"
Foliage$value = gsub("silvergreen", "greygreen", Foliage$value)
Foliage[Foliage$value == "silverblue",]$value = "grey"
Foliage[Foliage$value == "olivegreen",]$value = "dullgreen"
Foliage[Foliage$value == "yellow green",]$value = "yellow"
Foliage[Foliage$value == "deepgreen",]$value = "darkgreen"
PLAv1 = rbind(PLAv1[!PLAv1$trait_name == "Foliage",], Foliage)
# 23. Seasonal Colour
Seasonal = PLAv1[PLAv1$trait_name == "Seasonal Colour",]
Seasonal = separate(Seasonal, col = value, into = c("generic", "generic1", "generic2", "generic3","generic4", "generic5", "generic6", "generic7"), sep = ", ")
Seasonal = Seasonal %>% gather(key = "trait_name", value = "value", generic:generic7, na.rm = T)
Seasonal$value = str_trim(Seasonal$value)
unique(Seasonal$value)
Seasonal$trait_name = "colour_season"
PLAv1 = rbind(PLAv1[!PLAv1$trait_name == "Seasonal Colour",], Seasonal)
# 24. Trunk
Trunk = PLAv1[PLAv1$trait_name == "Trunk",]
Trunk = separate(Trunk, col = value, into = c("generic", "generic1", "generic2", "generic3","generic4", "generic5", "generic6", "generic7"), sep = ", ")
Trunk = Trunk %>% gather(key = "trait_name", value = "value", generic:generic7, na.rm = T)
Trunk$value = str_trim(Trunk$value)
Trunk$value = str_to_lower(Trunk$value)
unique(Trunk$value)
Trunk$trait_name = "bark_texture"
Trunk[Trunk$value == "shedding",]$value = "peeling"
Trunk[Trunk$value == "thorn",]$value = "spiked"
Trunk[Trunk$value == "patterned",]$trait_name = "bark_appearance"
Trunk[Trunk$value == "flaky",]$value = "flakey"
PLAv1 = rbind(PLAv1[!PLAv1$trait_name == "Trunk",], Trunk)

#Finished!
PLAv1 = unique(PLAv1)
PLAv1 = PLAv1[complete.cases(PLAv1),]
unique(PLAv1$trait_name)

write.csv(PLAv1, "./Scraped data/Plantselectorplus/PLAv2.csv", row.names = F)
