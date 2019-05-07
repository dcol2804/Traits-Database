#Set the working directory to Benara and import BENv1.csv
BENv1 = read.csv("./Scraped data/Benara/Benaranurseriesraw.csv", stringsAsFactors = F)
#first clean up the species
# take out the tray type of plants
BENv1 = BENv1[-str_which(BENv1$species, " Tray"),]
# take out any that have numbers in them
BENv1 = BENv1[-str_which(BENv1$species, "[:digit:]"),]

# now start editing the remaining species
# x = BENv1$species
# # change all the capitalised files to lower case
# x[str_which(x, "^[:upper:]{2}")] = str_to_sentence(x[str_which(x, "^[:upper:]{2}")])
# # take out all the cross species
# x = gsub(" x ", " ", x)
# unique(x)
# # take out the var.
# unique(x[str_which(x, "var. ")])
# x = gsub("var. ", "", x)
# x = gsub("Var. ", "", x)
#take out the syn.
BENv1$species = gsub(" [Ss]yn.*", "", BENv1$species)

# unique(x[str_which(x, "syn. ")])
# x[x == "Plerandra syn. Aralia elegantissima"] = "Plerandra elegantissima"
# x[x == "Corpuscularia syn. Delosperma lehmannii"] = "Delosperma lehmannii"
# x[x == "Dypsis syn. Chrysaliodocarpus lutescens"] = "Dypsis lutescens"
# x[x == "Liriope muscari syn. Liriope giganteum"] = "Liriope muscari"
# # PBR
# unique(x[str_which(x, "PBR")])
# x = gsub(" PBR", "", x)
# 
# #pbr
# unique(x[str_which(x, "pbr")])
# x = gsub("(pbr)", "", x)
# #subsp.
# unique(x[str_which(x, "subsp.")])
# x = gsub(" subsp.", "", x)

#G.
# unique(x[str_which(x, "G. ")])
# x = gsub(" G. *", "", x)
# 
# # Acer p.
# unique(x[str_which(x, "Acer p.")])
# x = gsub("Acer p. Dissectum atropurpurea", "Acer palmatum dissectum atropurpurea", x)
# #cv.
# unique(x[str_which(x, "cv.")])
# x = gsub("cv.", "", x)
# # hybridx
# unique(x[str_which(x, "hybridx")])
# x = gsub("hybridx", "hybrid", x)
# # take out any punctuation
# x = gsub("[[:punct:]]", "", x)
# 
# x = gsub("Solanum lycopersicum L", "Solanum lycopersicum", x)
# x = gsub("Adenanthos cunningham prostra", "Adenanthos cunninghamii", x)
# unique(x)
# a = data.frame( study = "Benaranurseries", species = "Plerandra elegantissima", trait_name = "synonyms", value = "Aralia elegantissima", stringsAsFactors = F)
# b = data.frame( study = "Benaranurseries", species = "Corpuscularia lehmannii", trait_name = "synonyms", value = "Delosperma lehmannii", stringsAsFactors = F)
# c = data.frame( study = "Benaranurseries", species = "Dypsis lutescens", trait_name = "synonyms", value = "Chrysaliodocarpus lutescens", stringsAsFactors = F)
# d = data.frame( study = "Benaranurseries", species = "Liriope muscari", trait_name = "synonyms", value = "Liriope giganteum", stringsAsFactors = F)
# BENv1$species = x
# BENv1 = rbind(BENv1, a, b, c, d, stringsAsFactors = F)


# Now start on the traits
BENv1$trait_name_original = BENv1$trait_name
BENv1$value_original = BENv1$value
# 1. Common name
library(dplyr)
BENv2 = BENv1 %>% mutate(trait_name = ifelse(trait_name == "Common Name", "common_name", trait_name))

common_name = BENv2[BENv2$trait_name == "common_name",]
common_name = separate(common_name, col = value, into = c("common_name", "common_name1", "common_name2", "common_name3","common_name4", "common_name5"), sep = ", ")
common_name = common_name %>% gather(key = "trait_name", value = "value", common_name:common_name5, na.rm = T)
common_name$trait_name = "common_name"
BENv2 = rbind(BENv2[!BENv2$trait_name == "common_name",], common_name)
# 2. Plant type
# This is a confusing one becaus ethey jam a million things into this category
# Lets see what we have here

keywords = BENv2[BENv2$trait_name == "Plant Type",]

# make a list of all possible terms and decide on which trait in the database they belong to.
#construct mini dfs for each trait that you assign to each term
# stitch them together

evergreen = keywords %>% mutate(value = ifelse(grepl("Evergreen", keywords$value) == T, "evergreen", NA))
evergreen$trait_name = "leaf_loss"
evergreen = evergreen[complete.cases(evergreen),]

hedging_possible = keywords %>% mutate(value = ifelse(grepl("Hedge", keywords$value) == T, "hedging_possible", NA))
hedging_possible$trait_name = "habit"
hedging_possible = hedging_possible[complete.cases(hedging_possible),]

shrub = keywords %>% mutate(value = ifelse(grepl("Shrub", keywords$value) == T, "shrub", NA))
shrub$trait_name = "form"
shrub = shrub[complete.cases(shrub),]

country_of_origin = keywords %>% mutate(value = ifelse(grepl("Native Australian Plant", keywords$value) == T, "Australia", NA))
country_of_origin$trait_name = "country_of_origin"
country_of_origin = country_of_origin[complete.cases(country_of_origin),]

tree = keywords %>% mutate(value = ifelse(grepl("Tree", keywords$value) == T, "tree", NA))
tree$trait_name = "form"
tree = tree[complete.cases(tree),]

coastal = keywords %>% mutate(value = ifelse(grepl("Coastal", keywords$value) == T, "Yes", NA))
coastal$trait_name = "coastal_tolerance"
coastal = coastal[complete.cases(coastal),]

groundcover = keywords %>% mutate(value = ifelse(grepl("Ground Cover", keywords$value) == T, "groundcover", NA))
groundcover$trait_name = "purpose"
groundcover = groundcover[complete.cases(groundcover),]

fern = keywords %>% mutate(value = ifelse(grepl("Fern", keywords$value) == T, "fern", NA))
fern$trait_name = "form"
fern = fern[complete.cases(fern),]

indoor = keywords %>% mutate(value = ifelse(grepl("Indoor", keywords$value) == T, "indoor", NA))
indoor$trait_name = "placement"
indoor = indoor[complete.cases(indoor),]

foliage = keywords %>% mutate(value = ifelse(grepl("Foliage", keywords$value) == T, "foliage", NA))
foliage$trait_name = "other_feature"
foliage = foliage[complete.cases(foliage),]

strappy = keywords %>% mutate(value = ifelse(grepl("Strappy Leaf", keywords$value) == T, "strap-leaved", NA))
strappy$trait_name = "form"
strappy = strappy[complete.cases(strappy),]

verge = keywords %>% mutate(value = ifelse(grepl("Verge", keywords$value) == T, "street", NA))
verge$trait_name = "placement"
verge = verge[complete.cases(verge),]

succulent = keywords %>% mutate(value = ifelse(grepl("Succulent", keywords$value) == T, "succulent", NA))
succulent$trait_name = "form"
succulent = succulent[complete.cases(succulent),]

annual = keywords %>% mutate(value = ifelse(grepl("Annual", keywords$value) == T, "annual", NA))
annual$trait_name = "form"
annual = annual[complete.cases(annual),]

fruit = keywords %>% mutate(value = ifelse(grepl("Fruiting", keywords$value) == T, "edible", NA))
fruit$trait_name = "purpose"
fruit = fruit[complete.cases(fruit),]

fruit1 = keywords %>% mutate(value = ifelse(grepl("Fruiting", keywords$value) == T, "showey_fruit", NA))
fruit1$trait_name = "other_feature"
fruit1 = fruit1[complete.cases(fruit1),]

grass = keywords %>% mutate(value = ifelse(grepl("Grasses", keywords$value) == T, "grass", NA))
grass$trait_name = "form"
grass = grass[complete.cases(grass),]

climber = keywords %>% mutate(value = ifelse(grepl("Climber", keywords$value) == T, "grass", NA))
climber$trait_name = "form"
climber = climber[complete.cases(climber),]

herb = keywords %>% mutate(value = ifelse(grepl("Herb", keywords$value) == T, "edible", NA))
herb$trait_name = "purpose"
herb = herb[complete.cases(herb),]

decid = keywords %>% mutate(value = ifelse(grepl("Deciduous", keywords$value) == T, "deciduous", NA))
decid$trait_name = "leaf_loss"
decid = decid[complete.cases(decid),]

vege = keywords %>% mutate(value = ifelse(grepl("Vegetable", keywords$value) == T, "vegetable", NA))
vege$trait_name = "form"
vege = vege[complete.cases(vege),]

newkeywords = rbind(evergreen, decid, climber, grass, fern, indoor, groundcover, strappy,shrub,tree,succulent,verge,vege,herb,hedging_possible,fruit,fruit1,foliage,country_of_origin,coastal)
BENv2 = rbind(BENv2[!BENv2$trait_name == "Plant Type",], newkeywords)

# 3. Dimensions

size = BENv2[BENv2$trait_name == "Dimensions",]

# Ok so you need to seperate things without an x and things that have "cm" in them. 
# And I would separate the cm thing after splitting columns into height and width. 

Heightonly = size[grepl("x", size$value) == F,]

Heightonlycm = Heightonly[grepl("cm", Heightonly$value) == T,]
#Take out all the letters
Heightonlycm$value = gsub("[[:alpha:]]", "", Heightonlycm$value)
#Take out all the ranges of cm
Heightonlycmsplit = Heightonlycm[grepl("-", Heightonlycm$value) == T,]

#Now split and turn into max and min heights
library(tidyr)
Heightonlycmsplit = separate(Heightonlycmsplit, col = value, into = c("min_height", "max_height"), sep = "-")
Heightonlycmsplit = Heightonlycmsplit %>% gather(key = "trait_name", value = "value", min_height:max_height, na.rm = T)
# Now convert to m
Heightonlycmsplit$value = as.numeric(Heightonlycmsplit$value)/100
#Well done!
#Now heightonlycm
Heightonlycm = Heightonlycm[!grepl("-", Heightonlycm$value) == T,]
Heightonlycm$value = gsub("[[:punct:]]", "", Heightonlycm$value)
Heightonlycm$value = as.numeric(Heightonlycm$value)/100
Heightonlycm$trait_name = "height"

#now for m only
Heightonlym = Heightonly[!grepl("cm", Heightonly$value) == T,]
# most of them are vegetables. Take out any that start with Maturity or Vine, There are just a couple of ranges that fit this description-  or &
Heightonlym = Heightonlym[!grepl("Matur", Heightonlym$value) == T,]
Heightonlym = Heightonlym[!grepl("Vine", Heightonlym$value) == T,]

Heightonlym = Heightonlym[!grepl("-", Heightonlym$value) == T,]
Heightonlym = Heightonlym[!grepl("&", Heightonlym$value) == T,]
Heightonlym = Heightonlym[!grepl("wide", Heightonlym$value) == T,]

Heightonlym$value = gsub("[[:alpha:]]", "", Heightonlym$value)
Heightonlym$trait_name = "height"

# Now go back to all the ones that contain an x and split them. There will be cm and ranges for both height and width

sizesplit = size[grepl("x", size$value) == T,]
sizesplit = separate(sizesplit, col = value, into = c("height", "width"), sep = "x")

# height cm min max
Heightcm = sizesplit[grepl("cm", sizesplit$height) == T,c(1:4, 6,7)]
Heightcmsplit = Heightcm[grepl("-", Heightcm$height) == T,]
Heightcmsplit = separate(Heightcmsplit, col = height, into = c("min_height", "max_height"), sep = "-")
Heightcmsplit = Heightcmsplit %>% gather(key = "trait_name", value = "value", min_height:max_height, na.rm = T)
# this line is just to get rid of the pesky 2 cells that were in m
Heightcmsplit$value[25:26] = as.numeric(gsub("[[:alpha:]]", "", Heightcmsplit$value[25:26]))*100

Heightcmsplit$value = as.numeric(gsub("[[:alpha:]]", "", Heightcmsplit$value))/100
#Done with the cmheightsplits now for the cm height without "-"
Heightcm = Heightcm[!grepl("-", Heightcm$height) == T,]
Heightcm$height = as.numeric(gsub("[[:alpha:]]", "", Heightcm$height))/100
Heightcm$trait_name = "height"
names(Heightcm) = c("study", "species", "trait_name", "value", "trait_name_original","value_original")
#Now for heights in m
Heightm = sizesplit[!grepl("cm", sizesplit$height) == T,c(1:4, 6,7)]
#convert any "to" to "-"
Heightm$height = gsub("to", "-", Heightm$height)
#take out all the letters
Heightm$height = gsub("[[:alpha:]]", "", Heightm$height)
#mins and maxes
Heightmsplit = Heightm[grepl("-", Heightm$height) == T,]
Heightmsplit = separate(Heightmsplit, col = height, into = c("min_height", "max_height"), sep = "-")
Heightmsplit = Heightmsplit %>% gather(key = "trait_name", value = "value", min_height:max_height, na.rm = T)
names(Heightmsplit) = c("study", "species", "trait_name", "value", "trait_name_original","value_original")
# now just Heightm
Heightm = Heightm[!grepl("-", Heightm$height) == T,]
names(Heightm) = c("study", "species", "trait_name", "value", "trait_name_original","value_original")
Heightm$trait_name = "height"

#####################
# width cm min max
widthcm = sizesplit[grepl("cm", sizesplit$width) == T,c(1:3, 5:7)]
widthcmsplit = widthcm[grepl("-", widthcm$width) == T,]
widthcmsplit = separate(widthcmsplit, col = width, into = c("min_width", "max_width"), sep = "-")
widthcmsplit = widthcmsplit %>% gather(key = "trait_name", value = "value", min_width:max_width, na.rm = T)
# this line is just to get rid of the pesky 1 cell that was in m
widthcmsplit$value[20] = as.numeric(gsub("[[:alpha:]]", "", widthcmsplit$value[20]))*100

widthcmsplit$value = as.numeric(gsub("[[:alpha:]]", "", widthcmsplit$value))/100
#Done with the cmwidthsplits now for the cm width without "-"
widthcm = widthcm[!grepl("-", widthcm$width) == T,]
widthcm$width = as.numeric(gsub("[[:alpha:]]", "", widthcm$width))/100
widthcm$trait_name = "width"
names(widthcm) = c("study", "species", "trait_name", "value", "trait_name_original","value_original")
#Now for widths in m
widthm = sizesplit[!grepl("cm", sizesplit$width) == T,c(1:3, 5:7)]
#convert any "to" to "-"
widthm$width = gsub("to", "-", widthm$width)
#take out all the letters
widthm$width = gsub("[[:alpha:]]", "", widthm$width)
#mins and maxes
widthmsplit = widthm[grepl("-", widthm$width) == T,]
widthmsplit = separate(widthmsplit, col = width, into = c("min_width", "max_width"), sep = "-")
widthmsplit = widthmsplit %>% gather(key = "trait_name", value = "value", min_width:max_width, na.rm = T)
names(widthmsplit) = c("study", "species", "trait_name", "value", "trait_name_original","value_original")
# now just widthm
widthm = widthm[!grepl("-", widthm$width) == T,]
names(widthm) = c("study", "species", "trait_name", "value", "trait_name_original","value_original")
widthm$trait_name = "width"

#now add alltogether and paste into the BENv2

newsize = rbind(Heightonlym,Heightonlycm,Heightonlycmsplit, Heightm, Heightcm, Heightmsplit, Heightcmsplit, widthm,widthmsplit,widthcm,widthcmsplit)
BENv2 = rbind(BENv2[!BENv2$trait_name == "Dimensions",], newsize)

#4. Sunlight

light = BENv2[BENv2$trait_name == "Sunlight",]


light$value = gsub("Full Sun", "fullsun", light$value)
light$value = gsub("Part Sun", "partshade", light$value)
light$value = gsub("Shade", "fullshade", light$value)

light = separate(light, col = value, into = c("light", "light1","light2"), sep = ", ")
light = light %>% gather(key = "trait_name", value = "value", light:light2, na.rm = T)
light$trait_name = "light_level"

BENv2 = rbind(BENv2[!BENv2$trait_name == "Sunlight",], light)
#5. skipping to flowering season because its easy
season = BENv2[BENv2$trait_name == "Flowering Season",]

season = separate(season, col = value, into = c("season", "season1","season2","season3"), sep = ", ")
season = season %>% gather(key = "trait_name", value = "value", season:season3, na.rm = T)
season$trait_name = "flower_period"

BENv2 = rbind(BENv2[!BENv2$trait_name == "Flowering Season",], season)
#6. Flower colour
fcolour = BENv2[BENv2$trait_name == "Flower Colour",]
fcolour = separate(fcolour, col = value, into = c("fcolour", "fcolour1","fcolour2","fcolour3", "fcolour4", "fcolour5","fcolour6", "fcolour7"), sep = ", ")
fcolour = fcolour %>% gather(key = "trait_name", value = "value", fcolour:fcolour7, na.rm = T)
unique(fcolour$value)
library(stringr)
# lowercase
fcolour$value = str_to_lower(fcolour$value)
# gold = golden
fcolour$value = gsub("gold", "golden", fcolour$value)
# Mixed = Assorted
fcolour$value = gsub("mixed", "assorted", fcolour$value)
# gsub "Pale "
fcolour$value = gsub("pale ", "", fcolour$value)
fcolour$trait_name = "flower_colour"

BENv2 = rbind(BENv2[!BENv2$trait_name == "Flower Colour",], fcolour)

# 6. leaf colour

lcolour = BENv2[BENv2$trait_name == "Foliage Colour",]
lcolour$value = str_to_lower(lcolour$value)
lcolour = separate(lcolour, col = value, into = c("fcolour", "fcolour1","fcolour2","fcolour3", "fcolour4", "fcolour5","fcolour6", "fcolour7", "fcolour8"), sep = ", ")
lcolour = lcolour %>% gather(key = "trait_name", value = "value", fcolour:fcolour8, na.rm = T)

lcolour$value[lcolour$value == "blue"] = "bluegreen"
lcolour = lcolour[!lcolour$value == "black",]
lcolour = lcolour[!lcolour$value == "brown",]
lcolour$value[lcolour$value == "crimson"] = "red"
lcolour$value[lcolour$value == "grey"] = "greygreen"
lcolour$value[lcolour$value == "lime"] = "lightgreen"
lcolour = lcolour[!lcolour$value == "orange",]
lcolour$value[lcolour$value == "maroon"] = "burgundy"
lcolour$value[lcolour$value == "pink"] = "pinkred"
lcolour$value[lcolour$value == "silver"] = "silver_foliage"
lcolour$trait_name[lcolour$value == "silver_foliage"] = "other_feature"
lcolour = lcolour[!lcolour$value == "white",]
lcolour$value[lcolour$value == "copper"] = "bronze"
# Unfortunately, they include all the autumn colours under leaf colour
# What I could do is see if I can isolate the deciduous plants from the rest.


speciesautumncolours = BENv2[BENv2$value == "deciduous",]$species
speciesautumncolours = setdiff(speciesautumncolours, c("Breynia nivosa", "Cercis canadensis", "Gleditsia triacanthos", "Lagerstroemia indica fauriei", "Prunus cerasifera", "Prunus bilreana"))
autumncolours = lcolour[lcolour$species %in% speciesautumncolours,]
autumncolours = autumncolours[(autumncolours$value != "green"& autumncolours$value != "grey"&autumncolours$value != "silver"),]
#right, now we have the autumn colours list. Take these out of the original list.
lcolour = lcolour[!(lcolour$species %in% autumncolours$species & lcolour$value %in% autumncolours$value),]

lcolour$trait_name = "foliage_colour"
autumncolours$trait_name = "seasonal_colour"
autumncolours$value[autumncolours$value == "gold"] = "yellow"
autumncolours$value[autumncolours$value == "purple"] = "burgundy"
BENv2 = rbind(BENv2[!BENv2$trait_name == "Foliage Colour",], lcolour, autumncolours)
# Now clean up the other traits

BENv2 = BENv2[!BENv2$trait_name == "Botanical Genus",]
BENv2 = BENv2[!BENv2$trait_name == "Botanical Species",]
BENv2 = BENv2[!BENv2$trait_name == "Width",]
BENv2 = BENv2[!BENv2$trait_name == "Height",]
BENv2 = BENv2[!BENv2$trait_name == "Bird/Wildlife Attracting",]
BENv2 = BENv2[!BENv2$trait_name == "Planting Season",]
BENv2 = BENv2[!BENv2$trait_name == "Market Name",]

unique(BENv2$trait_name)

#Last 2 traits! Flower Type and Foliage Type

Flowert = BENv2[BENv2$trait_name == "Flower Type",]
unique(Flowert$value)
Flowert$value= sub("( -.*)", "", Flowert$value)
Flowert$value= sub("(/.*)", "", Flowert$value)
unique(Flowert$value)
Flowert$value= sub("Pea Shaped Flower", "pea-shaped", Flowert$value)
Flowert$value= sub("Cylindrical Bottlebrush", "Bottlebrush", Flowert$value)
Flowert$value= sub("Terminal Clusters", "Cluster", Flowert$value)
Flowert$value = str_to_lower(Flowert$value)
Flowert$trait_name = "flower_description"
Flowert$value = gsub("catkins", "catkin",Flowert$value)
Flowert$value = gsub("bracts", "bract",Flowert$value)
BENv2 = rbind(BENv2[!BENv2$trait_name == "Flower Type",], Flowert)
# Just foliage type to go.

Foliaget = BENv2[BENv2$trait_name == "Foliage Type",]
Foliaget$value= sub("( -.*)", "", Foliaget$value)
Foliaget$value = str_to_lower(Foliaget$value)
unique(Foliaget$value)
Foliaget$trait_name = "leaf_description"
Foliaget$value= sub("fern frond", "frond", Foliaget$value)
Foliaget$value= sub("rosette of thickened fleshy leaves", "rosette", Foliaget$value)
Foliaget[Foliaget$value == "rosette",]$trait_name = "habit"
Foliaget$value= sub("scale like", "scale_like", Foliaget$value)
Foliaget$value= sub("spear-shaped", "sword_shaped", Foliaget$value)
Foliaget = Foliaget[!Foliaget$value == "no",]
BENv2 = rbind(BENv2[!BENv2$trait_name == "Foliage Type",], Foliaget)

unique(BENv2$trait_name)
BENv2 = unique(BENv2)

write.csv(BENv2, "./Scraped data/Benara/BENv2.csv", row.names = F)
