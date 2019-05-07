# set the working directory and load the ACTv1.csv
ACTv1 = read.csv("./Scraped data/ACTplantselector/ACTplantselectorraw.csv", stringsAsFactors = F)

ACTv1$trait_name_original = ACTv1$trait_name
ACTv1$value_original = ACTv1$value
# First, clean up the species names

library(stringr)

# fixing up a particular name
#x = ACTv1$species
#x = gsub("Blechnum penna marina", "Blechnum penna-marina", x)

# Fixing up dwarf varieties

#xd = unique(x[str_which(x, "warf")])

# get rid of the cdwarf bit in the species column
#x[(x == "Banksia marginata dwarf form")] = "Banksia marginata dwarf"
#x[(x == "Banksia spinulosa Dwarf")] = "Banksia spinulosa dwarf"

#prostrate forms
#unique(x[(str_which(x, "prostrate"))])
#x[(x == "Grevillea lanigera prostrate forms")] = "Grevillea lanigera prostrate"
# var.
#unique(x[(str_which(x, "var"))])
#x = gsub("var.", "", x)
# spp. and also subspecies: ssp.
#unique(x[(str_which(x, "spp."))])
#x[(x == "Rosa spp.& hybrids.")] = "Rosa sp"
#x = gsub(" spp.", " sp", x)
#x = gsub(" spp", " sp", x)
#x = gsub(" ssp.", "", x)

#any containing syn or Syn, replace everything after the word syn with nothing
ACTv1$species = gsub(" [Ss]yn.*", "", ACTv1$species)

# unique(x[(str_which(x, "Syn"))])
# unique(x[(str_which(x, "syn"))])

# x[(x == "Babingtonia virgata Syn Baeckea virgata")] = "Babingtonia virgata"
# x[(x == "Callistemon sieberi Syn Callistemon paludosus")] = "Callistemon sieberi"
# x[(x == "Callistemon citrinus 'Splendens' (Syn. C. citrinus 'Endeavour)")] = "Callistemon citrinus 'Splendens'"
# x[(x == "Ficinia nodosa Syn. Isolepis nodosa")] = "Ficinia nodosa"
# x[(x == "Themeda triandra Syn. Themeda australis")] = "Themeda triandra"
# x[(x == "Xerochrysum viscosum Syn Bracteantha viscosa & Helichrysum viscosa")] = "Xerochrysum viscosum"
# x[(x == "Convolvulus sabatius (syn. C. mauritanicus)")] = "Convolvulus sabatius"
# x[(x == "Sannantha bidwillii syn. Babingtonia bidwillii")] = "Sannantha bidwillii"
# x[(x == "Rytidosperma sp syn. Danthonia sp")] = "Rytidosperma sp"
# # any containing x
# # x = gsub(" [Xx] ", " ", x)
# 
# unique(x)
# tail(ACTv1$species)
# ACTv1$species = x
# 
# a = data.frame( study = "ACTps", species = "Babingtonia virgata", trait_name = "synonyms", value = "Baeckea virgata", stringsAsFactors = F)
# b = data.frame( study = "ACTps", species = "Callistemon sieberi", trait_name = "synonyms", value = "Callistemon paludosus", stringsAsFactors = F)
# c = data.frame( study = "ACTps", species = "Callistemon citrinus 'Splendens'", trait_name = "synonyms", value = "Callistemon citrinus 'Endeavour'", stringsAsFactors = F)
# d = data.frame( study = "ACTps", species = "Ficinia nodosa", trait_name = "synonyms", value = "Isolepis nodosa", stringsAsFactors = F)
# e = data.frame( study = "ACTps", species = "Themeda triandra", trait_name = "synonyms", value = "Themeda australis", stringsAsFactors = F)
# f = data.frame( study = "ACTps", species = "Xerochrysum viscosum", trait_name = "synonyms", value = "Bracteantha viscosa", stringsAsFactors = F)
# g = data.frame( study = "ACTps", species = "Xerochrysum viscosum", trait_name = "synonyms", value = "Helichrysum viscosa", stringsAsFactors = F)
# h = data.frame( study = "ACTps", species = "Convolvulus sabatius", trait_name = "synonyms", value = "Convolvulus mauritanicus", stringsAsFactors = F)
# i = data.frame( study = "ACTps", species = "Sannantha bidwillii", trait_name = "synonyms", value = "Babingtonia bidwillii", stringsAsFactors = F)
# j = data.frame( study = "ACTps", species = "Rytidosperma sp", trait_name = "synonyms", value = "Danthonia sp", stringsAsFactors = F)
# ACTv1 = rbind(ACTv1, a, b, c, d, e, f, g, h, i, j, stringsAsFactors = F)
ACTv1$species = str_trim(ACTv1$species)
ACTv2 = ACTv1


# Now you are ready to look at the data
# 1.common_name = done

# 2.Description = later analysis file of descriptions
# 3 Origin. CHeck with everyone if they want states. Country and Continent could be good too. 
# 4.Plant type
# change trait_name to "form"
ACTv2$trait_name = gsub("plant_type", "form", ACTv2$trait_name)

library(dplyr)
library(stringr)

form = ACTv2 %>% filter(trait_name == "form") %>% select(value) %>% unique()

# unchanged values. take out capital letters.


for (i in 1:length(ACTv2$trait_name)){
  if (ACTv2$trait_name[i] == "form"){
    ACTv2$value[i] = str_to_lower(ACTv2$value[i])
  }
}
ACTv2$trait_name[ACTv2$value == "groundcover"] = "purpose"
# 5. light_requirements
# change light_requirements to light
ACTv2$trait_name = gsub("light_requirement", "light_level", ACTv2$trait_name)

ACTv2$value = gsub("Full Sun", "fullsun", ACTv2$value)
ACTv2$value = gsub("Part Shade", "partshade", ACTv2$value)
ACTv2$value = gsub("Shade", "fullshade", ACTv2$value)
# split based on commas
library(tidyr)

light = ACTv2 %>% filter(trait_name == "light_level")

lightsplit = separate(light, col = value, into = c("light1", "light2", "light3"), sep = ", ")
lightsplit$light2 = gsub(",", "", lightsplit$light2)
lightsplit$light3 = gsub(",", "", lightsplit$light3)
#gather the light traits
lightgather = lightsplit %>% gather(key = "discard", value = "value", light1:light3, na.rm = T)

newlight = unique(select(lightgather, - discard))
# I'm a bit unceratin because the species names are not unique. So I will lose data when I extract and relaod the traits. 
# But is it important? I don't think so but its still a shame.There are only about 6 species where this is the case
# Ok I'll just move on with it. 

ACTv2 = rbind(ACTv2[!(ACTv2$trait_name == "light_level"),], newlight, stringsAsFactors = F)
# 6. frost_tolerance
frost = ACTv2 %>% filter(trait_name == "frost_tolerance")
# there are three levels: hardy, mostly tolerant and sensative. 
# "Hardy" and "sensative" will go in the frost tolerance trait, while light_frost tolerance will be a yes for "mostly tolerant" 

frost[(frost$value == "Mostly Tolerant"),]$trait_name = "lightfrost_tolerance"

frost[(frost$value == "Mostly Tolerant"),] $value = "Yes"
##
frost[(frost$value == "Hardy"),] $value = "Yes"
frost[(frost$value == "Sensitive"),] $value = "No"

# bind on the amended data
ACTv2 = rbind(ACTv2[!(ACTv2$trait_name == "frost_tolerance"),], frost, stringsAsFactors = F)
# 7. Fire retardant trait. I'm deleting the no values and only keeping the yes values
fire = ACTv2[(ACTv2$trait_name == "fire_retardant"),]

fire = fire[!(fire$value == "No"),]
fire$trait_name = "purpose"
fire$value = "fire_retardant"
## bind back on
ACTv2 = rbind(ACTv2[!(ACTv2$trait_name == "fire_retardant"),], fire, stringsAsFactors = F)
#8. watering_requirement
# just change the name to supp_watering and str_to_lower
ACTv2[(ACTv2$trait_name == "watering_requirement"),]$trait_name = "supp_watering"
ACTv2[(ACTv2$trait_name == "supp_watering"),]$value = str_to_lower(ACTv2[(ACTv2$trait_name == "supp_watering"),]$value)
#plant_origin

origin = ACTv2[(ACTv2$trait_name == "plant_origin"),]
# select all that contain Australia and turn them into Australia
origin$value[grepl("Australia", origin$value)] = "Australia"

#select all the ones that have states in them and also say Australia
states = origin[(grepl("NSW", origin$value) == T|
                   grepl("WA", origin$value) == T|grepl("Q", origin$value) == T|
                grepl("Vic", origin$value) == T|grepl("SA", origin$value) == T|
                grepl("locally", origin$value) == T|grepl("ACT", origin$value) == T),]
#select all the ones that have states in them and also say Australia
states$trait_name = "country_of_origin"
states$value = "Australia"
origin = rbind(origin[!(grepl("NSW", origin$value) == T|
                   grepl("WA", origin$value) == T|grepl("Q", origin$value) == T|
                   grepl("Vic", origin$value) == T|grepl("SA", origin$value) == T|
                   grepl("locally", origin$value) == T|grepl("ACT", origin$value) == T),], states, stringsAsFactors = F)

#continent of origin
origin[grepl("Europe", origin$value) == T, ]$value = "Europe"
origin[origin$value == "Europe",]$trait_name = "continent_of_origin"

#Asia
origin[grepl("China..", origin$value)== T|grepl("Korea..", origin$value)== T,]$value = "Asia"
origin[grepl("Asia", origin$value)== T,]$value = "Asia"
origin[origin$value == "Asia",]$trait_name = "continent_of_origin"

#Mexico
origin[grepl("Mexico", origin$value)== T,]$value = "Mexico"
origin[origin$value == "Mexico",]$trait_name = "country_of_origin"
#New Zealand
origin[grepl("New Zealand", origin$value)== T,]$value = "New Zealand"
origin[origin$value == "New Zealand",]$trait_name = "country_of_origin"
#North America
origin[grepl("North America", origin$value)== T,]$value = "N_America"
origin[origin$value == "N_America",]$trait_name = "continent_of_origin"
# South Africa
origin[grepl("South Africa", origin$value)== T,]$value = "S_Africa"
origin[origin$value == "S_Africa",]$trait_name = "country_of_origin"
# South America
origin[grepl("South America", origin$value)== T,]$value = "S_America"
origin[origin$value == "S_America",]$trait_name = "continent_of_origin"
# Mediterranean
origin[grepl("editerranean..", origin$value)== T|grepl("North Africa", origin$value)== T,]$value = "Mediterranean"
origin[origin$value == "Mediterranean",]$trait_name = "continent_of_origin"
# delete the rest
origin = origin[!(grepl("Uncertain", origin$value)== T|grepl("Cultivar", origin$value)== T|grepl("Variable", origin$value)== T),]
# change the remaining plant_origin to country_of_origin
origin$trait_name[origin$trait_name == "plant_origin"] = "country_of_origin"

# paste in the ammended values

ACTv2 = rbind(ACTv2[!(ACTv2$trait_name == "plant_origin"),], origin, stringsAsFactors = F)

# make sure all the capital letters are gone

# remove repeats
ACTv2 = unique(ACTv2)
#cleaned!
write.csv(ACTv2, "./Scraped data/ACTplantselector/ACTv2.csv", row.names = F)
