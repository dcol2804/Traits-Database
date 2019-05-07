#############################################################################################################
################################## 
#############################################################################################


# This code follows on from the merged database at the end of SCRv2.R, 190321DB.csv
# It implements the changes suggested in the recent traits meeting (see TRaitlistlong tab in the database journal)
# It takes out all the major traits that will not be included
# Then it goes to the value level changes
# Then lastly, it investigates the traits that require more info and makes those changes too.


#############################################################################################################
# this is to take out the traits that won't be part of my bit of the database
# or might be replaced with pics etc
library(tidyverse)
setwd("C:/Users/MQ20182357/OneDrive - Macquarie University/The database")

df = read.csv("190321DB.csv", stringsAsFactors = F)

# Take out all the traits that are:

# Synonyms
# Family
# Heat tolerance
# Pollution tolerance
# Leaf description
# Flower description
# Fruit type
# Origin climate
# Origin locations
# Maintenance
# Activities
# Pests and diseases

Takeout = c("synonyms", "family", "heat_tolerance", "pollution_tolerance", "leaf_description", 
"flower_description", "fruit_type", "climate_of_origin", "country_of_origin", "continent_of_origin", 
"maintenance_level", "maintenance_activities", "pests_diseases", "dimensions", "shade", 
"pestdisease_risk", "rootdisturbance_tolerance")

df1 = df[!(df$trait_name %in% Takeout),]

# Next move the smaller categories to their new home and get rid of the traits after that.

# light shade
df1[df1$value == "lightshade",]$value = "partshade"

# light frost
lf = df1[df1$trait_name == "lightfrost_tolerance",]
lf[lf$value == "Yes",]$value = "light"
lf$trait_name = "frost_tolerance"
lf = lf[!lf$value == "No",]
df1 = rbind(df1[!df1$trait_name == "lightfrost_tolerance",], lf)

# severedrought_tolerance

sdt = df1[df1$trait_name == "severedrought_tolerance",]
sdt[sdt$value == "Yes",]$trait_name = "drought_tolerance"
sdt = sdt[!sdt$value == "No",]
df1 = rbind(df1[!df1$trait_name == "severedrought_tolerance",], lf)
# shade
st = df1[df1$trait_name == "shade_tolerance",]
shadespecies = st[st$value == "Yes",]$species
shadespeciesll = df1[df1$value == "fullshade",]$species
length(shadespeciesll %in% shadespecies)
# therefore get rid of the yeses and Nos. 
shadespecies = st[st$value == "No",]$species
shadespeciesll = df1[df1$value == "fullsun",]$species
length(shadespecies %in% shadespeciesll  )
# so all are listed as having fullshade status
#get rid of the shade tolerance completely
df1 = df1[!df1$trait_name == "shade_tolerance",]

# severe waterlogging tolerance
swt = df1[df1$trait_name == "frequentwaterlogging",]
swt[swt$value == "Yes",]$trait_name = "waterlogging_tolerance"
swt = swt[!swt$value == "No",]
df1 = rbind(df1[!df1$trait_name == "frequentwaterlogging",], lf)

# Ok now we have finished getting rid of categories
# Next is to clean up the deep/ shallow root trait. I'm gonna put them all in the root depth category
df1[df1$value == "deep",]$trait_name = "root_depth"
df1[df1$value == "shallow",]$trait_name = "root_depth"
df1[df1$value == "average",]$trait_name = "root_depth"

# change slightacid and slight alkaline to acid and alkaline
df1[df1$value == "slight_acid",]$value = "acid"
df1[df1$value == "slight_alkaline",]$value = "alkaline"

# collapse shrubs and trees into small shrubs and small trees

df1[df1$value == "smallshrub",]$value = "shrub"
df1[df1$value == "medshrub",]$value = "shrub"
df1[df1$value == "largeshrub",]$value = "shrub"

df1[df1$value == "smalltree",]$value = "tree"
df1[df1$value == "medtree",]$value = "tree"
df1[df1$value == "largetree",]$value = "tree"

# Also the annual and perennial

df1[df1$value == "annual",]$trait_name = "longevity"
df1[df1$value == "perennial",]$trait_name = "longevity"

# sedge only has 3 plants.. change to grass

df1[df1$value == "sedge",]$value = "grass"

# take out entries that are climate dependant and water dependant for growth rate
df1 = df1[!df1$value == "climate_dependant",]
df1 = df1[!df1$value == "water_dependant",]

# introduce new values to longevity
df1[df1$value == "short (1 - 20 years)",]$value = "perennialshort"
df1[df1$value == "medium (20 -50 years)",]$value = "perennialmedium"
df1[df1$value == "long (> 50 years)",]$value = "perenniallong"

# continuous flowering is absorbed by Any_time
df1[df1$value == "continuous",]$value = "Any_time"
# a whole bunch of things have to be removed
df1 = df1[!df1$value == "rare",] 
df1 = df1[!df1$value == "after_rain",]
df1 = df1[!df1$value == "extended",]
df1 = df1[!df1$value == "short",]
df1 = df1[!df1$value == "after_dry",]
# change potplant into container
df1[df1$value == "potplant",]$value = "container"

# combine balcony into container
df1[df1$value == "balcony",]$value = "container"

# get rid of amenity and powerlines
df1 = df1[!df1$value == "amenity",]
df1 = df1[!df1$value == "powerlines",]
# get rid of tea, cutfoliage, sensory sound

df1 = df1[!df1$value == "tea",]
df1 = df1[!df1$value == "cutfoliage",]
df1 = df1[!df1$value == "sensorysound",]
df1 = df1[!df1$value == "sensory_sound",]
# move bee to pollinator

df1[df1$value == "bee",]$value = "pollinator"

# take out possible weed
df1 = df1[!df1$value == "possible_weed",]

# move largesize to infrastructure damage

df1 = df1[!df1$value == "largesize",]

# Now the trickiest of all, the root invasiveness
#3.5 meters PSP
df1[df1$value == "sewer_3.5" & df1$study == "Plant_selector_plus",]$value = "Yes_SA"
# any left over are probably marked as invasive
df1[df1$value == "sewer_3.5",]$value = "invasive"

# 4 m Watercorp
df1[df1$value == "sewer_4" & df1$study == "Watercorp",]$value = "Yes_NSW"
# any left over are probably marked as invasive
df1[df1$value == "sewer_4",]$value = "invasive"

# 6 m Sydney_water
df1[df1$value == "sewer_6" & df1$study == "Sydney_water",]$value = "Yes_SW"
df1[df1$value == "sewer_6" & df1$study == "Watercorp",]$value = "Yes_NSW"
# any left over are probably marked as invasive
df1[df1$value == "sewer_6",]$value = "invasive"

# 10 m Sydney_water
df1[df1$value == "sewer_10" & df1$study == "Watercorp",]$value = "Yes_NSW"
# any left over are probably marked as invasive
df1[df1$value == "sewer_10",]$value = "invasive"

# 20 m Sydney_water
df1[df1$value == "sewer_20" & df1$study == "Watercorp",]$value = "Yes_NSW"
# any left over are probably marked as invasive
df1[df1$value == "sewer_20",]$value = "invasive"

# change buttress roots to charactersitic feature
df1[df1$value == "buttress",]$trait_name = "other_feature"

# change the couple of trait_names
df1[df1$trait_name == "other_feature",]$trait_name = "characteristic_feature"
df1[df1$trait_name == "purpose",]$trait_name = "usage"

#######################################################
# Thats all the big changes done, without analysis and "poking around".

# Now I have to do further investigation on a bunch of traits:

# Soil character
# is this the same as soil fertility and do they grow in wet environments etc? 
# convert all the sand_loam values to sand values and loam values
sl = df1[df1$value == "sand_loam",]
sl$value = "sand"
sl1 = df1[df1$value == "sand_loam",]
sl1$value = "loam"

df1 = rbind(df1[!df1$value == "sand_loam",], sl, sl1)


# do the same with clay_loam
cl = df1[df1$value == "clay_loam",]
cl$value = "clay"
cl1 = df1[df1$value == "clay_loam",]
cl1$value = "loam"

df1 = rbind(df1[!df1$value == "clay_loam",], cl, cl1)

# get rid of duplicates
df1 = unique(df1)

# Now lets see how well this compares to well drained trait in soil character
wd = df1[df1$value == "welldrained",]
wd = wd %>% filter() %>% distinct(species, .keep_all = T)
candl = intersect(unique(df1$species[df1$value == "sand"]), unique(df1$species[df1$value == "loam"]))
# add on just sand as well. Only 8 of those species
candl = unique(append(candl, unique(df1$species[df1$value == "sand"])))
# there are 698 species out of the 788 which have just sand or sand and loam. 
y = intersect(candl, wd$species)
# Now have a look at the 25 species that have welldrained soil character but not sand or sand and loam
# (function I got from https://www.r-bloggers.com/outersect-the-opposite-of-rs-intersect-function/)
outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}

z = outersect(wd$species, y)

a = df1 %>% filter(newspecies %in% z)
# so "a" shows that there are 25 species which have clay or clayloam as their soil type but require well drained soils. 
# is this enough to get rid of the category? Probably.
# does it go the otherway and we should add sandy soils to these plants? No, just leave it.
df1 = df1[!df1$value == "welldrained",]




moist = df1[df1$value == "moist" & df1$trait_name == "soil_character",]
moist1 = df1[df1$trait_name == "supp_watering" & df1$value == "medium",]
moist2 = df1[df1$trait_name == "supp_watering" & df1$value == "high",]
b = intersect(moist$species, unique(append(moist1$species, moist2$species)))
d = moist[!moist$species %in% b,]
c = df1[(df1$species %in% d$species) & df1$trait_name == "supp_watering",]
d$trait_name = "supp_watering"
d$value = "medium"
df1 = rbind(df1[! (df1$value == "moist" & df1$trait_name == "soil_character"),], d)
# so only 107 of the 338 species have an equivalent in the ideal conditions
# why not just add moist to the ideal conditions for the other 231 species and be done with it? 
# check before you do so

# Soil fertility - make them all a high in soil fertility and remove duplicates
fert = df1[df1$value == "fertile" & df1$trait_name == "soil_character",]
fert$trait_name = "soil_fertility"
fert$value = "high"

df1 = rbind(df1[!(df1$value == "fertile" & df1$trait_name == "soil_character"),], fert)
df1 = unique(df1)

# now to "most". I hypothesise that there would be a fair overlap with all three soil types.  
most = df1[df1$value == "most" & df1$trait_name == "soil_character",]

soil3 = intersect(unique(df1$species[df1$value == "sand"]), unique(df1$species[df1$value == "loam"]))
soil3 = intersect(soil3, unique(df1$species[df1$value == "clay"]))
b = intersect(unique(most$species), soil3)
c = outersect(unique(most$species), b)
d = df1[(df1$species %in% c) & df1$trait_name == "soil_type",]
# so of 295 "most"s, 258 of them have all three soil types. Do we interpolate that they should have all three? 
# I'm going to leave the 30 or so species that I have recorded most soils and they dont possess all three.

df1 = df1[!(df1$value == "most" & df1$trait_name == "soil_character"),]


# Canopy shape
# Is it the same as habit?
# Three of them are: open, spreading and rounded
cso = df1[df1$trait_name == "canopy_shape" & df1$value == "open",]
cso1 = df1[df1$trait_name == "habit" & df1$value == "open",]

length(intersect(unique(cso$species), unique(cso1$species)))

css = df1[df1$trait_name == "canopy_shape" & df1$value == "spreading",]
css1 = df1[df1$trait_name == "habit" & df1$value == "spreading",]

length(intersect(unique(css$species), unique(css1$species)))

csr = df1[df1$trait_name == "canopy_shape" & df1$value == "rounded",]
csr1 = df1[df1$trait_name == "habit" & df1$value == "rounded",]

length(intersect(unique(csr$species), unique(csr1$species)))

# So these largely don't overlap. Could combine them into the same category (habit/canopy shape)?
# also, looking at the list there is a vase_shape and tghe shrubby habit could combine with bushy
# low growing can combine with compact. Combine pendulous/weeping. twisted can be branching. vigerous can be got rid of
cs = unique(df1[df1$trait_name == "canopy_shape",]$value)
cs
h = unique(df1[df1$trait_name == "habit",]$value)
h
# vase_shaped
df1$value[df1$value == "vase_shaped"] = "vase"
# shrubby to bushy
df1$value[df1$value == "shrubby"] = "bushy"
# low growing to compact
df1$value[df1$value == "low_growing"] = "compact"
# pendulous to weeping
df1$value[df1$value == "pendulous"] = "weeping"
# twisted to branching
df1$value[df1$value == "twisted"] = "branching"
# "climbing" to "climber" under form
df1[df1$value == "climbing",]$trait_name = "form"
df1$trait_name[df1$value == "climbing"] = "form"
# get rid of vigerous
df1 = df1[!df1$value == "vigrous",]

# finally change both trait_names to habit_canopy

df1[df1$trait_name == "habit",]$trait_name = "habit_canopy"
df1[df1$trait_name == "canopy_shape",]$trait_name = "habit_canopy"

# foliage colour 
# condense number of traits down
fc = unique(df1[df1$trait_name == "foliage_colour",]$value)
fc
# seasonal colour
# There are only like one of each which have a season. move them over
df1[df1$value == "Spring" & df1$trait_name == "seasonal_colour",]$trait_name = "colour_season"
df1[df1$value == "Winter" & df1$trait_name == "seasonal_colour",]$trait_name = "colour_season"
df1[df1$value == "Autumn" & df1$trait_name == "seasonal_colour",]$trait_name = "colour_season"
df1[df1$value == "Summer" & df1$trait_name == "seasonal_colour",]$trait_name = "colour_season"
# get rid of "none
df1 = df1[!(df1$value == "none" & df1$trait_name == "seasonal_colour"),]
# colour season
# Do some analysis on whether they are the same as each other/of leaf colour

# flower colour
# condense
fc = unique(df1[df1$trait_name == "flower_colour",]$value)

fc

fc = df1[df1$trait_name == "flower_colour",]

fc = fc[!fc$value == "no",]
fc[fc$value == "mauve"| fc$value == "lilac"| fc$value == "lavender"|fc$value ==  "violet",]$value = "purple"
fc[fc$value == "mauve"| fc$value == "lilac",] = "orange"
fc[fc$value == "crimson"| fc$value == "cherry"| fc$value == "magenta"| fc$value == "maroon",]$value = "red"
fc[fc$value == "fuschia"| fc$value == "rose",]$value = "pink"
fc[fc$value == "golden",]$value = "yellow"

df1 = rbind(df1[!df1$trait_name == "flower_colour",], fc)
# supp water
# is this the same as drought tolerance? 

sw = df1[df1$trait_name == "supp_watering",]
swn = unique(sw[sw$value == "none",]$species)
swl = unique(sw[sw$value == "low",]$species)
swm = unique(sw[sw$value == "medium",]$species)
swh = unique(sw[sw$value == "high",]$species)

dt = df1[df1$trait_name == "drought_tolerance",]
dty = unique(dt[dt$value == "Yes",]$species)
dtn = unique(dt[dt$value == "No",]$species)
# see the intersction between low or no supp watering and drought tolerance
length(intersect(dty, unique(append(swl, swn))))
  # out of 1609, 1122 species that have low or none supp watering are marked as drought tolerant. (dty is 1404)

length(intersect(dtn, unique(append(swm, swh))))
# out of 880, species that are marked as moderate to high supp watering, (and 347 dtn) 241 are marked as not drought tolerant.
# Now look at weed_status
weedspecies = unique(df1[df1$trait_name == "weed_status",]$species)
weeds = df1[df1$trait_name == "weed_status",]
weeds

# 4 species. Jesus. Well at least we got more into the database
# now switch the old categories to the new ones

weeds$value = gsub("weed_", "", weeds$value)
weeds$value = str_to_lower(weeds$value)

df1 = rbind(df1[!df1$trait_name == "weed_status",], weeds)


# weed status
wl = read.csv("Scraped data/apc_weed_list_Rachael.csv", stringsAsFactors = F)
# Extract exactly what is needed from Rachael's speadsheet
# wl$canonicalName = gsub(" x ", " ", wl$canonicalName)
# wl$canonicalName = gsub(" subsp. ", " ", wl$canonicalName)
# wl$canonicalName = gsub(" var. ", " ", wl$canonicalName)
# wl$canonicalName = gsub(" sp. ", " ", wl$canonicalName)
# take out everything from the first bracket
wl$canonicalName = gsub(" \\(.*", "", wl$canonicalName)
length(intersect(unique(wl$canonicalName), unique(df1$newspecies)))


# x = wl[which(str_count(wl$canonicalName, "[[:upper:]]") > 1), ]

wl = wl[wl$regionName == "nsw"|wl$regionName == "nt"|wl$regionName == "qld"|wl$regionName == "wa"|
          wl$regionName == "tas"|wl$regionName == "vic"|wl$regionName == "sa"|wl$regionName == "act",]

w = intersect(unique(df1$newspecies), unique(wl$canonicalName))

wl1 = wl[wl$canonicalName %in% w,]

wl1 = wl1 %>% select(canonicalName, regionName, naturalised:potentiallyNativeAndNaturalised)

for (i in 1:length(wl1$canonicalName)){
  if (wl1$naturalised[i] == TRUE){
      wl1$naturalised[i] = wl1$regionName[i]
  }else{
    wl1$naturalised[i] = ""
  }
  if (wl1$potentiallyNaturalised[i] == TRUE){
    wl1$potentiallyNaturalised[i] = str_c("potentially_", wl1$regionName[i])
    
  }else{
    wl1$potentiallyNaturalised[i] = ""
  }
  if (wl1$nativeAndNaturalised[i] == TRUE){
    wl1$nativeAndNaturalised[i] = wl1$regionName[i]

  }else{
    wl1$nativeAndNaturalised[i] = ""
  }
  if (wl1$potentiallyNativeAndNaturalised[i] == TRUE){
    wl1$potentiallyNativeAndNaturalised[i] = str_c("potentially_", wl1$regionName[i])
    
  }else{
    wl1$potentiallyNativeAndNaturalised[i] = ""
  }
}

# now change all the FALSEs into "" and string them together
wl1$value = str_c(wl1$naturalised, wl1$potentiallyNaturalised, wl1$nativeAndNaturalised, wl1$potentiallyNativeAndNaturalised)
wl1 = wl1[!wl1$value == "",]
length(unique(wl1$canonicalName))

# add into database
wl1$List_source = "Rachaelcsv"
wl1$date_sourced = "28/3/2019"
wl1$species_number = NA
wl1$newspecies = wl1$canonicalName
wl1$multiple_forms = NA
wl1$trait_index = NA
wl1$trait_name_original = ""
wl1$value_original = ""
wl1$study = "APC"
wl1$trait_name = "weed_status"
wl2 = data.frame(List_source = wl1$List_source, date_sourced = wl1$date_sourced, species = wl1$canonicalName, newspecies = wl1$newspecies, species_number = wl1$species_number, multiple_forms = wl1$multiple_forms, study = wl1$study, trait_index = wl1$trait_index, trait_name_original = wl1$trait_name_original, trait_name = wl1$trait_name, value_original = wl1$value_original, value = wl1$value, stringsAsFactors = F)

df1 = rbind(df1, wl2)

# cool!

# Now add in the new amended trait indexes (Takes a while)

j = read.csv("Traitsindexes5.csv", stringsAsFactors = F)

for (s in 1:length(df1$trait_index)){
  if (df1$trait_name[s] %in% j$Traits == T){
    df1$trait_index[s] = j[j$Traits== df1$trait_name[s],]$Trait_index
  }
}

# one last matching thing for the cultivars

df1$newspecies = gsub("[[:punct:]]", "", df1$newspecies)


# summarise and make a table

o = as.data.frame(table(df1$trait_name))

for (t in 1:length(o$Var1)){
  if (o$Var1[t] %in% j$Traits == T){
    o$trait_index[t] = j[j$Traits== o$Var1[t],]$Trait_index
  }
}
o = arrange(o, trait_index)

# now write the table as a csv

write.csv(o, "190410traitssummary.csv", row.names = F)

# species in total
length(unique(df1$newspecies))
# 4164
# find out how many clean binomials species you have
b = df1 %>% filter(str_count(df1$newspecies,"\\w+") == 2) 
length(unique(b$newspecies))
# 3532

# cultivars
c = df1 %>% filter(str_count(df1$newspecies, "[A-Z]") > 1)
length(unique(c$newspecies))
# 442

4164 - 3538 - 442

# 184

df1 = unique(df1)
colnames(df1)[7] = "source"

write.csv(df1, "190417DB.csv", row.names = F)

# prepare the species index tab

speciesindex = df1 %>% select(newspecies, species_number, multiple_forms, date_sourced, List_source) 
speciesindex = speciesindex %>% distinct(newspecies, .keep_all = T)
speciesindex$species_number = 1:length(speciesindex$newspecies)
write.csv(speciesindex, "190417specieslist.csv", row.names = F)
?filter
