# Set working directory to Ozbreed and load OZBv1.csv
OZBv1 = read.csv("./Scraped data/Ozbreed/Ozbreedraw.csv", stringsAsFactors = F)

library(tidyverse)

# Clean up species column
OZBv1$species = gsub(" [Ss]yn.*", "", OZBv1$species)

OZBv1$trait_name_original = OZBv1$trait_name
OZBv1$value_original = OZBv1$value



# Get rid of x and hybrid
OZBv1 = OZBv1[complete.cases(OZBv1),]
unique(OZBv1$species)

# OZBv1$species = gsub(" hybrid", " sp", OZBv1$species)
# OZBv1$species = gsub(" x ", " ", OZBv1$species)
# OZBv1$species = gsub(" [[:punct:]] ", "", OZBv1$species)





unique(OZBv1$trait_name)
#1. description
#2. Size
#3. Uses
#4. Position
#5. Care
#6. Where it works
#7. Cultivar

#1. description
#leave it.
#2. Size
size = OZBv1[OZBv1$trait_name == "Size",]

#They all have an x! Skip most of the Benara steps
sizesplit = separate(size, col = value, into = c("height", "width"), sep = " x ", extra = "merge")
#will need to act on four of the columns that contain extra info.
Heightcm = sizesplit[grepl("cm", sizesplit$height) == T,c(1:4, 6,7)]

Heightcmsplit = Heightcm[grepl("-", Heightcm$height) == T,]
Heightcmsplit1 = Heightcm[grepl("^[0-9]{2}\\s", Heightcm$height) == T,]
Heightcmsplit2 = Heightcm[grepl("\\.", Heightcm$height) == T,]
Heightcmsplit2$height = "60 120cm"
Heightcmsplit1 = rbind(Heightcmsplit1,Heightcmsplit2)
Heightcmsplit1$height = gsub("[[:alpha:]]", "", Heightcmsplit1$height)
Heightcmsplit1$height = str_trim(Heightcmsplit1$height)
Heightcmsplit1$height = gsub("\\s+", "-",Heightcmsplit1$height)
Heightcmsplit = rbind(Heightcmsplit, Heightcmsplit1)
Heightcmsplit = separate(Heightcmsplit, col = height, into = c("min_height", "max_height"), sep = "-")
Heightcmsplit = Heightcmsplit %>% gather(key = "trait_name", value = "value", min_height:max_height, na.rm = T)
Heightcmsplit$value = gsub("[[:punct:]]", "", Heightcmsplit$value)
Heightcmsplit$value = as.numeric(gsub("[[:alpha:]]", "", Heightcmsplit$value))/100

# Now just the normal cm heights
Heightcm = Heightcm[!grepl("-", Heightcm$height) == T,]
Heightcm = Heightcm[!grepl("^[0-9]{2}\\s[0-9]", Heightcm$height) == T,]
Heightcm = Heightcm[!grepl("\\.", Heightcm$height) == T,]
Heightcm$height = gsub("[[:punct:]]", "", Heightcm$height)
Heightcm$height = as.numeric(gsub("[[:alpha:]]", "", Heightcm$height))/100
Heightcm$trait_name = "height"
names(Heightcm) = c("study", "species", "trait_name", "value", "trait_name_original", "value_original")


#Now for heights in m
Heightm = sizesplit[!grepl("cm", sizesplit$height) == T,c(1:4, 6,7)]
#convert any "to" to "-"
Heightm$height[14] = "1.5-2"
Heightm$height[18] = "1.5-1.8"
Heightm$height[20] = "7-12"
Heightm$height[24] = "6-10"
#take out all the letters
Heightm$height = gsub("[[:alpha:]]", "", Heightm$height)
#mins and maxes
Heightmsplit = Heightm[grepl("-", Heightm$height) == T,]
Heightmsplit = separate(Heightmsplit, col = height, into = c("min_height", "max_height"), sep = "-")
Heightmsplit = Heightmsplit %>% gather(key = "trait_name", value = "value", min_height:max_height, na.rm = T)

# now just Heightm
Heightm = Heightm[!grepl("-", Heightm$height) == T,]
names(Heightm) = c("study", "species", "trait_name", "value", "trait_name_original", "value_original")
Heightm$trait_name = "height"

#####################
# width cm min max
widthcm = sizesplit[grepl("cm", sizesplit$width) == T,c(1:3, 5:7)]
widthcm$width = gsub("[.;(].*","",widthcm$width)
widthcm1 = widthcm[grepl("^[0-9]{2}\\s", widthcm$width) == T,]
widthcm1$width = gsub("[[:alpha:]]", "", widthcm1$width)
widthcm1$width = str_trim(widthcm1$width)
widthcm1$width = gsub("\\s+", "-", widthcm1$width)
widthcm = rbind(widthcm[grepl("^[0-9]{2}\\s", widthcm$width) == F,], widthcm1)
widthcmsplit = widthcm[grepl("-", widthcm$width) == T,]
widthcmsplit = separate(widthcmsplit, col = width, into = c("min_width", "max_width"), sep = "-")
widthcmsplit = widthcmsplit %>% gather(key = "trait_name", value = "value", min_width:max_width, na.rm = T)
widthcmsplit$value[31] = "100"
widthcmsplit$value[42] = "100"
widthcmsplit$value = as.numeric(gsub("[[:alpha:]]", "", widthcmsplit$value))/100
#Done with the cmwidthsplits now for the cm width without "-"
widthcm = widthcm[!grepl("-", widthcm$width) == T,]
widthcm$width = as.numeric(gsub("[[:alpha:]]", "", widthcm$width))/100
widthcm$trait_name = "width"
names(widthcm) = c("study", "species", "trait_name", "value", "trait_name_original", "value_original")
#Now for widths in m
widthm = sizesplit[!grepl("cm", sizesplit$width) == T,c(1:3, 5:7)]
widthm$width[23] = "1.2-1.5"
widthm$width[31] = "1-1.5"
#take out everything after a full stop
widthm$width = gsub("[.;(].*","",widthm$width)
#take out all the letters
widthm$width = gsub("[[:alpha:]]", "", widthm$width)
#mins and maxes
widthmsplit = widthm[grepl("-", widthm$width) == T,]
widthmsplit = separate(widthmsplit, col = width, into = c("min_width", "max_width"), sep = "-")
widthmsplit = widthmsplit %>% gather(key = "trait_name", value = "value", min_width:max_width, na.rm = T)
names(widthmsplit) = c("study", "species", "trait_name", "value", "trait_name_original", "value_original")
# now just widthm
widthm = widthm[!grepl("-", widthm$width) == T,]
names(widthm) = c("study", "species", "trait_name", "value", "trait_name_original", "value_original")
widthm$trait_name = "width"

#now add alltogether and paste into the BENv2

newsize = rbind(Heightm, Heightcm, Heightmsplit, Heightcmsplit, widthm,widthmsplit,widthcm,widthcmsplit)
newsize = newsize[complete.cases(newsize),]
OZBv1 = rbind(OZBv1[!OZBv1$trait_name == "Size",], newsize)

#3. Uses
Uses = OZBv1[OZBv1$trait_name == "Uses",]
# border
# specimen plant
# roadside
# [Mm]ass planting
# feature planting
# hedge
# pots
# container
# raised bed
# street
# screen
# low water
# boggy
# ground cover
# evergreen
# prune
# watercourses
# draiange channels
# exposed area
# park
# desk
# indoor
# patio
# cut flower
# bird
# pot 
# right I need a glossary of terms that are commonly used in horticulture
Pos = OZBv1[OZBv1$trait_name == "Position",]

fullsun = Pos[which(grepl("[Ff]ull sun", Pos$value )== T|grepl("sunny", Pos$value )== T),]
fullsun$trait_name = "light_level"
fullsun$value = "fullsun"
partshade = Pos[which(grepl("part[ shade]", Pos$value )== T|grepl("moderate shade", Pos$value )== T|grepl("partial shade", Pos$value )== T|grepl("semi shade", Pos$value )== T),]
partshade$trait_name = "light_level"
partshade$value = "partshade"
fullshade = Pos[which(grepl("% shade", Pos$value )== T|grepl("heavy shade", Pos$value )== T),]
fullshade$trait_name = "light_level"
fullshade$value = "fullshade"

coast = Pos[which(grepl("coast", Pos$value )== T),]
coast$trait_name = "coastal_tolerance"
coast$value = "Yes"

drought = Pos[which(grepl("drought", Pos$value )== T),]
drought$trait_name = "drought_tolerance"
drought$value = "Yes"

drought = Pos[which(grepl("drought", Pos$value )== T),]
drought$trait_name = "drought_tolerance"
drought$value = "Yes"

frost = Pos[which(grepl("frost", Pos$value )== T & grepl("light frost", Pos$value )== F& grepl("sheltered", Pos$value )== F),]
frost$trait_name = "frost_tolerance"
frost$value = "Yes"
lfrost = Pos[which(grepl("light frost", Pos$value )== T & grepl("sheltered", Pos$value )== F),]
lfrost$trait_name = "lightfrost_tolerance"
lfrost$value = "Yes"

moist = Pos[which(grepl("moist", Pos$value )== T),]
moist$trait_name = "ideal_conditions"
moist$value = "moist"

welldrained = Pos[which(grepl("well[- ]drained", Pos$value )== T|grepl("free drain", Pos$value )== T),]
welldrained$trait_name = "ideal_conditions"
welldrained$value = "well_drained"

wind = Pos[which(grepl("wind", Pos$value )== T & grepl("protect", Pos$value )== F),]
wind$trait_name = "wind_tolerance"
wind$value = "Yes"

msoil = Pos[which(grepl("most soil", Pos$value )== T),]
msoil$trait_name = "soil_character"
msoil$value = "most"

establish_care = Pos[which(grepl("establish", Pos$value )== T),]
establish_care$trait_name = "establishment_care"
establish_care$value = "frostprotection"

raised = Pos[which(grepl("raised", Pos$value )== T),]
raised$trait_name = "establishment_care"
raised$value = "raised_bed"

newPos = rbind(fullsun, partshade, fullshade, wind, coast, drought, frost, 
               lfrost, raised, establish_care,msoil, welldrained, moist)
OZBv1 = rbind(OZBv1[!OZBv1$trait_name == "Position",], newPos)
#The remaining I'm too lazy to do right now. This version is for fast nontext data only 
OZBv1$value = OZBv1$value[complete.cases(OZBv1$value)]
OZBv1 = unique(OZBv1)
unique(OZBv1$trait_name)
write.csv(OZBv1, "./Scraped data/Ozbreed/OZBv2.csv", row.names =  F)


      