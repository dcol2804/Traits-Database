#set the wd and load in TREv1.csv
TREv1 = read.csv("./Scraped data/Alpinenurseries/Alpinenurseriesraw.csv", stringsAsFactors = F)

# retain old data
TREv1$trait_name_original = TREv1$trait_name
TREv1$value_original = TREv1$value


# x = TREv1$species
#unique(x)


# x[x == "Olea europaea (Fruiting)"] = "Olea europaea"
# 
# x[x == "Calodendrum capensis (GRAFTED)"] = "Calodendrum capensis"
# x[x == "Nerium oleander 'Madoni Grand' (White)"] = "Nerium oleander 'Madoni Grand'"
# x[x == "Nerium oleander 'Prof Martin' (Red)"] = "Nerium oleander 'Prof Martin'"
# 
# 
# x[x == "Lagerstroemia Ind Summer Natchez"] = "Lagerstroemia 'Indian Summer Natchez'"
# x[x == "Lagerstroemia Ind Summer 'Tuscarora'"] = "Lagerstroemia 'Indian Summer Tuscarora'"
# 
# x[x == "X Cupressocyparis 'Gold Rider'"] = "Cupressocyparis leylandii 'Gold Rider'"
# x[x == "Aspidistra elatior Varegata"] = "Aspidistra elatior 'Varegata'"
# x[x == "Waterhousea sweeper"] = "Waterhousea floribunda 'Sweeper'"

# syn
TREv1$species = gsub(" [Ss]yn.*", "", TREv1$species)

# unique(x[(str_which(x, "syn"))])
# x[x == "Acer palmatum 'Senkaki' syn A. palmatum 'Sango Kaku'"] = "Acer palmatum 'Senkaki'"
# # var 
# unique(x[(str_which(x, "var"))])
# x[x == "Buxus microphylla var japonica (ball)"] = "Buxus microphylla japonica"
# x[x == "Ficus microcarpa var hilli"] = "Ficus microcarpa hilli"
# x[x == "Fraxinus angustifolia var Raywoodii"] = "Fraxinus oxycarpa 'Raywoodii'"
# # x
# x = gsub(" X ", " ", x)
# x = gsub(" x ", " ", x)
# # miscellaneous
# x[x == "Clivia miniata Belgium hybrids"] = "Clivia miniata 'Belgian'"
# unique(x)
# TREv1$species = x
# a = data.frame( study = "Treemovals", species = "Acer palmatum 'Senkaki'", trait_name = "synonym", value = "Acer palmatum 'Sango Kaku'", stringsAsFactors = F)
# TREv1 = rbind(TREv1, a, stringsAsFactors = F)
# 
# #Ok, get rid of all punctuation
# TREv1$species = gsub("[[:punct:]]", "", TREv1$species)
# unique(TREv1$species)
# Right, now to start on the data itself
#There are 19 traits


unique(TREv1$trait_name)
# 1. common_name: no action required
# 2. family: all good there
# TREv1$trait_name[(TREv1$trait_name == "Family")] = "family"
# # 3. Origins
# origin = TREv1[(TREv1$trait_name == "Origins"),]
# 
# origin[grepl("Europe", origin$value) == T, ]$value = "Europe"
# origin[origin$value == "Europe",]$trait_name = "continent_of_origin"
# 
# origin[grepl("Mexico", origin$value)== T,]$value = "Mexico"
# origin[origin$value == "Mexico",]$trait_name = "country_of_origin"
# 
# origin[grepl("South Africa", origin$value)== T,]$value = "S_Africa"
# origin[origin$value == "S_Africa",]$trait_name = "country_of_origin"
# 
# origin[grepl("South east Asia", origin$value)== T,]$value = "SE_Asia"
# origin[origin$value == "SE_Asia",]$trait_name = "continent_of_origin"
# 
# origin[grepl("East Asia", origin$value)== T|grepl("Asia", origin$value)== T|grepl("Himalayas", origin$value)== T|grepl("Mekong", origin$value)== T,]$value = "Asia"
# origin[origin$value == "Asia",]$trait_name = "continent_of_origin"
# 
# origin[grepl("..China", origin$value) == T|grepl("..China", origin$value) == T, ]$value = "Asia"
# origin[origin$value == "Asia",]$trait_name = "continent_of_origin"
# origin[grepl("China", origin$value) == T, ]$value = "China"
# origin[origin$value == "China",]$trait_name = "country_of_origin"
# 
# origin[grepl("North America", origin$value)== T|grepl("orthern America", origin$value)== T,]$value = "N_America"
# origin[origin$value == "N_America",]$trait_name = "continent_of_origin"
# 
# origin[grepl("Japan", origin$value)== T,]$value = "Japan"
# origin[origin$value == "Japan",]$trait_name = "country_of_origin"
# 
# origin[grepl("USA", origin$value)== T|grepl("Alberta", origin$value)== T|
#          grepl("Florida", origin$value)== T|grepl("Minnesota", origin$value)== T|grepl("Carolina", origin$value)== T,]$value = "USA"
# origin[origin$value == "USA",]$trait_name = "country_of_origin"
# 
# origin[grepl("editerranean", origin$value)== T|grepl("north Africa", origin$value)== T,]$value = "Mediterranean"
# origin[origin$value == "Mediterranean",]$trait_name = "continent_of_origin"
# 
# origin[grepl("Australia", origin$value)== T|grepl("Queensland", origin$value)== T|
#          grepl("Sydney", origin$value)== T|grepl("NSW", origin$value)== T|
#          grepl("New", origin$value)== T,]$value = "Australia"
# origin[origin$value == "Australia",]$trait_name = "country_of_origin"
# 
# origin[grepl("South America", origin$value)== T|grepl("Peru", origin$value)== T,]$value = "S_America"
# origin[origin$value == "S_America",]$trait_name = "continent_of_origin"
# 
# origin[(origin$value == "Iran")|(origin$value == "Brazil"),]$trait_name = "country_of_origin"
# 
# origin[grepl("Africa", origin$value)== T,]$value = "Africa"
# origin[origin$value == "Africa",]$trait_name = "continent_of_origin"
# # get rid of leftover dud data
# origin = origin[!(origin$trait_name == "Origins"),]
# # now paste the data back in
# 
# TREv1 = rbind(TREv1[!TREv1$trait_name ==  "Origins",], origin, stringsAsFactors = F)
#  
# 4. Evergreen/deciduous

leafloss = TREv1[TREv1$trait_name == "Plant Type",]
# contains = Palm or palm
palms = leafloss[grepl("palm", leafloss$value) == T,]
palms$value = "palm"
palms$trait_name = "form"
# climate colder cool
climate = leafloss[grepl("cold", leafloss$value) == T|grepl("cool", leafloss$value) == T|grepl("climate", leafloss$value) == T|grepl("drought", leafloss$value) == T,]
climate$value = "climate_dependant"
drought = climate[4,]
drought$value = "water_dependant"
climate = rbind(climate, drought, stringsAsFactors = F)
climate$trait_name = "leaf_loss"
# contains semi deciduous or semi-deciduous
semideciduous = leafloss[grepl("semi deciduous", leafloss$value)== T|grepl("semi-deciduous", leafloss$value)== T|grepl("partly deciduous", leafloss$value)== T,]
semideciduous$value ="semi_deciduous"
semideciduous$trait_name ="leaf_loss"

# contains small tree
trees = leafloss[grepl("tree", leafloss$value) == T, ]
strees = trees[grepl("mall tree", trees$value) == T,]
strees$value = "smalltree"
strees$trait_name = "form"

trees = trees[!grepl("mall tree", trees$value) == T,]
#tree-like
arb = trees[grepl("tree-like", trees$value)== T,]
arb$trait_name = "habit"
arb$value = "arborescent"
#plain old tree
trees = trees[!grepl("tree-like", trees$value) == T,]
trees$trait_name = "form"
trees$value = "tree"

#"large shrub"
shrub = leafloss[grepl("shrub", leafloss$value) == T,]
lshrub = shrub[grepl("large shrub", shrub$value) == T,]
lshrub$trait_name = "form"
lshrub$value = "largeshrub"
#"shrub"
shrub = shrub[!grepl("large shrub", shrub$value) == T,]
shrub$trait_name = "form"
shrub$value = "shrub"
# multi-stemmed
ms = leafloss[grepl("multi", leafloss$value) == T,]
ms$trait_name = "multistem_development"
ms$value = "Yes"
#clumping
cl = leafloss[grepl("multi", leafloss$value) == T,]
cl$trait_name = "habit"
cl$value = "clumping"
# weeping
w = leafloss[grepl("weep", leafloss$value) == T,]
w$trait_name = "habit"
w$value = "weeping"

# decid
decid = leafloss[grepl("eciduo", leafloss$value) == T,]
decid$trait_name = "leaf_loss"
decid$value = "deciduous"
#evergreen
everg = leafloss[grepl("vergreen", leafloss$value) == T,]
everg$trait_name = "leaf_loss"
everg$value = "evergreen"
#combine
nleafloss = rbind(palms,climate,semideciduous, strees, trees, lshrub, shrub, ms, cl,w,decid,everg, stringsAsFactors = F)
#paste in replacement values
TREv2 = rbind(TREv1[!TREv1$trait_name == "Plant Type",], nleafloss, stringsAsFactors = F)

# 5. size
library(tidyr)
library(stringr)
size = TREv2[TREv2$trait_name == "Size",]


size = separate(size, col = value, into = c("height", "width", "extra"), sep = "x")

# anomilies
editheight = size[str_length(size$height)> 20,]

editheight$height[1] =  "0.5" 
#tricky one
max_height_nature = editheight[2,]
max_height_nature$height = "45"
max_height_nature = max_height_nature[,c(1:4,7,8)]
names(max_height_nature) = c("study", "species", "trait_name", "value","trait_name_original", "value_original")
max_height_nature$trait_name = "max_height_nature"

max_height1 = max_height_nature
max_height1$trait_name = "max_height"
max_height1$value = "25"

longev = max_height_nature
longev$trait_name = "longevity"
longev$value = "long (> 50 years)"

dimen = longev
dimen$trait_name = "dimensions"
dimen$value = "climate_dependant"

editheight$height[2] = "4-8m"
editheight$width[2] = "2m"

editheight$height[3] = "15-20m"
editheight$width[3] = "12m"

editheight$height[4] = "2-3m"
editheight$width[4] = "3-4m"

editheight$height[5] = "30-50m"
editheight$width[5] = NA

dimen1 = editheight[5,]
dimen1 = dimen1[,c(1:4,7,8)]
names(dimen1) = c("study", "species", "trait_name", "value","trait_name_original", "value_original")
dimen1$trait_name = "dimensions"
dimen1$value = "site_dependant"

editheight$height[6] = "25-30m"
editheight$width[6] = "8m"

editheight$height[7] = "25m"
editheight$width[7] = "9m"

editheight$height[8] = "0.5m"

editheight$height[9] = "2-5m"
editheight$height[10] = "2-5m"

editheight = editheight[-c(11:16),]

editheight$height[11] = "8-10m"
editheight$width[11] = "4m"

editheight$height[12] = "15-20m"
editheight$width[12] = "5m"

editheight$height[13] = "3-6m"
editheight$width[13] = "2m"


size = rbind(size[!str_length(size$height)> 20,], editheight, stringsAsFactors = F)
#now width
editwidth = size[str_length(size$width)> 20,]
editwidth = editwidth[!is.na(editwidth$species) == T,]

# naturalh = editwidth[1,]
# naturalh = naturalh[,c(1:4,7,8)]
# names(naturalh) = c("study", "species", "trait_name", "value","trait_name_original", "value_original")
# naturalh$trait_name = "max_height_nature"
# naturalh$value = "35"

editwidth$width[1] = "3-4m"

prun = editwidth[2,]
prun = prun[,c(1:4,7,8)]
names(prun) = c("study", "species", "trait_name", "value","trait_name_original", "value_original")
prun$trait_name = "maintenance_activities"
prun$value = "pruning"

editwidth$width[2] = "6 - 8m"

editwidth$width[3] = "4m"

dimen2 = editwidth[3,]
dimen2 = dimen2[,c(1:4,7,8)]
names(dimen2) = c("study", "species", "trait_name", "value","trait_name_original", "value_original")
dimen2$trait_name = "dimensions"
dimen2$value = "site_dependant"

editwidth$width[4] = "10m"

dimen3 = editwidth[4,]
dimen3 = dimen3[,c(1:4,7,8)]
names(dimen3) = c("study", "species", "trait_name", "value","trait_name_original", "value_original")
dimen3$trait_name = "dimensions"
dimen3$value = "site_dependant"

editwidth$width[5] = "1m"

dimen4 = editwidth[6,]
dimen4 = dimen4[,c(1:4,7,8)]
names(dimen4) = c("study", "species", "trait_name", "value","trait_name_original", "value_original")
dimen4$trait_name = "dimensions"
dimen4$value = "site_dependant"

editwidth$width[6] = "5-8m"

editwidth$width[7] = "5m"

max_height_nature1 = editwidth[7,]
max_height_nature1$height = "15"
max_height_nature1 = max_height_nature1[,c(1:4,7,8)]
names(max_height_nature1) = c("study", "species", "trait_name", "value","trait_name_original", "value_original")
max_height_nature1$trait_name = "max_height_nature"

editwidth$width[8] = "10-12m"

max_height_nature2 = editwidth[8,]
max_height_nature2$height = "30"
max_height_nature2 = max_height_nature2[,c(1:4,7,8)]
names(max_height_nature2) = c("study", "species", "trait_name", "value","trait_name_original", "value_original")
max_height_nature2$trait_name = "max_height_nature"

editwidth$width[9] = "12-15m"

max_height_nature3 = editwidth[9,]
max_height_nature3$height = "30"
max_height_nature3 = max_height_nature3[,c(1:4,7,8)]
names(max_height_nature3) = c("study", "species", "trait_name", "value","trait_name_original", "value_original")
max_height_nature3$trait_name = "max_height_nature"

editwidth$width[10] = "5-10m"

max_height_nature4 = editwidth[10,]
max_height_nature4$height = "25"
max_height_nature4 = max_height_nature4[,c(1:4,7,8)]
names(max_height_nature4) = c("study", "species", "trait_name", "value","trait_name_original", "value_original")
max_height_nature4$trait_name = "max_height_nature"

editwidth$width[11] = "10-15m"

editwidth$width[12] = "4m"

max_height_nature5 = editwidth[12,]
max_height_nature5$height = "10"
max_height_nature5 = max_height_nature5[,c(1:4,7,8)]
names(max_height_nature5) = c("study", "species", "trait_name", "value","trait_name_original", "value_original")
max_height_nature5$trait_name = "max_height_nature"

editwidth$width[13] = "0.9m"

max_height_nature6 = editwidth[13,]
max_height_nature6$height = "2"
max_height_nature6 = max_height_nature6[,c(1:4,7,8)]
names(max_height_nature6) = c("study", "species", "trait_name", "value","trait_name_original", "value_original")
max_height_nature6$trait_name = "max_height_nature"

editwidth$width[14] = "5-8m"

dimen5 = editwidth[14,]
dimen5 = dimen5[,c(1:4,7,8)]
names(dimen5) = c("study", "species", "trait_name", "value","trait_name_original", "value_original")
dimen5$trait_name = "dimensions"
dimen5$value = "climate_dependant"

editwidth$width[15] = "5-8m"

max_height_nature7 = editwidth[15,]
max_height_nature7$height = "35"
max_height_nature7 = max_height_nature7[,c(1:4,7,8)]
names(max_height_nature7) = c("study", "species", "trait_name", "value","trait_name_original", "value_original")
max_height_nature7$trait_name = "max_height_nature"

editwidth$width[16] = "3-5m"

max_height_nature8 = editwidth[16,]
max_height_nature8$height = "15"
max_height_nature8 = max_height_nature8[,c(1:4,7,8)]
names(max_height_nature8) = c("study", "species", "trait_name", "value","trait_name_original", "value_original")
max_height_nature8$trait_name = "max_height_nature"

editwidth$width[17] = "4-6m"

editwidth$width[18] = "10-12m"

max_height_nature9 = editwidth[18,]
max_height_nature9$height = "25"
max_height_nature9 = max_height_nature9[,c(1:4,7,8)]
names(max_height_nature9) = c("study", "species", "trait_name", "value","trait_name_original", "value_original")
max_height_nature9$trait_name = "max_height_nature"

editwidth$width[19] = "3m"
editwidth$width[20] = "3m"
editwidth$width[21] = "10m"

dimen6 = editwidth[21,]
dimen6 = dimen6[,c(1:4,7,8)]
names(dimen6) = c("study", "species", "trait_name", "value","trait_name_original", "value_original")
dimen6$trait_name = "dimensions"
dimen6$value = "climate_dependant"

dimen7 = editwidth[21,]
dimen7 = dimen7[,c(1:4,7,8)]
names(dimen7) = c("study", "species", "trait_name", "value","trait_name_original", "value_original")
dimen7$trait_name = "dimensions"
dimen7$value = "site_dependant"

editwidth$width[22] = "8-12m"
editwidth$width[23] = "8-12m"

max_height_nature11 = editwidth[22,]
max_height_nature11$height = "40"
max_height_nature11 = max_height_nature11[,c(1:4,7,8)]
names(max_height_nature11) = c("study", "species", "trait_name", "value","trait_name_original", "value_original")
max_height_nature11$trait_name = "max_height_nature"

max_height_nature12 = editwidth[23,]
max_height_nature12$height = "40"
max_height_nature12 = max_height_nature12[,c(1:4,7,8)]
names(max_height_nature12) = c("study", "species", "trait_name", "value","trait_name_original", "value_original")
max_height_nature12$trait_name = "max_height_nature"

editwidth$width[24] = "12-15m"
editwidth$width[25] = "4-6m"
# paste back the fixed up widths
size = rbind(size[!str_length(size$width)> 20,], editwidth, stringsAsFactors = F)
size = size[!is.na(size$species) == T,]
size1 = size[,c(1:5, 7,8)]
#now take out all the letters


size1$height = gsub("00", "", size$height)
size1$width = gsub("[[:alpha:]]", "", size$width)


#final doctering of the hights into max height, height and minheight
heightsplit = size1[grepl("-", size1$height) == T,c(1:4,6,7)]
#take out letters
heightsplit$height = gsub("[[:alpha:]]", "", heightsplit$height)
heightsplit = separate(heightsplit, col = height, into = c("min_height", "max_height"), sep = "-")
heightsplit = heightsplit %>% gather(key = "trait_name", value = "value", min_height:max_height, na.rm = T)

heights = size1[!grepl("-", size1$height) == T, c(1:4,6,7)]
heights$height = gsub("[[:alpha:]]", "", heights$height)
heights$trait_name = "height"
names(heights) = c("study", "species", "trait_name", "value","trait_name_original", "value_original")
# now widths
widthsplit = size1[grepl("-", size1$width) == T,c(1:3, 5:7)]
widthsplit = separate(widthsplit, col = width, into = c("min_width", "max_width"), sep = "-")
widthsplit = widthsplit %>% gather(key = "trait_name", value = "value", min_width:max_width, na.rm = T)
widths = size1[!grepl("-", size1$width) == T, c(1:3, 5:7)]
widths$trait_name = "width"
names(widths) = c("study", "species", "trait_name", "value","trait_name_original", "value_original")
#now stack all the size info together:

sizedata = rbind(heightsplit,heights,widthsplit, widths, max_height_nature, 
                 max_height1, longev, dimen,dimen1, prun, max_height_nature1,
                 max_height_nature2,max_height_nature3,max_height_nature4,
                 max_height_nature5,max_height_nature6,max_height_nature7,
                 max_height_nature8,max_height_nature9,
                 max_height_nature11,max_height_nature12,dimen2,dimen3,dimen4,dimen5,
                 dimen6,dimen7)
# now paste back into TREv2
TREv2 = rbind(TREv2[!TREv2$trait_name == "Size",], sizedata)


# 6. Vigour and longevity
Vig = TREv2[TREv2$trait_name == "Vigour / Longevity",]

unique(Vig$value)

# Ok, all of these are too tricky, Ill just publish now

write.csv(TREv2, "./Scraped data/Alpinenurseries/TREv2.csv", row.names = F)
