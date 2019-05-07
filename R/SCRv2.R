#################################################################################################################
#################################################################################################################

# This code combines all the scraped data into one big database.
# It cleans the species names using the "taxize" R package
# It imports the manually collected database and merges the two together
# publishes the data as a csv file

setwd("C:/Users/MQ20182357/OneDrive - Macquarie University/The database")
library(taxize)
library(tidyverse)
files <- list.files(full.names = T, recursive = T, pattern = ".*v2.csv")

x = data.frame(study = character(), species = character(), trait_name = character(), value = character())

for (q in 1:length(files)){
 
  y  = read.csv(files[q], stringsAsFactors = F)
  
  x = rbind(x, y)
}

# clean up special characters from the species names
# This loop is to remove some pesky characters that didn't download properly during the scraping process
# Its not important in understand this.
a = str_match_all(x$species, "[[:alnum:]|[:blank:]|[:punct:]]" )

a[lengths(a$species) == 0] <- NA_character_



b = character()

for (i in 1:length(x$species) ){
  y1 = str_c(a[[i]], collapse = "")
  
  b =  append(b, y1)
}
############### Needed if chinese characters appear
# x2 = str_match_all(z, "[a-zA-Z]|[:blank:]")
# 
# z = character()
# for (i in 1:2736 ){
#   y = str_c(x2[[i]], collapse = "")
#   
#   z =  append(z, y)
# }

x$species = b

x$species = gsub("U009[12]", "'", x$species)
x$species = str_trim(x$species)

# Right, back to the database..:

# There are 5183 distinct species form the data I've collected so far. This includes cultivars etc
z = x %>% distinct(species, .keep_all = T)

# See how many can be matched perfectly using the taxize package. 

# This step takes about 20 mins!
x1 = gnr_resolve(z$species)

# Make a list of species that have a match and those that don't.

# get rid of spaces at either end of the name
x1$submitted_name = str_trim(x1$submitted_name)
x1$matched_name = str_trim(x1$matched_name)

# x2 species that match perfectly with a known species
x2 = x1[x1$matched_name == x1$submitted_name,]
x2 = distinct(x2, x2$submitted_name, .keep_all = T)

# take out all the one word species i.e. genera only
x2 = x2[!str_count(x2$submitted_name,"\\w+") == 1, ]

# x4 are the ones that do not match
x3 = setdiff(unique(x1$submitted_name), x2$matched_name)
x4 = x1[x1$submitted_name %in% x3,]
x4 = distinct(x4, x4$submitted_name, .keep_all = T)

# about 1630 species do not match!
# The new column of matched names is the submitted name column. We just have to get the 1070 into that one.
# Make a list of all the ones that are probably registered cultivars. 
# These should mostly have more than one capital letter so I can isolate them based on that:
Cultivars = x4 %>% filter(str_count(x4$user_supplied_name, "[A-Z]") > 1)

# Make a new list of everything else. These are varieties, subspecies and spelling errors etc
Leftovers = x4[!(x4$user_supplied_name %in% Cultivars$user_supplied_name),]

# now load the downloaded cultivar list and see if you can match them
realplants = read.csv( "Scraped data/Cultivar_list/cultivar_list.csv")
Cultivars$match = ""
Cultivars$match = Cultivars$user_supplied_name %in% realplants$x
length(Cultivars$match[Cultivars$match == TRUE])

# try the same matching process but with a fuzzy matching function: agrep()
for (i in 1:length(Cultivars$match)){
  if (length(agrep(Cultivars$user_supplied_name[i], realplants$x, ignore.case = T, value = T))== 0){
Cultivars$match[i] = NA
}else{
  Cultivars$match[i] = agrep(Cultivars$user_supplied_name[i], realplants$x, ignore.case = T, value = T)
}
}
length(Cultivars$match[is.na(Cultivars$match)==F])




# Better.. 479 or about 45% of the cultivars match
Leftovercultivars = Cultivars[which(is.na(Cultivars$match) == TRUE),]
Leftovercultivars = x[x$species %in% Leftovercultivars$user_supplied_name,]

# Even after our best effort, it looks like there are some important plants in there that we could do with including. The smaller plants, the traits may be quite similar to each other
#match them manually/with a few lines of code:

Leftovercultivars$newspecies[grepl("Acacia longifolia",Leftovercultivars$species)== T] = "Acacia longifolia subsp. longifolia"
Leftovercultivars$newspecies[grepl("Autumn Blaze",Leftovercultivars$species)== T] = "Acer freemanii Autumn Blaze"
Leftovercultivars$newspecies[grepl("Jeffersred",Leftovercultivars$species)== T] = "Acer freemanii Jeffersred"
Leftovercultivars$newspecies[grepl("Calodendrum capensis",Leftovercultivars$species)== T] = "Calodendrum capensis"
Leftovercultivars$newspecies[grepl("Cupressus glabra",Leftovercultivars$species)== T] = "Cupressus glabra 'Blue Ice'"
Leftovercultivars$newspecies[grepl("Elaeocarpus reticulatus White",Leftovercultivars$species)== T] = "Elaeocarpus reticulatus"
Leftovercultivars$newspecies[grepl("Raywood",Leftovercultivars$species)== T] = "Fraxinus angustifolia Raywoodii"
Leftovercultivars$newspecies[grepl("Cimmzam",Leftovercultivars$species)== T] = "Fraxinus pennsylvanica Cimmzam Cimmaron"
Leftovercultivars$newspecies[grepl("Captain Cook",Leftovercultivars$species)== T] = "Callistemon viminalis 'Captain Cook'"
Leftovercultivars$newspecies[grepl("Dusky Bells",Leftovercultivars$species)== T] = "Correa 'Dusky Bells'"
Leftovercultivars$newspecies[grepl("Tuscarora",Leftovercultivars$species)== T] = "Lagerstroemia indica 'Tuscarora'"
Leftovercultivars$newspecies[grepl("Natchez",Leftovercultivars$species)== T] = "Lagerstroemia indica 'Natchez'"
Leftovercultivars$newspecies[grepl("Yuma",Leftovercultivars$species)== T] = "Lagerstroemia indica 'Yuma'"
Leftovercultivars$newspecies[grepl("Zuni",Leftovercultivars$species)== T] = "Lagerstroemia indica 'Zuni'"
Leftovercultivars$newspecies[grepl("Lipan",Leftovercultivars$species)== T] = "Lagerstroemia indica 'Lipan'"
Leftovercultivars$newspecies[grepl("Souix",Leftovercultivars$species)== T] = "Lagerstroemia indica 'Sioux'"
Leftovercultivars$newspecies[grepl("Sioux",Leftovercultivars$species)== T] = "Lagerstroemia indica 'Sioux'"
Leftovercultivars$newspecies[grepl("Tonto",Leftovercultivars$species)== T] = "Lagerstroemia indica 'Tonto'"
Leftovercultivars$newspecies[grepl("Apalachee",Leftovercultivars$species)== T] = "Lagerstroemia indica 'Apalachee'"
Leftovercultivars$newspecies[grepl("Biloxi",Leftovercultivars$species)== T] = "Lagerstroemia indica 'Biloxi'"
Leftovercultivars$newspecies[grepl("Commanchee",Leftovercultivars$species)== T] = "Lagerstroemia indica 'Commanchee'"
Leftovercultivars$newspecies[grepl("Luscious",Leftovercultivars$species)== T] = "Tristaniopsis laurina Luscious"
Leftovercultivars$newspecies[grepl("Prunus cerasifera 'Oakville Crimson Spire'",Leftovercultivars$species)== T] = "Prunus cerasifera 'Oakville Crimson Spire'"
Leftovercultivars$newspecies[grepl("Eucalyptus leucoxylon 'Euky Dwarf",Leftovercultivars$species)== T] = "Eucalyptus leucoxylon 'Euky Dwarf'"
Leftovercultivars$newspecies[grepl("Nandina domestica 'AKA'",Leftovercultivars$species)== T] = "Nandina domestica 'AKA'"
Leftovercultivars$newspecies[grepl("Nandina domestica 'Nana'",Leftovercultivars$species)== T] = "Nandina domestica 'Nana'"
Leftovercultivars$newspecies[grepl("Red Robyn",Leftovercultivars$species)== T] = "Photinia x fraseri 'Red Robyn'"
Leftovercultivars$newspecies[grepl("Screen Master",Leftovercultivars$species)== T] = "Pittosporum tenuifolium Screenmaster"
Leftovercultivars$newspecies[grepl("Elvins",Leftovercultivars$species)== T] = "Prunus 'Elvins"
Leftovercultivars$newspecies[grepl("Sapporo",Leftovercultivars$species)== T] = "Ulmus 'Sapporo Autumn Gold'"
Leftovercultivars$newspecies[grepl("Ulmus parvifolia 'Todd'",Leftovercultivars$species)== T] = "Ulmus pavifolia Todd"
Leftovercultivars$newspecies[grepl("Smaragd",Leftovercultivars$species)== T] = "Thuja occidentalis Smaragd"
Leftovercultivars$newspecies[grepl("Raphiolepis 'Springtime'",Leftovercultivars$species)== T] = "Rhaphiolepis indica 'Springtime'"
Leftovercultivars$newspecies[grepl("Raphiolepis 'Ballerina'",Leftovercultivars$species)== T] = "Rhaphiolepis indica 'Ballerina'"
Leftovercultivars$newspecies[grepl("Phormium tenax Atropurpurea",Leftovercultivars$species)== T] = "Phormium tenax Purpurea"
Leftovercultivars$newspecies[grepl("Phormium tenax 'Atropurpurea'",Leftovercultivars$species)== T] = "Phormium tenax Purpurea"
Leftovercultivars$newspecies[grepl("Shademaster",Leftovercultivars$species)== T] = "Gleditsia triacanthos 'Shade Master'"

# Get rid of any rows that do not have a match
Leftovercultivars = Leftovercultivars[complete.cases(Leftovercultivars$newspecies),]

# finally, we need the spelling errors etc. in Leftovers

# Get rid of sp and spp
Leftovers = Leftovers[grepl("s[ps]p$",Leftovers$user_supplied_name) ==F, ]
Leftovers = Leftovers[grepl("sp$",Leftovers$user_supplied_name) ==F, ]
Leftovers = Leftovers[grepl("[ps]\\.$",Leftovers$user_supplied_name) ==F, ]
Leftovers = Leftovers[grepl("hybrid$",Leftovers$user_supplied_name) ==F, ]
Leftovers = Leftovers[grepl("hybrids$",Leftovers$user_supplied_name) ==F, ]
Leftovers = Leftovers[grepl("species$",Leftovers$user_supplied_name) ==F, ]
Leftovers = Leftovers[grepl("form$",Leftovers$user_supplied_name) ==F, ]
Leftovers = Leftovers[grepl("forms$",Leftovers$user_supplied_name) ==F, ]
Leftovers = Leftovers[grepl("dwarf",Leftovers$user_supplied_name) ==F, ]

Leftovers = Leftovers[!str_count(Leftovers$matched_name,"\\w+") == 1, ]
# Get rid of everything from the brackets in the matched name column and see if they match
Leftovers$matched_name = str_trim(gsub( "\\(.*", "", Leftovers$matched_name))
Leftovers$matched_name = str_trim(gsub( " [[:upper:]].*", "", Leftovers$matched_name))

Leftovers$user_supplied_name =
  Leftovers$newspecies = ""
for (i in 1:length(Leftovers$user_supplied_name)){
  if (grepl("ssp\\.", Leftovers$user_supplied_name[i]) == T){
    Leftovers$newspecies[i] = gsub("ssp\\.", "subsp\\.", Leftovers$user_supplied_name[i])
  }
}

for (i in 1:length(Leftovers$user_supplied_name)){
  if (grepl("ssp ", Leftovers$user_supplied_name[i]) == T){
    Leftovers$newspecies[i] = gsub("ssp ", "subsp\\. ", Leftovers$user_supplied_name[i])
  }
}

for (i in 1:length(Leftovers$user_supplied_name)){
  if (grepl("ssp ", Leftovers$user_supplied_name[i]) == T){
    Leftovers$newspecies[i] = gsub("subsp ", "subsp\\. ", Leftovers$user_supplied_name[i])
  }
}

for (i in 1:length(Leftovers$submitted_name)){
if (Leftovers$matched_name[i] == gsub(" var "," var. ", Leftovers$submitted_name[i]))
  Leftovers$newspecies[i] = Leftovers$matched_name[i]
}

for (i in 1:length(Leftovers$submitted_name)){
  if (Leftovers$matched_name[i] == gsub(" subsp "," subsp. ", Leftovers$submitted_name[i]))
    Leftovers$newspecies[i] = Leftovers$matched_name[i]
}

 # if the name is somewhere within the matched name after all the cleaning, move it over
for (i in 1:length(Leftovers$user_supplied_name)){
  if (grepl(Leftovers$user_supplied_name[i], Leftovers$matched_name[i])==T){
    Leftovers$newspecies[i] = Leftovers$user_supplied_name[i]
  }else if (grepl(Leftovers$submitted_name[i], Leftovers$matched_name[i])==T){
    Leftovers$newspecies[i] = Leftovers$matched_name[i]
  }
  }

# 
 takeout = c("Grevillea montis cole subsp brevistyla", "Thymus praecox pseudolanuginosus",      
             "x Citrofortunella microcarpa", "Eremophila glabra prostrate",           
             "Grevillea rhyolitica x juniperina" ,     "Liquidambar styraciflua 'festeri'" ,    
             "Ligustrum lucidum aureum", "Acmena smithii varminor"  ,             
             "Azalea karume",   "Michelia coco")         
 
Leftovers = Leftovers[Leftovers$submitted_name %in% takeout ==F,]

# everything remaining is a spelling error and we are taking the matched name

for (i in 1:length(Leftovers$matched_name)){
  if (Leftovers$newspecies[i] == ""){
    Leftovers$newspecies[i] = Leftovers$matched_name[i]
  }
}
Cultivars = Cultivars[which(is.na(Cultivars$match) == F),]
# Right, now we need to return to the orginal database with our lists and fill up a new species column with our matches
# These will be: Cultivars$user_supplied_name - matched to the cultivars list
#                Leftovercultivars$newspecies  - manually cleaned and the new species names added
#                x2$submitted_name             - The perfect species matches from the Taxize package
#                Leftover$newspecies           - Any that matched after a bit of cleaning
x$newspecies = ""
for (i in 1:length(x$species)){
  if (x$species[i] %in% Cultivars$user_supplied_name == T){
    x$newspecies[i] = Cultivars$match[x$species[i] == Cultivars$user_supplied_name]
  }else if(x$species[i] %in% Leftovercultivars$species == T){ 
    x$newspecies[i] = Leftovercultivars$newspecies[x$species[i] == Leftovercultivars$species]
  }else if (x$species[i] %in% x2$submitted_name == T){
    x$newspecies[i] = x2$submitted_name[x$species[i] == x2$submitted_name]
  }else if (x$species[i] %in% Leftovers$submitted_name == T){
    x$newspecies[i] = Leftovers$newspecies[x$species[i] == Leftovers$submitted_name]
  }else{
    x$newspecies[i] = NA_character_
}
} 

# lets inspect the final ditched species:
ditched = unique(x$species[is.na(x$newspecies) == T])
ditched
# OK there are some last minute adjustments in the ~ 800 or so diteched species that I know from scrolling through, have been missed.

x$newspecies[grepl("Leighton Green",x$species)== T] = "Cupressocyparis leylandii Leightons Green"
x$newspecies[grepl(" tenax ",x$species)== T] = "Phormium tenax Purpurea"
x$newspecies[grepl("Lomandra longifolia",x$species)== T] = "Lomandra longifolia"
x$newspecies[grepl("Abelia X grandiflora",x$species)== T] = "Abelia x grandiflora"
x$newspecies[grepl("s Giant",x$species)== T] = "Ajuga reptans 'Catlin's Giant'"
x$newspecies[grepl("Isolepis Nodosa",x$species)== T] = "Isolepis nodosa"
x$newspecies[grepl("Endeavour",x$species)== T] = "Callistemon 'Endeavour'"
x$newspecies[grepl(" Kings Park Special",x$species)== T] = "Callistemon 'Kings Park Special'"
x$newspecies[grepl("Cedrus atlantica 'Glauca'",x$species)== T] = "Cedrus atlantica Glauca"
x$newspecies[grepl("Cedrus atlantica Glauca",x$species)== T] = "Cedrus atlantica Glauca"
x$newspecies[grepl(" Blue Pacific",x$species)== T] = "Ceanothus 'Blue Pacific'"
x$newspecies[grepl("Abelia Francis Mason",x$species)== T] = "Abelia x grandiflora 'Francis Mason'"
x$newspecies[grepl("rbanite",x$species)== T] = "Fraxinus pennsylvanica 'Urbdell'-Urbanite"
x$newspecies[grepl("Aeonium arboreum Zwartkop",x$species)== T] = "Aeonium arboreum var. zwartkop"
x$newspecies[grepl("Buxus microphylla Japonica",x$species)== T] = "Buxus microphylla var. japonica"
x$newspecies[grepl("Tiny Trev",x$species)== T] = "Syzygium australe 'Tiny Trev'"
x$newspecies[grepl("Dicksonia Antarctica",x$species)== T] = "Dicksonia antarctica"
x$newspecies[grepl("Citrus limon x sinensis 'Meyer'",x$species)== T] = "Citrus limon 'Meyer'"
x$newspecies[grepl("Photinia robusta",x$species)== T] = "Photinia robusta"
x$newspecies[grepl("Coprosma 'Kirkii'",x$species)== T] = "Coprosma kirkii"
x$newspecies[grepl("Elaeocarpus 'Prima Donna'",x$species)== T] = "Elaeocarpus reticulatus 'Prima Donna'"
x$newspecies[grepl("Juniperus communis Hibernica",x$species)== T] = "Juniperus communis 'Hibernica'"
ditched = x[is.na(x$newspecies) == T,]
ditched_species = unique(ditched$species)

scraped = x[is.na(x$newspecies) == F,]
scraped_species = unique(scraped$species)

####################################################
# Lets leave the scraped data here and read in the manually collected data

# import the original database (pre any scraping)
g = read.csv("190225prescrapingDB.csv", stringsAsFactors = F)
g = unique(g)

g$trait_name_original = ""
g$value_original = ""
colnames(g)[2] = "date_sourced"
g$newspecies = g$species
g = g %>% select(List_source, date_sourced, species, newspecies, species_number, multiple_forms, study, trait_index, trait_name_original, trait_name, value_original, value)


# Now return to the scraped data and ammend so I can stitch the two together
scraped$List_source = "Scrape"
scraped$date_sourced = NA
scraped$species_number = NA
scraped$multiple_forms = NA
scraped$trait_index = NA

scraped = scraped %>% select(List_source, date_sourced, species, newspecies, species_number, multiple_forms, study, trait_index, trait_name_original, trait_name, value_original, value)

# these dates are sourced from the diary/online record system of Onedrive
scraped[scraped$study == "UCdavis",]$date_sourced = "3/2/2019"
scraped[scraped$study == "Benaranurseries",]$date_sourced = "30/1/2019"
scraped[scraped$study == "Ozbreed",]$date_sourced = "4/2/2019"
scraped[scraped$study == "ACTps",]$date_sourced = "5/2/2019"
#scraped[scraped$study == "TCCS",]$date_sourced = "6/2/2019"
scraped[scraped$study == "Treemovals",]$date_sourced = "5/2/2019"
#scraped[scraped$study == "KingsparkBG",]$date_sourced = "7/2/2019"
scraped[scraped$study == "Sydney_water",]$date_sourced = "12/2/2019"
scraped[scraped$study == "Plant_selector_plus",]$date_sourced = "14/2/2019"
scraped[scraped$study == "Gardensonline",]$date_sourced = "14/2/2019"
scraped[scraped$study == "Gardeningwithangus",]$date_sourced = "17/2/2019"
scraped[scraped$study == "Riyadh",]$date_sourced = "19/2/2019"

# add in the species_numbers to the new data (takes a while)
for (r in 1:length(scraped$newspecies)){
 if (scraped$newspecies[r] %in% unique(g$species) == T){
   scraped$species_number[r] = which(unique(g$species) == scraped$newspecies[r])
 }
}

str(scraped)

# make a list of trait index numbers that match each trait_name
j = read.csv("Traitsindexes.csv", stringsAsFactors = F)
# make a list of trait index numbers that match each trait_name
# add them to the trait index column (takes a while)
for (s in 1:length(scraped$trait_index)){
  if (scraped$trait_name[s] %in% j$Traits == T){
    scraped$trait_index[s] = j[j$Traits== scraped$trait_name[s],]$Trait_index
  }
  }

# now it is important to save the scraped data you are rejecting from the database.
# m is the data of rejected species
# n is the data of accepted species but rejected traits e.g. descriptions (because they are too wordy)
m = x[is.na(x$newspecies) == T,]
n = scraped[is.na(scraped$trait_index),]
write.csv(m, "rejected_species_data.csv", row.names = F, na = "")
write.csv(n, "wordy_trait_data.csv", row.names = F, na = "")
# check that only the residual traits that haven't been fixed are showing up here
unique(n$trait_name)

# now to stick them manually collected data and the scraped together
i = scraped[!is.na(scraped$trait_index),]

i = rbind(g, i)
# 
i = i %>% distinct(species, study, trait_name, value, .keep_all = T)
# sort
i = i %>% arrange(species_number, species, trait_index)


#Take out the blank traits - these are mostly from the manually collected data.
i = i[!i$value == "",]


# Done!

write.csv(i, "190321DB.csv", row.names = F, na = "")

