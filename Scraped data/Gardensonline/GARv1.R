#Set the working directory to Gardensonline and import the GARv1.csv file

GARv1 = read.csv("./Scraped data/Gardensonline/Gardensonlineraw.csv", stringsAsFactors = F)

library(stringr)
library(dplyr)
library(tidyr)



GARv1$species = gsub(" [Ss]yn.*", "", GARv1$species)

GARv1$trait_name_original = GARv1$trait_name
GARv1$value_original = GARv1$value
#We have to clean up the species column first
# isolate all the species with syn in the name
syns = GARv1[grepl("[Ss]yn[. ]",GARv1$species) == T,]
syns1 = GARv1[grepl("[Ss]ny[. ]",GARv1$species) == T,]
syns1$species = gsub("ny", "yn", syns1$species)
syns2 = GARv1[grepl("[Ss]yn,",GARv1$species) == T,]
syns2$species = gsub(",", ".", syns2$species)
syns = rbind(syns, syns1, syns2)
#replace all value column with the strings after the syn word
syns$value = sub(".*[Ss]yn[. ]","",syns$species)
#Go back to the original dataframe and replace the species with everything before the syn word
GARv1$species = sub("[Ss]yn[. ].*","",GARv1$species)
#do the same for the syns dataframe
syns$species = sub("[Ss]yn[. ].*","",syns$species)
syns$trait_name = "synonyms"
syns = unique(syns)
# Now paste back in the 200 or so synonym values 
GARv1 = rbind(GARv1, syns)
#get rid of x
GARv1$species = gsub(" [Xx] ", " ", GARv1$species)
GARv1$species = gsub(" ssp[. ]", " ", GARv1$species)
GARv1$species = gsub(" group", "", GARv1$species)
GARv1$species = gsub(" var[. ]", "", GARv1$species)
GARv1$species = gsub(" cultivar", "", GARv1$species)
GARv1$species = gsub(" [Hh]ybrids", "", GARv1$species)
GARv1$species = gsub("[[:punct:]]", "", GARv1$species)
GARv1$species = gsub("\\s+", " ", GARv1$species)
unique(GARv1$species)
#now get rid of empty space
GARv1$species = str_trim(GARv1$species)
#complete cases
GARv1 = GARv1[complete.cases(GARv1$value),]
#1. common_name

CN = GARv1[GARv1$trait_name == "common_name",]
CN = separate(CN, col = value, into = c("common_name", "common_name1", "common_name2", "common_name3", "common_name4"), sep = ", ")
CN = CN %>% gather(key = "trait_name", value = "value", common_name:common_name4, na.rm = T)
CN$trait_name = "common_name"

GARv1 = rbind(GARv1[!GARv1$trait_name == "common_name",], CN) 
#2. Type
type = GARv1[GARv1$trait_name == "Type",]
type = separate(type, col = value, into = c("common_name", "common_name1", "common_name2", "common_name3", "common_name4"), sep = "/")
type = type %>% gather(key = "trait_name", value = "value", common_name:common_name4, na.rm = T)
type$value = str_to_lower(type$value)
unique(type$value)
type$trait_name = "form"
type$value = gsub("lawn", "grass", type$value)
type$value = gsub("cactii", "succulent", type$value)
type$value = gsub("fruit", "edible", type$value)
type[type$value == "edible",]$trait_name = "purpose"
type$value = gsub("nuttrees", "edible", type$value)
type[type$value == "edible",]$trait_name = "purpose"
type$value = gsub("creeper", "climber", type$value)
type[type$value == "groundcover",]$trait_name = "purpose"
type$value = gsub("tuber", "bulb", type$value)
type$value = gsub("corm", "bulb", type$value)

GARv1 = rbind(GARv1[!GARv1$trait_name == "Type",], type) 
#3. Family
GARv1[GARv1$trait_name == "Family",]$trait_name = "family"
GARv1[GARv1$trait_name == "family",]$value = str_to_sentence(GARv1[GARv1$trait_name == "family",]$value)
#4. Origins
OR = GARv1[GARv1$trait_name == "Origins",]
unique(OR$value)
# I literally can't be bothered with this
for (i in 1:length(OR$value)){
if (grepl("Aust", OR$value[i])== T|grepl("NSW", OR$value[i])== T|grepl("Queensland", OR$value[i])== T|grepl("Vict", OR$value[i])== T|grepl("WA", OR$value[i])== T|grepl("NT", OR$value[i])== T|grepl("VIC", OR$value[i])== T|grepl("New South", OR$value[i])== T|grepl("Sydney", OR$value[i])== T|grepl("Tasmania", OR$value[i])== T){

  OR$value[i] = "Australia"
  OR$trait_name[i] = "country_of_origin"
}
}
for (i in 1:length(OR$value)){
  if (grepl("Europe", OR$value[i])== T){
    
    OR$value[i] = "Europe"
    OR$trait_name[i] = "continent_of_origin"
  }
}
for (i in 1:length(OR$value)){
  if (grepl("USA", OR$value[i])== T){
    
    OR$value[i] = "USA"
    OR$trait_name[i] = "country_of_origin"
  }
}
for (i in 1:length(OR$value)){
  if (grepl("China", OR$value[i])== T){
    
    OR$value[i] = "China"
    OR$trait_name[i] = "country_of_origin"
  }
}
for (i in 1:length(OR$value)){
  if (grepl("Mexico", OR$value[i])== T){
    
    OR$value[i] = "Mexico"
    OR$trait_name[i] = "country_of_origin"
  }
}
for (i in 1:length(OR$value)){
  if (grepl("New Zealand", OR$value[i])== T){
    
    OR$value[i] = "New Zealand"
    OR$trait_name[i] = "country_of_origin"
  }
}
for (i in 1:length(OR$value)){
  if (grepl("Japan", OR$value[i])== T){
    
    OR$value[i] = "Japan"
    OR$trait_name[i] = "country_of_origin"
  }
}
OR = OR[!OR$trait_name == "Origins",]
GARv1 = rbind(GARv1[!GARv1$trait_name == "Origins",], OR) 
#5. Light
Light = GARv1[GARv1$trait_name == "Light",]
unique(Light$value)
#Light$value = str_to_lower(Light$value)
Light$value = str_trim(Light$value)
unique(Light$value)
Light$value = gsub("Sun / Shade", "fullsun/partshade", Light$value)
Light$value = gsub("Full sun/Light shade", "fullsun/lightshade", Light$value)
Light$value = gsub("Full sun", "fullsun", Light$value)
Light$value = gsub("Light / Full shade", "lightshade/fullshade", Light$value)
Light$value = gsub("Shade", "fullshade", Light$value)

Light = separate(Light, col = value, into = c("common_name", "common_name1", "common_name2", "common_name3", "common_name4"), sep = "/")
Light = Light %>% gather(key = "trait_name", value = "value", common_name:common_name4, na.rm = T)
unique(Light$value)
Light$trait_name = "light_level"

GARv1 = rbind(GARv1[!GARv1$trait_name == "Light",], Light) 
#6. Wind
Wind = GARv1[GARv1$trait_name == "Wind",]
Wind = Wind[complete.cases(Wind),]
unique(Wind$value)
coast = Wind[Wind$value == "Wind/Salt tolerant",]
coast$trait_name = "coastal_tolerance"
coast$value = "Yes"

Wind$trait_name = "wind_tolerance"
Wind$value = gsub("Wind Tolerant", "Yes", Wind$value)
Wind$value = gsub("Sheltered", "No", Wind$value)
Wind$value = gsub("Wind/Salt tolerant", "Yes", Wind$value)
Wind = Wind[!Wind$value == "Medium",]
GARv1 = rbind(GARv1[!GARv1$trait_name == "Wind",], Wind, coast)
#7. Growth
Growth = GARv1[GARv1$trait_name == "Growth",]
unique(Growth$value)
Growth$value = gsub("Very fast", "fast", Growth$value)
Growth$value = str_to_lower(Growth$value)
Growth$trait_name = "growth_rate"

GARv1 = rbind(GARv1[!GARv1$trait_name == "Growth",], Growth)
#8. Frost
Frost = GARv1[GARv1$trait_name == "Frost",]
Frost[Frost$value == "Hardy",]$value = "Yes"
Frost[Frost$value == "Tender",]$value = "No"
Frost[Frost$value == "Marginal",]$value = "Yes1"
Frost$trait_name = "frost_tolerance"
Frost[Frost$value == "Yes1",]$trait_name = "lightfrost_tolerance"
Frost[Frost$trait_name == "lightfrost_tolerance",]$value = "Yes"

GARv1 = rbind(GARv1[!GARv1$trait_name == "Frost",], Frost)
#9. Evergreen
EG = GARv1[GARv1$trait_name == "Evergreen",]
EG[EG$value == "Yes",]$value = "evergreen"
EG$trait_name = "leaf_loss"
EG = EG[!EG$value == "No",]
GARv1 = rbind(GARv1[!GARv1$trait_name == "Evergreen",], EG)
#10. Native
Native = GARv1[GARv1$trait_name == "Native",]
Native = Native[!Native$value == "No",]
Native$trait_name = "country_of_origin"
Native$value = "Australia"
GARv1 = rbind(GARv1[!GARv1$trait_name == "Native",], Native)
#11. Height
GARv1[GARv1$trait_name == "Height",]$trait_name = "height" 

#12. Width
GARv1[GARv1$trait_name == "Width",]$trait_name = "width" 
# Plenty of zeros which is annoying. I've deleted all of them . I hope the other data are ok
GARv1 = GARv1[!GARv1$value == "0",]
#13. Position
Pos = GARv1[GARv1$trait_name == "Position",]
Pos = separate(Pos, col = value, into = c("common_name", "common_name1", "common_name2", "common_name3", "common_name4", "common_name5", "common_name6", "common_name7","common_name8","common_name9"), sep = ", ")
Pos = Pos %>% gather(key = "trait_name", value = "value", common_name:common_name9, na.rm = T)
Pos$value = str_to_lower(Pos$value)
unique(Pos$value)

Pos[Pos$value == "feature",]$trait_name = "purpose"

Pos[Pos$value == "border",]$trait_name = "purpose"

Pos$value = gsub("feature", "garden", Pos$value)
Pos[Pos$value == "garden",]$trait_name = "placement"

Pos[Pos$value == "screen",]$trait_name = "purpose"

Pos = Pos[!Pos$value == "other",]
Pos = Pos[!Pos$value == "rockery",]

Pos$value = gsub("hedge", "hedging_possible", Pos$value)
Pos[Pos$value == "hedging_possible",]$trait_name = "habit"

Pos$value = gsub("ground cover", "groundcover", Pos$value)
Pos[Pos$value == "groundcover",]$trait_name = "purpose"

Pos$value = gsub("shrubbery", "garden", Pos$value)
Pos[Pos$value == "garden",]$trait_name = "placement"

Pos$value = gsub("trellis", "climber", Pos$value)
Pos[Pos$value == "climber",]$trait_name = "form"

Pos$value = gsub("pots/tubs", "potplant", Pos$value)
Pos[Pos$value == "potplant",]$trait_name = "placement"

Pos = Pos[!Pos$value == "glasshouse",]

Pos[Pos$value == "indoor",]$trait_name = "placement"

Pos$value = gsub("patio", "balcony", Pos$value)
Pos[Pos$value == "balcony",]$trait_name = "placement"

Pos$value = gsub("vegetable garden", "garden", Pos$value)
Pos[Pos$value == "garden",]$trait_name = "placement"

Pos$value = gsub("hanging basket", "potplant", Pos$value)
Pos[Pos$value == "potplant",]$trait_name = "placement"

Pos$value = gsub("under trees", "partshade", Pos$value)
Pos[Pos$value == "partshade",]$trait_name = "light_level"
unique(Pos$trait_name)
GARv1 = rbind(GARv1[!GARv1$trait_name == "Position",], Pos)
#14. RETAIL
#leave this
#15. Flower Colour
FC = GARv1[GARv1$trait_name == "Flower Colour",]
FC = separate(FC, col = value, into = c("common_name", "common_name1", "common_name2", "common_name3", "common_name4", "common_name5", "common_name6", "common_name7","common_name8","common_name9"), sep = ", ")
FC = FC %>% gather(key = "trait_name", value = "value", common_name:common_name9, na.rm = T)
FC$value = str_to_lower(FC$value)
unique(FC$value)
FC$trait_name = "flower_colour"
GARv1 = rbind(GARv1[!GARv1$trait_name == "Flower Colour",], FC)
#16. Flowering Time
FT = GARv1[GARv1$trait_name == "Flowering Time",]
FT = separate(FT, col = value, into = c("common_name", "common_name1", "common_name2", "common_name3", "common_name4", "common_name5", "common_name6", "common_name7","common_name8","common_name9", "common_name10", "common_name11", "common_name12"), sep = ", ")
FT = FT %>% gather(key = "trait_name", value = "value", common_name:common_name12, na.rm = T)

unique(FT$value)
for (i in 1:length(FT$value)){
if (FT$value[i] == "December"|FT$value[i] == "January"|FT$value[i] == "February"){
  FT$value[i] = "Summer"
}
if (FT$value[i] == "March"|FT$value[i] == "April"|FT$value[i] == "May"){
  FT$value[i] = "Autumn"
}
if (FT$value[i] == "June"|FT$value[i] == "July"|FT$value[i] == "August"){
  FT$value[i] = "Winter"
}
if (FT$value[i] == "September"|FT$value[i] == "October"|FT$value[i] == "November"){
  FT$value[i] = "Spring"
}
}
FT = unique(FT)
FT$trait_name = "flower_period"
GARv1 = rbind(GARv1[!GARv1$trait_name == "Flowering Time",], FT)
#17. Climate Zone
#18. description
#19. Soil
#20. maintenance
#21. diseases

#Leaving these paragraph traits. 

GARv1$value = str_trim(GARv1$value)
GARv1$species = str_trim(GARv1$species)
GARv1 = unique(GARv1)
# -6000 observation
unique(GARv1$trait_name)
write.csv(GARv1, "./Scraped data/Gardensonline/GARv2.csv", row.names = F)

