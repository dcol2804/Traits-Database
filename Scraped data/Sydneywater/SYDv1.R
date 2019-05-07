# set the working directory to sydney water and import SYDv1.csv
SYDv1 = read.csv("./Scraped data/Sydneywater/Sydneywaterraw.csv", stringsAsFactors = F)
#clean up the species names first
SYDv1$species = gsub(" [Ss]yn.*", "", SYDv1$species)

SYDv1$trait_name_original = SYDv1$trait_name
SYDv1$value_original = SYDv1$value

# syns = SYDv1[grepl("[Ss]yn",SYDv1$species) == T,]
# syns = syns[!syns$species == "Syncarpia glomulifera",]
# #replace all value column with the strings after the syn word
# syns$value = sub(".*[Ss]yn","",syns$species)
# #Unfort we have to extract Syncarpia glom
# Sync = SYDv1[SYDv1$species == "Syncarpia glomulifera",]
# SYDv1 = SYDv1[!SYDv1$species == "Syncarpia glomulifera",]
# #Go back to the original dataframe and replace the species with everything before the syn word
# SYDv1$species = sub("[Ss]yn.*","",SYDv1$species)
# #do the same for the syns dataframe
# syns$species = sub("[Ss]yn.*","",syns$species)
# syns$trait_name = "synonyms"
# syns = unique(syns)
# #stick them back on
# SYDv1 = rbind(SYDv1, syns, Sync)
# 
# 
# var = SYDv1[grepl(" var[. ]",SYDv1$species) == T,]
# unique(var$species)
# # get rid of the vars
# SYDv1$species = gsub(" var ", " ", SYDv1$species)
# # get rid of the " x "
# SYDv1$species = gsub(" [Xx] ", " ", SYDv1$species)
# # find the Dwarf forms and fix
# 
# Dwarf = SYDv1[grepl("[Dd]warf",SYDv1$species) == T,]
# SYDv1$species = gsub(" [Dd]warf.", " dwarf", SYDv1$species)
# 
# #get rid of punctuation
# 
# SYDv1$species = gsub("[[:punct:]]", "", SYDv1$species)
# # subsp
# SYDv1$species = gsub(" subsp ", " ", SYDv1$species)
# SYDv1$species = gsub(" spp ", "sp", SYDv1$species)
# SYDv1$species = gsub("X ", "", SYDv1$species)
# SYDv1$species = str_trim(SYDv1$species)
# #ok that should do for now
SYDv1 = SYDv1
unique(SYDv1$trait_name)


#1. Plant type
form = SYDv1[SYDv1$trait_name == "Plant type",]
unique(form$value)
form = separate(form, col = value, into = c("generic", "generic1", "generic2", "generic3"), sep = ", ")
form = form %>% gather(key = "trait_name", value = "value", generic:generic3, na.rm = T)
form$value = str_trim(form$value)
form$value = str_to_lower(form$value)
unique(form$value)
form$trait_name = "form"
form[form$value == "ground cover",]$value = "groundcover"
form$trait_name[form$value == "groundcover"] = "purpose"
form[form$value == "turf",]$value = "grass"
form[form$value == "vegetables & herbs"|form$value == "vegetables and herbs",]$value = "edible"
form$trait_name[form$value == "edible"] = "purpose"

SYDv1 = rbind(SYDv1[!SYDv1$trait_name == "Plant type",], form)
#2. Sun/Shade tolerance
light_level = SYDv1[SYDv1$trait_name == "Sun/Shade tolerance",]
light_level = separate(light_level, col = value, into = c("generic", "generic1", "generic2", "generic3"), sep = ", ")
light_level = light_level %>% gather(key = "trait_name", value = "value", generic:generic3, na.rm = T)
light_level$value = str_trim(light_level$value)
light_level$value = str_to_lower(light_level$value)
unique(light_level$value)
light_level$value = gsub(" ", "", light_level$value)
light_level$value[light_level$value == "shade"] = "fullshade"
light_level$trait_name = "light_level"
SYDv1 = rbind(SYDv1[!SYDv1$trait_name == "Sun/Shade tolerance",], light_level)
#3.Frost tolerance
frost = SYDv1[SYDv1$trait_name == "Frost tolerance",]
frost$value[frost$value == "Frost hardy"] = "Yes"
frost$value[frost$value == "Frost sensitive"] = "No"
frost$trait_name = "frost_tolerance"

SYDv1 = rbind(SYDv1[!SYDv1$trait_name == "Frost tolerance",], frost)
#4. Soil type(s)
soil = SYDv1[SYDv1$trait_name == "Soil type(s)",]
soil = separate(soil, col = value, into = c("generic", "generic1", "generic2", "generic3"), sep = ", ")
soil = soil %>% gather(key = "trait_name", value = "value", generic:generic3, na.rm = T)
soil$value = str_trim(soil$value)
soil$value = str_to_lower(soil$value)
unique(soil$value)
soil$trait_name = "soil_type"

SYDv1 = rbind(SYDv1[!SYDv1$trait_name == "Soil type(s)",], soil)
#5. Water rating
supp_watering = SYDv1[SYDv1$trait_name == "Water rating",]
supp_watering$trait_name = "supp_watering"
supp_watering$value = str_to_lower(supp_watering$value)

SYDv1 = rbind(SYDv1[!SYDv1$trait_name == "Water rating",], supp_watering)
#6. Plant origin
OR = SYDv1[SYDv1$trait_name == "Plant origin",]
unique(OR$value)
#7. common name
SYDv1[SYDv1$trait_name == "common name",]$trait_name = "common_name"
SYDv1 = SYDv1[complete.cases(SYDv1),]
unique(SYDv1$trait_name)
#That was quick

write.csv(SYDv1, "./Scraped data/Sydneywater/SYDv2.csv", row.names = F)
