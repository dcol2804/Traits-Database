#set the working directory to scraped data and import UCDv1.csv
UCDv1 = read.csv("./Scraped data/UCDavis/UCdavisraw.csv", stringsAsFactors = F)
unique(UCDv1$species)
x2 = str_match_all(UCDv1$species, "[[:alnum:]|[:blank:]|[:punct:]]" )

x2[lengths(x2) == 0] <- NA_character_



z = character()

for (i in 1:length(UCDv1$species) ){
  y = str_c(x2[[i]], collapse = "")
  
  z =  append(z, y)
}

UCDv1$species = z

UCDv1$species = gsub("U[0-9]{4}", "", UCDv1$species)
############### Needed if chinese characters appear
# x2 = str_match_all(z, "[a-zA-Z]|[:blank:]")
# 
# z = character()
# for (i in 1:2736 ){
#   y = str_c(x2[[i]], collapse = "")
#   
#   z =  append(z, y)
# }
x2 = str_match_all(UCDv1$value, "[[:alnum:]|[:blank:]|[:punct:]]" )

x2[lengths(x2) == 0] <- NA_character_



z = character()

for (i in 1:length(UCDv1$value) ){
  y = str_c(x2[[i]], collapse = "")
  
  z =  append(z, y)
}

UCDv1$value = z
UCDv1$value = gsub("U[0-9]{4}", "", UCDv1$value)
UCDv1$value = gsub("U[0-9]{2}AE", "", UCDv1$value)
# Now fix unique bad names
UCDv1$species = gsub(" [Ss]yn.*", "", UCDv1$species)

UCDv1$trait_name_original = UCDv1$trait_name
UCDv1$value_original = UCDv1$value

# UCDv1$species = gsub(" [Xx] ", " ", UCDv1$species)
# 
# UCDv1$species = gsub(" hybrids and cultivars", "sp", UCDv1$species)
# 
# UCDv1$species = gsub(" subsp.", "", UCDv1$species)
# 
# UCDv1$species = gsub("\\(dwarf varieties\\)", "dwarf", UCDv1$species)
# 
# UCDv1$species = gsub(" Margarita B.O.P.", "", UCDv1$species)
# 
# UCDv1$species = gsub(" and.*", "", UCDv1$species)
# 
# UCDv1$species = gsub(" (Feijoa sellowiana)", "", UCDv1$species)
# 
# UCDv1$species = gsub("[[:punct:]]", "", UCDv1$species)
# UCDv1$species = gsub(" var ", " ", UCDv1$species)
UCDv1$species = gsub("\\s+ ", " ", UCDv1$species)
unique(UCDv1$species)
# that will do for now
fixthis = UCDv1[UCDv1$species == "christmas berry",]
UCDv1 = UCDv1[!UCDv1$species == "christmas berry",]
unique(UCDv1$trait_name)

# 1. Common name
cname = UCDv1[UCDv1$trait_name == "Common name",]
cname$trait_name = "common_name"

UCDv1 = rbind(UCDv1[!UCDv1$trait_name == "Common name",], cname)
# 2. Latin name
UCDv1 = UCDv1[!UCDv1$trait_name == "Latin name",]
# 3. California native
UCDv1 = UCDv1[!UCDv1$trait_name == "California native",]
# 4. All-star
UCDv1 = UCDv1[!UCDv1$trait_name == "All-Star",]
# 5. Type
form = UCDv1[UCDv1$trait_name == "Type",]
form$trait_name = "form"
form$value = str_to_lower(form$value)
form[form$value == "groundcover",]$trait_name = "purpose"
form[form$value == "vine",]$value = "climber"
unique(form$value)
UCDv1 = rbind(UCDv1[!UCDv1$trait_name == "Type",], form)
# 6. Size
size = UCDv1[UCDv1$trait_name == "Size",]
size$trait_name = "form"
size[size$value == "Small",]$value = "smallshrub"
size = size[!size$value == "Large",]
UCDv1 = rbind(UCDv1[!UCDv1$trait_name == "Size",], size)
# 7. Flower colour
FC = UCDv1[UCDv1$trait_name == "Flower colour",]

FC$value = str_to_lower(FC$value)
FC = FC[-2,]
FC$trait_name = "flower_colour"
UCDv1 = rbind(UCDv1[!UCDv1$trait_name == "Flower colour",], FC)
# 8. Sun exposure
Sun = UCDv1[UCDv1$trait_name == "Sun Exposure",]
Sun$value = str_to_lower(Sun$value)
Sun$value = gsub(" ", "", Sun$value)
Sun$trait_name = "light_level"
Sun$value[Sun$value == "shade"] = "fullshade"
UCDv1 = rbind(UCDv1[!UCDv1$trait_name == "Sun Exposure",], Sun)
# 9. Water needs
supp = UCDv1[UCDv1$trait_name == "Water needs",]
supp$value = str_to_lower(supp$value)
supp$value[supp$value == "very low"] = "none"
supp$trait_name = "supp_watering"
UCDv1 = rbind(UCDv1[!UCDv1$trait_name == "Water needs",], supp)
# 10. Flowering season
FS = UCDv1[UCDv1$trait_name == "Flowering season",]

FS$value[FS$value == "Fall"] = "Autumn"
FS$trait_name = "flower_period"
unique(FS$value)

UCDv1 = rbind(UCDv1[!UCDv1$trait_name == "Flowering season",], FS)
# 11. Wildlife value(s)
Wild = UCDv1[UCDv1$trait_name == "Wildlife Value(s)",]
#I don't think I can use these.. 
# 12. Pruning needs
PN = UCDv1[UCDv1$trait_name == "Pruning needs",]
# too verbal for now

UCDv1 = unique(UCDv1)
# 13. Description
write.csv(UCDv1, "./Scraped data/UCDavis/UCDv2.csv", row.names = F)
