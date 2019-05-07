library(rvest)
library(stringr)
library(stringr)
url = "http://actsmart-plantselector.com.au/browse-plants/?common_name=&botanical_name"

list_of_search_pages = str_c(url, "=&pa=", 1:57)

df = data.frame(species = character(), Common_name = character(), trait_name = character(), value = character(), stringsAsFactors = F)

for (i in list_of_search_pages){
  test_page = read_html(i)

botanical_name_html = html_nodes(test_page, ".botanical_name")
botanical_name = html_text(botanical_name_html)

common_name_html = html_nodes(test_page, ".common_name")
common_name = html_text(common_name_html)
common_namedf = data.frame( species = botanical_name, Common_name = common_name, trait_name = "common_name", value = common_name, stringsAsFactors = F)

plant_type_html = html_nodes(test_page, ".plant_type")
plant_type = html_text(plant_type_html)
plant_type = gsub( "\n\t\t\t\t\tPlant Type\n\t\t\t\t\t", "", plant_type[2:7])
plant_type = gsub("\t\t\t\t", "", plant_type)
plant_type = data.frame( species = botanical_name, Common_name = common_name, trait_name = "plant_type", value = plant_type, stringsAsFactors = F)

plant_origin_html = html_nodes(test_page, ".plant_origin")
plant_origin = html_text(plant_origin_html)
plant_origin = gsub( "\n\t\t\t\t\tPlant Origin\n\t\t\t\t\t", "", plant_origin)
plant_origin = gsub("\t\t\t\t", "", plant_origin)
plant_origin = data.frame( species = botanical_name, Common_name = common_name, trait_name = "plant_origin", value = plant_origin, stringsAsFactors = F)

light_requirement_html = html_nodes(test_page, ".sun_shade")
light_requirement = html_text(light_requirement_html)
light_requirement = gsub( "\n\t\t\t\t\tSun Shade\n\t\t\t\t\t", "", light_requirement[2:7])
light_requirement = gsub("\t\t\t\t", "", light_requirement)
light_requirement = data.frame( species = botanical_name, Common_name = common_name, trait_name = "light_requirement", value = light_requirement, stringsAsFactors = F)


frost_tolerance_html = html_nodes(test_page, ".frost_tolerance")
frost_tolerance = html_text(frost_tolerance_html)
frost_tolerance = gsub( "\n\t\t\t\t\tFrost Tolerance\n\t\t\t\t\t", "", frost_tolerance[2:7])
frost_tolerance = gsub("\t\t\t\t", "", frost_tolerance)
frost_tolerance = data.frame( species = botanical_name, Common_name = common_name, trait_name = "frost_tolerance", value = frost_tolerance, stringsAsFactors = F)

fire_retardant_html = html_nodes(test_page, ".fire_retardant")
fire_retardant = html_text(fire_retardant_html)
fire_retardant = gsub( "\n\t\t\t\t\tFire Retardant\n\t\t\t\t\t", "", fire_retardant)
fire_retardant = gsub("\t\t\t\t", "", fire_retardant)
fire_retardant = data.frame( species = botanical_name, Common_name = common_name, trait_name = "fire_retardant", value = fire_retardant, stringsAsFactors = F)

watering_requirement_html = html_nodes(test_page, ".watering_requirement")
watering_requirement = html_text(watering_requirement_html)
watering_requirement = gsub( "\n\t\t\t\t\tWatering Requirement\n\t\t\t\t\t", "", watering_requirement[2:7])
watering_requirement = gsub("\t\t\t\t", "", watering_requirement)
watering_requirement = data.frame( species = botanical_name, Common_name = common_name, trait_name = "watering_requirement", value = watering_requirement, stringsAsFactors = F)

description_html = html_nodes(test_page, "#plants p")
description = html_text(description_html)
description = description[1:6]
description = data.frame(species = botanical_name, Common_name = common_name, trait_name = "Description", value = description, stringsAsFactors = F)

tempdf = rbind(common_namedf, description,plant_type, plant_origin, light_requirement, frost_tolerance, fire_retardant, watering_requirement)

df = rbind(df, tempdf)

}
 df$species = str_trim(df$species)
 
 for (i in 1:length(df$species)){
 if (str_detect(df$Common_name[i], "'") == T){
   df$species[i] = str_c(df$species[i], " ", df$Common_name[i], collapse = NULL)
 }
 }

  df = select(df, - Common_name)
  df$study = "ACTps"
 df = select(df, study, species, trait_name, value)
 write.csv(df, "ACTplantselectorraw.csv", row.names = F)
