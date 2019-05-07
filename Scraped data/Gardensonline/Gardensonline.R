library(rvest)
library(stringr)

list_of_searchpages = str_c("https://www.gardensonline.com.au/GardenShed/PlantFinder/PageNumber_", 1:38, ".aspx")

plant_url_all = character()

for (i in list_of_searchpages){

  webpage = read_html()

  plant_url_html = html_nodes(webpage, "a")
plant_url = html_attr(plant_url_html, name = "href")
plant_url = plant_url[(str_which(plant_url, "^(Show_)[{0-9}2.*]"))]
plant_url = unique(plant_url)
plant_url = str_c("https://www.gardensonline.com.au/GardenShed/PlantFinder/", plant_url)
plant_url_all = append(plant_url_all, plant_url)

}
# Dud file

plant_url_all = plant_url_all[!c(str_which(plant_url_all, "4062"), str_which(plant_url_all, "4233"))]

df = data.frame(species = character(), trait_name = character(), value = character(), stringsAsFactors = F)

for (i in plant_url_all){
  webpage = read_html(i)
  
  species_html = html_nodes(webpage, "h1")
  species = html_text(species_html)
  species = species[1]
  
  common_names_html = html_nodes(webpage, ".PanelPlantLeft .PanelPlantNames p")
  common_names = html_text(common_names_html)
  common_names = str_c(common_names, collapse = ", ")
  if (length(common_names) == 0 ){
    common_names = NA
  }
  common_names = data.frame(trait_name = "common_name", value = common_names, stringsAsFactors = F)
  
  headings1_html = html_nodes(webpage, ".PanelPlantCriteria strong")
  headings1 = html_text(data_html)
  headings1 = gsub(":", "", headings1)
  
  data1_html = html_nodes(webpage, ".PanelPlantCriteria span")
  data1 = html_text(data1_html)
  
  data1df = data.frame(trait_name = headings1, value = data1, stringsAsFactors = F)
  
  headings2_html = html_nodes(webpage, ".PanelPlantCriteria h3")
  headings2 = html_text(headings2_html)
  
  data2_html = html_nodes(webpage, "h4+ p , .PanelPlantCriteria h3+ p")
  data2 = html_text(data2_html)
  data2 = gsub("\r\n", ", ", data2)
  data2 = gsub("\t", "", data2)
  data2 = gsub("Zone: ", "", data2)
  
  data2df = data.frame(trait_name = headings2, value = data2, stringsAsFactors = F)
  
  description_html = html_nodes(webpage, "#ctl00_PageBodyContentTop_PanelShowSummary p")
  description = html_text(description_html)
  description = gsub("\r", "", description)
  description = str_trim(description)
  description = str_c(description, collapse = "")
  
  soil_html = html_nodes(webpage, "#ctl00_PageBodyContentTop_PanelShowSoilOuter+ p , #ctl00_PageBodyContentTop_PanelShowSoil")
  soil = html_text(soil_html)
  soil = str_c(soil, collapse = "")
  if (length(soil) == 0 ){
    soil = NA
  }
  maintenance_html = html_nodes(webpage, "#ctl00_PageBodyContentTop_PanelShowMaintenanceOuter+ p , #ctl00_PageBodyContentTop_PanelShowMaintenance")
  maintenance = html_text(maintenance_html)
  maintenance = str_c(maintenance, collapse = "")
  if (length(maintenance) == 0 ){
    maintenance = NA
  }
  
  diseases_html = html_nodes(webpage, "#ctl00_PageBodyContentTop_PanelShowDiseases + p , #ctl00_PageBodyContentTop_PanelShowDiseases")
  diseases = html_text(diseases_html)
  if (length(diseases) == 0 ){
    diseases = NA
  }
  
  descriptionsdf = data.frame(trait_name = c("description", "soil", "maintenance", "diseases"), value = c(description, maintenance, soil, diseases), stringsAsFactors = F)
  
  tempdf = data.frame(species = species, rbind(common_names, data1df, data2df, descriptionsdf))

  df = rbind(df, tempdf)
}
df$study = "Gardensonline"
df = select(df, study, species, trait_name, value)

write.csv(df, "gardensonlineraw.csv", row.names = F)
