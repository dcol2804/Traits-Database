library(rvest)
library(stringr)

Url = "https://www.sydneywater.com.au/SW/your-home/saving-water-at-home/garden-and-pool/plant-selector/index.htm?Row=1&actsuburb=&su=&wn=&cn=&bn=&SortField=dDocTitle&ResultCount=24"

list_of_search_pages = str_c("https://www.sydneywater.com.au/SW/your-home/saving-water-at-home/garden-and-pool/plant-selector/index.htm?Row=",1:42,"&actsuburb=&su=&wn=&cn=&bn=&SortField=dDocTitle&ResultCount=24")


plant_url_all = character()

for (i in list_of_search_pages){
  

  
  webpage = read_html(i)
  
  
  plant_url_html = html_nodes(webpage, "a")
  plant_url = html_attr(plant_url_html, name = "href")
  
  # plant_url = plant_url[grep("(.*[0-9]{6})", plant_url)]
  plant_url = plant_url[grep("(SWP_CDF)", plant_url)]
  plant_url = str_c("https://www.sydneywater.com.au", plant_url)
  
  
  
  plant_url_all = append(plant_url_all, plant_url)
  Sys.sleep(3)
}





# Data: #main-content .col-md-8

df = data.frame(Species = character(), trait_name = character(), value = character(), stringsAsFactors = F)

for (i in plant_url_all){
  webpage = read_html(i)
  
  species_html = html_nodes(webpage, "h2")
  species = html_text(species_html)
  species = sub(" - botanical name", "", species)
  
  common_name_html = html_nodes(webpage, "#main-content h1")
  common_name = html_text(common_name_html)
  
  
  description_html = html_nodes(webpage, "p")
  description = html_text(description_html)
  
  if (length(description) == 4){
  description = description[1]
  }else{
    description = description[2]
  }
  
  # added in a clause for cases where no description exists
  
  headings_html = html_nodes(webpage, "#main-content .col-md-4")
  headings = html_text(headings_html)
  headings = gsub("\n", "", headings)
  headings = str_trim(headings)
  headings = headings[1:6]
  
  data_html = html_nodes(webpage, "#main-content .col-md-8")
  data = html_text(data_html)
  data = gsub("\n", "", data)
  data = str_trim(data)
  data = data[1:6]
  
  # create a temporary dataframe for each species
  
  dfprep = data.frame(Species = character(length = length(headings)+2), 
                      trait_name = character(length = length(headings)+2), 
                      value = character(length = length(headings)+2))
  
  # assemble into df
  
  dfprep[,1] = as.character(species)
  dfprep[,2] = c(headings, "common name", "description")
  dfprep[,3] = c(data, common_name, description)
  
  df = rbind(df, dfprep)
}
df$study = "Sydney_water"

df = data.frame(study = df$study, species = df$Species, trait_name = df$trait_name, value = df$value, stringsAsFactors = F)

write.csv(df, "sydneywaterraw.csv", row.names =  F)
