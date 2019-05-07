

library(rvest)
library(stringr)

URL = "http://www.pollenlibrary.com"

list_of_search_pages = str_c(URL, "/SEARCH/LETTER/", LETTERS)
list_of_search_pages = list_of_search_pages[-c(24,26)]
plant_url_all = character()

for (i in list_of_search_pages){
  
  
  
  webpage = read_html(i)
  
  
  plant_url_html = html_nodes(webpage, "#ctl00_main_lib_content_genusSpeciesList a")
  plant_url = html_attr(plant_url_html, name = "href")
  plant_url = str_c(URL, plant_url)
  plant_url_all = append(plant_url_all, plant_url)
  
}

df = data.frame(species = character(), trait_name = character(), value = character(), stringsAsFactors = F)

for (i in plant_url_all){
  
  
  webpage = read_html(i)
  
  species = html_nodes(webpage, "#ctl00_main_lib_content_breadcrumbs b")
  species = html_text(species)
  
  aller = html_nodes(webpage, "#ctl00_main_lib_content_divAllergenicity span")
  aller = html_text(aller)

 if (grepl("No allergy has been reported", aller) == T){
   aller = NA
 }else if (grepl("mild allergen", aller) == T){
    aller = "mildallergen"
 }else if (grepl("moderate allergen", aller) == T){
    aller = "moderateallergen"
 }else if (grepl("severe allergen", aller) == T){
  aller = "severeallergen"
 }else{
  aller = "check"
 }

  if (is.na(aller) == F){
    temp = data.frame(species = species, trait_name = "risk", value = aller, stringsAsFactors = F)
    df = rbind(df, temp)
  }
  
}  
df = pollenlibrary
pol = data.frame(List_source = "Scrape", date_sourced = "4/04/2019", species = df$species, newspecies = df$species, species_number = "", 
                 multiple_forms = "", study = "pollenlibrary", trait_index = "47",  trait_name_original = df$trait_name, trait_name = df$trait_name, value_original = df$value, 
                 value = df$value, stringsAsFactors = F)
write.csv(pol, "pollenlibrary.csv")
length(which(pol$species %in% unique(df1$newspecies)))
length(unique(pol$species))
pol1 = pol[which(pol$species %in% unique(df1$newspecies)),]
