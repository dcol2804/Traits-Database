
library(stringr)
library(rvest)
library(BBmisc)

Url = "http://rp.arriyadhenv.gov.sa/"


list_of_possible_urls = str_c(Url, "plant/", 373:800, "/?lang=en")
# Like plant selector plus, generate the possible plant list

plant_url_all = character()

# Check if they are real websites or not

for (i in list_of_possible_urls){
  
  tryCatch({
    if (is.error(read_html(i)) == F)
      plant_url_all = append(plant_url_all, i)
    
    if (is.error(read_html(i))!= F) stop()
  }, error=function(e){})
}
write.csv(plant_url_all, "RiyadhPlanturls.csv", row.names = F)
############# URLS are ready

df = data.frame(species = character(), trait_name = character(), value = character(), stringsAsFactors = F)

for (i in plant_url_all){
  webpage = read_html(i)
  
  species_html = html_nodes(webpage, ".col-xs-12+ .col-xs-12 .dv")
  species_cname = html_text(species_html)
  # deal with species and family in the first bit
  species_family = unlist(strsplit(species_cname[1], " , "))
  species = str_trim(species_family[1])
  family = gsub("\t", "", species_family[2])
  
  common_name = gsub(",.*", "",species_cname[2])
  
  description_html = html_nodes(webpage, "p")
  description = html_text(description_html)
  description = str_trim(description)
  
  # incase there are some blank strings infront of p
  if (str_length(description[1]) > 1){
    description = description[1]
  }else if (str_length(description[2])> 1){
    description = description[2]
  }else if (str_length(description[3])> 1){
    description = description[3]
  }
  # added in a clause for cases where no description exists
  
  headings_html = html_nodes(webpage, ".det-items span")
  headings = html_text(headings_html)
  
  headings2_html = html_nodes(webpage, ".row-color:nth-child(7) h3 , .row-color:nth-child(6) h3")
  headings2 = html_text(headings2_html)
  
  headings = append(headings, headings2)
  headings = gsub(":", "", headings)
  headings = str_trim(headings)
  
  headings2_html = html_nodes(webpage, ".row-color:nth-child(7) h3 , .row-color:nth-child(6) h3")
  headings2 = html_text(headings2_html)
  # 
  # if (headings[length(headings)] == "Indigenous to the Adelaide Region"){
  #   length(headings) = length(headings) -1
  # }else{
  #   headings = headings
  # }
  
  
  data_html = html_nodes(webpage, ".det-items")
  data = html_text(data_html)
  
  data = gsub("\n", "", data)
  data = gsub("\t", "", data)
  data1 = gsub(".*: ", "",data)
  data1 = str_trim(data1)
  data1 = str_squish(data1)
  

  dfprep = data.frame(species = character(length = length(headings)+1), 
                      trait_name = character(length = length(headings)+1), 
                      value = character(length = length(headings)+1))
  
  # assemble into df
  
  dfprep[,1] = as.character(species)
  dfprep[,2] = c(headings, "description")
  dfprep[,3] = c(data1, description)
  
  df = rbind(df, dfprep)
}
df$study = "Riyadh"
library(dplyr)
df1 = df %>% select(study, species, trait_name, value)

write.csv(df, "Riyadhraw.csv")