
library(rvest)


library(stringr)
# get the homepage url
url = "https://arboretum.ucdavis.edu/plant-database"

# find the pattern for the search pages and paste the sequences together to make a string of urls
list_of_search_pages = str_c(url, "?field_common_name_value=&field_latin_name_value=&field_california_native_value_1=All&field_type_value=All&field_size_value=All&field_color_value=All&page=", 0:20)
# check
# str(list_of_search_pages)
# 
# first_page <- read_html(list_of_search_pages[7])
# 
# the_link_to_click_on <- html_nodes(first_page, "a")
# 
# list_of_individual_pages = html_attr(the_link_to_click_on, name = "href")
# 
# plant_url = list_of_individual_pages[which(regexpr("/plant/", list_of_individual_pages) >= 1)]
# plant_url = iconv(plant_url, "UTF-8", "latin1")

# Step 5. Multiply this code to all the search page urls to get a list of individual plant page urls





# Now make the list of individual plants urls

plant_url_all = character()

for (i in list_of_search_pages){
  
  webpage = read_html(i)
  
  plant_url_html = html_nodes(webpage, "a")
  plant_url = html_attr(plant_url_html, name = "href")
  
  plant_url = plant_url[which(regexpr("/plant/", plant_url) >= 1)]
  
  # doesn't seem to recognise this function, some kind of bug..
  # plant_url = iconv(plant_url, "UTF-8", "ASCII")

  # Extra steps compared to Benara

  plant_url = unique(plant_url)
  
  plant_url = str_c("https://arboretum.ucdavis.edu", plant_url)
  
  # plant_url = gsub("%E2%80%99", "'", plant_url)
  
  # plant_url = gsub("%C3%BC", "u", plant_url)
  
  plant_url_all = append(plant_url_all, plant_url)
  
}
  
df = data.frame(species = character(), Index = numeric(), value = character(), stringsAsFactors = F)

for (i in plant_url_all){
  webpage = read_html(i)
 # too hard to match the labels with the data. Do this after the data is downloaded, by matching the strinbg to the heading
  
  data_html = html_nodes(webpage, "#block-sf1subtheme-mainpagecontent .field__item")
  data = html_text(data_html)
  data = data[-1]
  data = gsub("\n\n","",data)
  data = str_trim(data)
  

  # create a temporary dataframe
  dfprep = data.frame(species = character(length = length(data)), Index = numeric(length = length(data)),
                      value = data)
  #fill it with the species and the data
  
  dfprep[,1] = data[2]
  dfprep[,2] = rownames(dfprep)
  dfprep[,3] = data
  
  df = rbind(df, dfprep)
  
  Sys.sleep(5) 
}

# Now add in the data titles manually
df$Index = as.numeric(df$Index)
df$trait_name = character(length = length(df[,1]))

# Size avriable had to have two conditions, as medium was used in two different categories
df$trait_name[which(df$value == "Small" | df$value == "Large" )] = "Size"
df$trait_name[which(df$Index <= 9 & df$value == "Medium")] = "Size"

# Easy categories
df$trait_name[which(df$value == "Full Sun" | df$value == "Part Shade" | df$value == "Full Shade" | df$value == "Shade")] = "Sun Exposure"
df$trait_name[which(df$Index >= 9 & df$value == "Very Low" | df$value == "Low" |df$value == "Medium" | df$value == "High")] = "Water needs" 
df$trait_name[which(df$value == "Winter" | df$value == "Spring" | df$value == "Summer" | df$value == "Fall")] = "Flowering season"
df$trait_name[which(df$value == "Hummingbirds" | df$value == "Butterflies"| df$value == "Beneficial Insects" | df$value == "None" | df$value == "Bees" | df$value == "Birds")] = "Wildlife Value(s)"
# The pruning category
df$trait_name[str_which(df$value, regex("little or none", ignore_case = TRUE))] = "Pruning needs"
df$trait_name[str_which(df$value, regex("remove ", ignore_case = TRUE))] = "Pruning needs"
df$trait_name[str_which(df$value, regex("prune ", ignore_case = TRUE))] = "Pruning needs"
df$trait_name[str_which(df$value, regex("cut ", ignore_case = TRUE))] = "Pruning needs"
df$trait_name[str_which(df$value, regex("mow ", ignore_case = TRUE))] = "Pruning needs"
# Take out the location data
df = df[-str_which(df$value, regex("Garden", ignore_case = TRUE)),]
df = df[-str_which(df$value, regex("Nursery", ignore_case = TRUE)),]
df = df[-str_which(df$value, regex("Collection", ignore_case = TRUE)),]
# do these last incase any of these contained pruning words or location words
df$trait_name[which(df$Index == 1)] = "Common name"
df$trait_name[which(df$Index == 2)] = "Latin name"
df$trait_name[which(df$Index == 3)] = "Description"
df$trait_name[which(df$Index == 4)] = "California native"
df$trait_name[which(df$Index == 5)] = "All-Star"
df$trait_name[which(df$Index == 6)] = "Type"
# all the rest are flower colours. In retrospect, this could have been a simple categorical variable.. 
df$trait_name[which(df$trait_name == "")] = "Flower colour"

df$study = "UCdavis"

df = select(df, study, species, trait_name, value)

write.csv(df, "UCdavisraw.csv", row.names = F)

