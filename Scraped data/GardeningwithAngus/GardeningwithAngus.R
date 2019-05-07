install.packages("rvest")
install.packages("stringr")
library(rvest)
library(stringr)
# Save the starting URL page
URL = "https://www.gardeningwithangus.com.au/browse-by-botanic-name/"
# read in the URL
webpage = read_html(URL)
# find the CSS code item of the links to each page you would like to "click on" to open up each genus.
# this can be done fairly quickly by using the Selectorgadget app on Google Chrome. 
plant_url_html = html_nodes(webpage, ".browse-item")
plant_url = html_attr(plant_url_html, name = "value")


# glue together the elements of the URL to create each genera URL search page
list_of_search_pages = str_c(URL, "?botanic_name=", plant_url)
  
plant_url_all = character()
# now open each genera page and extract the individual plant pages using a loop.
for (i in list_of_search_pages){
  
  
  
  webpage = read_html(i)
  
  
  plant_url_html = html_nodes(webpage, ".entry-title a")
  plant_url = html_attr(plant_url_html, name = "href")
  
  plant_url_all = append(plant_url_all, plant_url)
  # Sys.sleep(60)
}
# plant_url_all now contains a list of every plant factsheet webpage on the whole site. 
# create a blank dataframe and fill it with the elements on each plant factsheet p[age. 
# You'll have to use the Selectorgadget to find the CSS code for the elements you'd like to download
df = data.frame(Species = character(), trait_name = character(), value = character(), stringsAsFactors = F)

for (i in plant_url_all){
  
  
  webpage = read_html(i)



description_html = html_nodes(webpage, ".entry-content p")
description = html_text(description_html)

# these are some contingencies because the plant description downloaded slightly differently, depending on the location of the photos on the webpage
if (str_length(description[1]) > 1){
  description = description[1]
}else if (str_length(description[2])> 1){
  description = description[2]
}else if (str_length(description[3])> 1){
  description = description[3]
}

headings_html = html_nodes(webpage, "td:nth-child(1)")
headings = html_text(headings_html)
headings = gsub(":", "", headings)

data_html = html_nodes(webpage, "td+ td")
data = html_text(data_html)
data = unlist(data)
# create a blank temporary dataframe for the current plant webpage
dfprep = data.frame(Species = character(length = length(headings)+1),
                    trait_name = character(length = length(headings)+1), 
                    value = character(length = length(headings)+1))

dfprep[,1] = as.character(data[1])
dfprep[,2] = c(headings, "description")
dfprep[,3] = c(data, description)
# attach the current plant webpage dataframe to the master dataframe. The loop will now restart on the next plant webpage
df = rbind(df, dfprep)
# Sys.sleep(60)
}

# publish the data as a csv to further clean.
write.csv(df, "Gardeningwithangusraw.csv", row.names = F)


