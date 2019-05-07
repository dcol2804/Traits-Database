# OK I can do this
install.packages("rvest")
library(rvest)

install.packages("stringr")
library(stringr)


# Get a list of the search page urls

# Step 1. Find the total number of plant species/files

p = 674

# Step 2. Find the total number of species per pages

q = 36

# Step 3. Find the search page url code addon. This can be done using CSS tracker app
# or common sense by looking at the first two or 3 search results pages. 
# This will be pasted on to the base web address


url = "https://www.benaranurseries.com/plants"
# check the page works
first_page = read_html(url)

list_of_search_pages = str_c(url, "?p=", 1:(p%/%q + 1))


str(list_of_search_pages)


# Get a list of the urls on search page 1

# Step 4. Test out the function if required. You will need the CSS code for the links that you want to open on each search page
# Then you will extract the "href" piece of information (the URL) from each of the links e.g.

# first_page <- read_html(url)

# the_link_to_click_on <- html_nodes(first_page, ".product-image")

# list_of_individual_pages = html_attr(the_link_to_click_on, name = "href")

# Step 5. Multiply this code to all the search page urls to get a list of individual plant page urls


plant_url_all = character()

for (i in list_of_search_pages){
  
  webpage = read_html(i)
  
  plant_url_html = html_nodes(webpage, ".product-image")
  plant_url = html_attr(plant_url_html, name = "href")
  
  plant_url_all = append(plant_url_all, plant_url)
  
}

#Step 5 

# Now that you have all the URLs for all the plants, you can decide what you want to scrape from each page and put it into a function.

# Plant species

# species_html = html_nodes(webpage, ".product-name")
# species = html_text(species_html)
# species

# Plant description

# webpage = read_html(plant_url_all[1])
# description_html = html_nodes(webpage, ".box-description .std")
# description = html_text(description_html)
# description

# Step 6 Try to get the data as clean as possible. This involves both choosing the most exact CSS
# And getting rid of unwanted characters/structures surrounding the data

# description = gsub("\r\n","",description)
# description = gsub("\r\n\r\n","",description)
# description = str_trim(description)
# description

#Step 7. Continue with other data

# headings

# headings_html = html_nodes(webpage, ".label")
# headings = html_text(headings_html)
# headings

# data

# data_html = html_nodes(webpage, ".data")
# data = html_text(data_html)
# data

# Step 8. You are ready to write the function and combine the data into a table

df = data.frame(species = character(), trait_name = character(), value = character(), stringsAsFactors = F)

for (i in plant_url_all){
  webpage = read_html(i)
  
 
  
  description_html = html_nodes(webpage, ".box-description .std")
  description = html_text(description_html)
  description = gsub("\r\n","",description)
  description = gsub("\r\n\r\n","",description)
  description = str_trim(description)
  description = unlist(description)
  
  # added in a clause for cases where no description exists
  
  if (length(description) == 0 ){
    description = NA
  }
  
  headings_html = html_nodes(webpage, ".label")
  headings = html_text(headings_html)
  headings = unlist(headings)
  
  data_html = html_nodes(webpage, ".data")
  data = html_text(data_html)
  data = unlist(data)
  
  species_html = html_nodes(webpage, ".product-name")
  species = html_text(species_html)
  
      
  # create a temporary dataframe for each species
  
  dfprep = data.frame(species = character(length = length(headings)+1), 
                      trait_name = character(length = length(headings)+1), 
                      value = character(length = length(headings)+1))
  
  # assemble into df
  
  
  dfprep[,2] = c(headings, "description")
  dfprep[,3] = c(data, description)
  if (length(dfprep$value[dfprep$trait_name == "Botanical species"]) == 0)
    species = species[1]
  else if
    (dfprep$value[dfprep$trait_name == "Botanical species"] == "x")
    species = species[1]
  else
    species = str_c(dfprep$value[dfprep$trait_name =="Botanical Genus"], " ", dfprep$value[dfprep$trait_name =="Botanical species"])
  
  
  dfprep[,1] = as.character(species)
  df = rbind(df, dfprep, stringsAsFactors = F)
}

df$study = "Benaranurseries"

df = select(df, study, species, trait_name, value)

write.csv(df, "Benaranurseriesraw.csv", row.names = F)
