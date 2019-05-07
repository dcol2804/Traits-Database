library(rvest)
library(stringr)

url = "https://www.ozbreed.com.au/plant-ranges/browse-plants-by-trade-name/"
webpage = read_html(url)
plant_url_html = html_nodes(webpage, "a")
plant_url = html_attr(plant_url_html, name = "href")
plant_url
# Take out the NAs
plant_url = plant_url[!is.na(plant_url)]
# 
plant_url = plant_url[-str_which(plant_url, regex("browse", ignore_case = TRUE))]

#lazy but now its literally cutting off the first 80 and the last 30 or something. So if I come back, have to check they haven't updated the page

plant_url = plant_url[-c(1:89)]
plant_url = plant_url[-c(151:length(plant_url))]

# now we can sort out the data we will download

# test on the first website
webpage = read_html(plant_url[1])
# species name
species_html = html_nodes(webpage, "em")
species = html_text(species_html)
# cultivar name
Cultivar_html = html_nodes(webpage, "strong")
Cultivar = html_text(Cultivar_html)
Cultivar
# data
test_html = html_nodes(webpage, "#single-blocks .vc_col-sm-6 .wpb_content_element:nth-child(1) p")
test = html_text(test_html)

test_split = str_split_fixed(test, ":", n= 2)
test_split[,2] = str_trim(test_split[,2])

# now to make the function
df1 = data.frame(trait_name = character(), value = character(), species = character(), stringsAsFactors = F)

for (i in plant_url[1:length(plant_url)]) {
  webpage = read_html(i)
  # species
  species_html = html_nodes(webpage, "em")
  species = html_text(species_html)
  
  if (length(species) > 1){
  
  species = species[length(species)]
  
  }else{
  species = species[1]
}
  # # cultivar name
  Cultivar_html = html_nodes(webpage, "strong")
  Cultivar = html_text(Cultivar_html)
  Cultivar = Cultivar[1]
  
  # data

  data_html = html_nodes(webpage, ".vc_col-sm-6 .wpb_content_element:nth-child(1) p")
  data1 = html_text(data_html)
  
  data1 = str_split_fixed(data1, ":", n= 2)
  data1[,2] = str_trim(data1[,2])
  
  # Assemble the tempdf
  tempdf = as.data.frame(data1, stringsAsFactors = F)
  names(tempdf) = c("trait_name", "value")
  tempdf[c(length(tempdf[,1])+ 1),1] = "Cultivar"
  tempdf[c(length(tempdf[,1])),2] = Cultivar
  tempdf$species = species
  
  df1 = rbind(df, tempdf)
  
  Sys.sleep(5) 
}

# clean up the df a bit
df2 = str_trim(df1)
df2 = df1[(!str_detect(df1$trait_name, regex("Copyright", ignore_case = TRUE)) == T),]

df3 = filter(df2, trait_name != "" | value != "")

           

# Baloskion tetraphyllum

df3[is.na(df3$species),]$species = "Baloskion tetraphyllum"
# Get rid of the description paragraphs and move them to the value column, adding in Description as their trait

Extraparas = filter(df3, trait_name != "Description" & trait_name != "Size" & trait_name != "Uses" & trait_name != "Position"& trait_name != "Care" &trait_name != "Where it works" & trait_name != "Cultivar")

NewExtraparas = Extraparas[Extraparas$trait_name != "NOTE" & Extraparas$trait_name != "Please note"& Extraparas$trait_name != "Location"& Extraparas$trait_name != "Phone",]

NewExtraparas$value = NewExtraparas$trait_name

NewExtraparas$trait_name = "Description"

NewExtraparas = NewExtraparas[-c(3,11),]

df4 = filter(df3, trait_name == "Description" | trait_name == "Size" | trait_name == "Uses" | trait_name == "Position"| trait_name == "Care" |trait_name == "Where it works" |trait_name == "Cultivar")

df5 = rbind(df4, NewExtraparas)

# Some of the species had spaces in them
df5$species = str_trim(df5$species)

# For some reason the cultivar didn't register on four of the species

Descriptionstuffups = filter(df3, value == "Description:")  

# This is fucked but I couldn't find any other way. 
df5[126,2] = "Blaze"

df5[399,2] = "Gladiator"

df5[434,2] = "Great white"

df5[784,2] = "Rainbow Twist"

# OK done. 
df5$study = "Ozbreed"

df5 = select(df5, study, species, trait_name, value)

write.csv(df5, "Ozbreedraw.csv", row.names = F)
