library(pdftools)

library(rvest)

library(stringr)

url = "https://www.bgpa.wa.gov.au/about-us/conservation/gardening-resources/754-plant-notes"

webpage = read_html(url)

# .scientificname
# .wf_file_text


  

  
plant_url_html = html_nodes(webpage, "a")
plant_url = html_attr(plant_url_html, name = "href")
  
plant_url

list_of_plant_pdfs = plant_url[str_which(plant_url, ".pdf")] 

list_of_plant_pdfs = str_c("https://www.bgpa.wa.gov.au", list_of_plant_pdfs)

#will need this later

Titles = c("Description", "Flowers", "Growing conditions", "Watering", "Pruning", "Fertiliser", "Pests and diseases", "Miscellaneous","common name")
df1 = data.frame(value = character(), trait_name = character(), species = character())
# Get the last few characters of each pdf for the name
for (Q in list_of_plant_pdfs){
N = sub("(.*pn_)", "", Q)

download.file(Q, N, mode = "wb")

x = pdf_text(N)

# Get rid of everything after "Further information"

x1 = sub("(Further Info.*)", "", x)

# Now split it into lines

x2 <- unlist(strsplit(x1, "\r\n"))
# Take out the blank space
x2 = str_trim(x2)

# Extract the Species from line two and the common name from line one.
if (x2[2] != "Description"){
common_name = x2[1]
species = x2[2]
}else{ 
  common_name = x2[1]
  species = x2[1]
  }


# to capture the lines inbetween the titles, we need to add a "stop" itme at the end of the document.
x2[length(x2)+1] = "common name"
# The titles should be all the same in each pdf
Titles = c("Description", "Flowers", "Growing conditions", "Watering", "Pruning", "Fertiliser", "Pests and diseases", "Miscellaneous","common name")

# Find which positions the titles are in. Lapply doesn't work here with the dplyr function :(

for (i in Titles){
Title_pos[which(Titles ==i)] = str_which(x2, i)
}
Title_pos

# now merge the lines that belong together in one trait. 
# Im taking the line after the first title to the line before the second title 
# and merging them with a space
df = data.frame(value = character())
for (i in 1:(length(Title_pos)-1)){
temp = data.frame(value = str_c(x2[(Title_pos[i]+1):(Title_pos[i+1] -1)], collapse = " "), stringsAsFactors = F)
df = rbind(df, temp)
}
df = rbind(df,common_name)

# Adding in the traits
df$trait_name = Titles
# and the species
df$species = species
df1 = rbind(df1, df)
}

df1$study = "KingsparkBG"
df1 = select(study, species, trait_name, value)

write.csv(df1, "KingsparkBGraw.csv", row.names = F)
