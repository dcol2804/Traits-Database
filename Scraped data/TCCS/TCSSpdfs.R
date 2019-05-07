library(pdftools)

library(rvest)

library(stringr)

url = "https://www.tccs.act.gov.au/city-living/trees/design-standards-23-draft-tree-species-list/"

url_list = c("native-15m", "native-10-15m", "native-less-than-10m", "introduced-15m", "introduced-10-15m", "introduced-less-than-10m", "conifers")

list_of_search_pages = str_c(url, url_list)

plant_url_all = character()



for (i in list_of_search_pages){
  
  webpage = read_html(i)
  
  plant_url_html = html_nodes(webpage, ".pdf")
  plant_url = html_attr(plant_url_html, name = "href")
  
  plant_url_all = append(plant_url_all, plant_url)
  
}

#Different format and jams the code
plant_url_all = plant_url_all[-125]

df = data.frame(trait_name = character(), value = character(), species = character())

for (Q in plant_url_all){
   
  N = sub("(.*[0-9]{6})[/]", "", plant_url_all[Q])

   # download the pdf
download.file(Q, N, mode = "wb")

# turn the pdf into text
x = pdf_text(N)

# take a new element every time R sees a "\r\n"
x <- strsplit(x, "\r\n")

# unlist the pdf
x = x[[1]]

# Find all the rows that have a large amount of space and replace with a nothing.
# In this case, the pattern is a large space, the dot represents any type of characters 
# and the star repeats any number of times to the end of the string. (I think. Worked it out from trial and error)

x = gsub("                  .*", "", x)

# Now take out the lines that = ""
x1 = x[!(x=="")]

# Paste the lines that start with (signified by ^) "    " onto the previous line.
for (i in c(1:length(x1))){
  # if the line starts with a large space, 
if (grepl("^   ", x1[i]) == T){
  # paste it onto the previous line
  x1[(i-1)] = paste0(x1[(i-1)], x1[i])
  

  }
  }
# remove the large space starting lines 
x2 = x1[!(grepl("^   ", x1) == T)]
x2 = str_squish(x2)

# Split the string for botanical name and common name. This creates a matrix. 
x3 = as.data.frame(str_split(x2, ":", simplify = T), stringsAsFactors = F)

# Now move all the traits over to the other half. To do this we need to isolate the traits # 
# with this ridiculous dumb character. So elaborate.

traits = which((str_match(x3$V1, "[^[:alnum:]]")!= " "))

# Now we want to do three things:

#1 Delete the pesky first character of each of these positions
#2 Copy the element over to the second column
#3 Replace the first column value with the value preceding it to get the trait right.

for (i in traits){
  # 1
  x3$V1[i] = as.character(substring(x3$V1[i], 3))
  # 2
  x3$V2[i] = x3$V1[i]
  # 3
  x3$V1[i] = x3$V1[(i-1)]
}
# Get the number for the height over as well
for (i in which(str_match(x3$V1, "[^[:digit:]]")== " ")){

  x3$V2[i] = x3$V1[i]
  # 3
  x3$V1[i] = x3$V1[(i-1)]
}
# Finally, remove the rows with nothing in the second column

x4 = x3[which(x3$V2 != ""),]
# And creat a species column
x4$species = x3[1,2]
names(x4) = c("trait_name", "value", "species")


df = rbind(df, x4)
}
df$species = str_trim(df$species)

# get rid of anything before a closed bracket. No idea why

df$species = gsub("(....(.)))","", df$species)

df$species[which(df$species == "")] = "Prunus yedoensis"

df$study = "ACTplanting"
df = select(df, study, species, trait_name, value)

write.csv(df, "TCCScanberraraw.csv", row.names = F)
