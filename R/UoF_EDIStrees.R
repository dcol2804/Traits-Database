library(pdftools)

library(rvest)

library(stringr)
# copy in the website URL
url = "http://hort.ufl.edu/database/trees/trees_scientific.shtml"

webpage = read_html(url)

# get all of the urls on the page
plant_url_html = html_nodes(webpage, "a")
plant_url = html_attr(plant_url_html, name = "href")
plant_url1 = plant_url[grepl("documents/pdf/", plant_url) == T]
plant_url1 = sub("..", "", plant_url1)
plant_url_all1 = str_c("http://hort.ufl.edu/database", plant_url1)

plant_url2 = plant_url[grepl("/pdffiles/ST",plant_url) == T]
plant_url_all = append(plant_url_all1, plant_url2)

# Now start assembling the data into one big df called df
df = data.frame(study = character(), species = character(), trait_name = character(), value = character())


for (Q in 1:length(plant_url_all)){
  
  # the names are abbreviations of the scientific name
  N = sub(".*(tree_fact_sheets/)", "", plant_url_all[Q])
  
  # download the pdf
  download.file(plant_url_all[Q], N, mode = "wb")

# read the pdf
j = pdf_text(N)

result = character()
for (i in 1:length(j)){


# Split the pdf so it is based on new lines
x = unlist(strsplit(j[i], "\n"))

# remove annotations
x = x[!grepl("Environmental Horticulture Department", x) == T]
x = x[!grepl("Institute of Food and Agricultural Sciences", x) == T]
x = x[!grepl("Fact Sheet", x) == T]
x = x[!grepl("November 1993", x) == T]
x = x[!grepl("Page [[:digit:]]", x) == T]
# find the location of the pattern that matchesa a large space and a letter

a = character()
for (i in 1:length(x)){
  z = str_locate(x[i], "      [:alpha:]")
  a = append(a, z)
}

# find the most frequent score

c = as.integer(names(sort(table(a),decreasing=TRUE)[2])) -1

# subset the original file at location c of each line
e = character()
for (i in 1:length(x)){
  d = str_sub(x[i], c, str_length(x[i]))
  e = append(e, d)
}
e = str_trim(e)
e = e[!e == ""]
e

# now get the original lines without the parts you extracted
g = ""
for (i in 1:length(x)){
  f = str_sub(x[i], 1, c)
  g = append(g, f)
}
g = str_trim(g)
g = g[!g == ""]
g
h = append(g, e)
h
#########################################

# Now append each page to them

result = append(result, h)
}

k = which(grepl("^[[:lower:]]", result) == T)
Breakendpos <- c(0, which(diff(k) != 1), length(k)) 
# This is complicated because I have a string of line numbers and then positions within these line numbers
lastlines = k[Breakendpos]
Breakstartpos = Breakendpos + 1
Breakstartlines = k[Breakstartpos]
Breakstartlines = Breakstartlines - 1

for (i in 1:length(lastlines)){
  result[c(Breakstartlines[i])] = str_c(result[c(Breakstartlines[i]:lastlines[i])], collapse = " ")
}

# get rid of the lines you have just pasted to the capital letter lines
result = result[!grepl("^[[:lower:]]", result) == T]

# OK so this is cleaned up and the file is called "result".

# Now I can just grab all the cells with : in them, split them and create a dataframe.
traits = data.frame(trait_name = character(), value = character())
for (i in 1:length(result)){
  if (grepl(":", result[i]) == T ){
    temp = data.frame(trait_name = unlist(strsplit(result[i], ":"))[1], 
                      value = unlist(strsplit(result[i], ":"))[2], stringsAsFactors = F)
    traits = rbind(traits, temp)
  }
}
# Great!

# Now clean up one or two of the traits before publishing them. 

  heights = traits$value[which(grepl( "Height", traits$trait_name)== T)]
  heights = str_extract(heights, "[[:digit:]].* ")
  heights = strsplit(heights, " to ")
  min_height = as.numeric(heights[[1]][1])*0.3048
  max_height = as.numeric(heights[[1]][2])*0.3048
  
  minheight = data.frame(trait_name = "min_height", value = min_height, stringsAsFactors = F)
  maxheight = data.frame(trait_name = "max_height", value = max_height, stringsAsFactors = F)
  
  widths = traits$value[which(grepl( "Spread", traits$trait_name)== T)]
  widths = str_extract(widths, "[[:digit:]].* ")
  widths = strsplit(widths, " to ")
  min_width = as.numeric(widths[[1]][1])*0.3048
  max_width = as.numeric(widths[[1]][2])*0.3048
  
  minwidth = data.frame(trait_name = "min_width", value = min_width, stringsAsFactors = F)
  maxwidth = data.frame(trait_name = "max_width", value = max_width, stringsAsFactors = F)
  
  traits = rbind(traits[-which(grepl( "Height", traits$trait_name)== T),], minheight, maxheight)
  traits = rbind(traits[-which(grepl( "Spread", traits$trait_name)== T),], minwidth, maxwidth)
  
  # Now all I have to do is add this particular pdf data to the large df in the correct format. 
  
  traits$study = "UoF_EDIS"
  traits$species = traits$value[traits$trait_name == "Scientific name"]
  
  traits = data.frame(study = traits$study, species = traits$species, trait_name = traits$trait_name, value = traits$value, stringsAsFactors = F)
  
  df = rbind(df, traits)
}

df1 = df
df1$trait_name = str_squish(df1$trait_name)

df1 = df1[str_count(df1$trait_name)< 30,]
unique(df1$trait_name)
