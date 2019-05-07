library(rvest)
library(stringr)
library(BBmisc)


# Generate all the possible plant id numbers
list_of_possible_urls = str_c("http://plantselector.botanicgardens.sa.gov.au/Plants/Details/",1:18220)


plant_url_all = character()

# Check if they are real websites or not

for (i in list_of_possible_urls){
  
    tryCatch({
      if (is.error(read_html(i)) == F)
        plant_url_all = append(plant_url_all, i)
      
      if (is.error(read_html(i))!= F) stop()
    }, error=function(e){})
  }


df = data.frame(species = character(), trait_name = character(), value = character(), stringsAsFactors = F)

for (i in plant_url_all){
  webpage = read_html(i)
  
  species_html = html_nodes(webpage, ".detail em")
  species = html_text(species_html)
  
  description_html = html_nodes(webpage, ".notes")
  description = html_text(description_html)
  description = gsub("\r\n","",description)
  description = str_trim(description)
  description = str_squish(description)
  # added in a clause for cases where no description exists
  
  headings_html = html_nodes(webpage, ".attrtitle")
  headings = html_text(headings_html)
  headings = str_trim(headings)
  if (headings[length(headings)] == "Indigenous to the Adelaide Region"){
    length(headings) = length(headings) -1
  }else{
    headings = headings
  }

 
  
  data_html = html_nodes(webpage, ".attrvalue")
  data = html_text(data_html)
  a = str_c(data, collapse = " ")
  
  
  if (grepl("Full Sun", a) == T & grepl("Part Shade", a) == T & grepl("Full Shade", a) == T){
  b = str_which(data, "Full Sun")
  c = str_which(data, "Part Shade")
  d = str_which(data, "Full Shade")
  data[min(c(b, c, d))] = str_c(data[c(b,c,d)], collapse = ", ")
  data = data[!(data == "Full Sun")]
  data = data[!(data == "Part Shade")]
  data = data[!(data == "Full Shade")]
  
}else if (grepl("Full Sun", a) == T & grepl("Part Shade", a) == T & grepl("Full Shade", a) == F){
    b = str_which(data, "Full Sun")
    c = str_which(data, "Part Shade")
    data[min(c(b, c))] = str_c(data[c(b,c)], collapse = ", ")
    data = data[!(data == "Full Sun")]
    data = data[!(data == "Part Shade")]
    
  }else if (grepl("Full Sun", a) == F & grepl("Part Shade", a) == T & grepl("Full Shade", a) == T){
      # b = str_which(data, "Full Sun")
      c = str_which(data, "Part Shade")
      d = str_which(data, "Full Shade")
      data[min(c(c, d))] = str_c(data[c(c,d)], collapse = ", ")
      data = data[!(data == "Part Shade")]
      data = data[!(data == "Full Shade")]
      
  }else if (grepl("Full Sun", a) == T & grepl("Part Shade", a) == F & grepl("Full Shade", a) == T){
        b = str_which(data, "Full Sun")
        # c = str_which(data, "Part Shade")
        d = str_which(data, "Full Shade")
        data[min(c(b, d))] = str_c(data[c(b,d)], collapse = ", ")
        data = data[!(data == "Full Sun")]
        # data = data[!(data == "Part Shade")]
        data = data[!(data == "Full Shade")]
      
   }
#  if (data[1]data[5] == "Full Sun"|data[5] =="Part Shade"|data[5] =="Full Shade"){
#    
#    data[3] = str_c(data[c(3,4,5)], collapse = ", ") 
#    data = data[-c(4,5)]
#  }else if (data[4] == "Full Sun"|data[4] =="Part Shade"|data[4] =="Full Shade"){
#    
#    data[3] = str_c(data[c(3,4)], collapse = ", ") 
#    data = data[-4]
#  }else{
#   data = data
# }
  
  
  # create a temporary dataframe for each species
  
  dfprep = data.frame(species = character(length = length(headings)+1), 
                      trait_name = character(length = length(headings)+1), 
                      value = character(length = length(headings)+1))
  
  # assemble into df
  
  dfprep[,1] = as.character(species)
  dfprep[,2] = c(headings, "description")
  dfprep[,3] = c(data, description)
  
  df = rbind(df, dfprep)
}

df$study = "Plant_selector_plus"
df = select(df, study, species, trait_name, value)

write.csv(df, "Plantselectorplusraw.csv", row.names = F)
write.csv(plant_url_all, "PSPURLS.csv")
