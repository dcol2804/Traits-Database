# This code creates a list of commonly used australian cultivars from two sources on the web: 
# the first is the APNI and the second is the plantfileonline pdf of species and cultivars

library(rvest)
url = "https://biodiversity.org.au/nsl/services/search?product=APNI&tree.id=&name=&inc._scientific=&inc._cultivar=&inc.cultivar=on&inc._other=&max=5200&display=apni&search=true"

searchpage = read_html(url)

urls = html_nodes(searchpage, "a")
cultivar_list_pages = html_attr(urls, name = "href")
cultivar_list_pages = cultivar_list_pages[which(grepl("apni-format/name", cultivar_list_pages) == T)]
number = str_extract(cultivar_list_pages, "[:digit:]+")
cultivar_list_pages = str_c("https://biodiversity.org.au/nsl/services/rest/name/apni/", number, "/api/apni-format")

cultivar_list = character()

for (i in cultivar_list_pages){
  webpage = read_html(i)
cultivar_name = html_nodes(webpage, "strong")
cultivar_name = html_text(cultivar_name)
cultivar_name = gsub("Showing ", "", cultivar_name)
cultivar_list = append(cultivar_list, cultivar_name)
}



# second source

library(pdftools)

library(stringr)

download.file("http://www.plantfile.com/species&cultivars.pdf", "Plantfileonline", mode = "wb")

j = pdf_text("Plantfileonline")





# Split the pdf so it is based on new lines
j = unlist(strsplit(j, "\r\n"))
j = j[!grepl("^[[:upper:]]{1}", j) == T]
j = as.data.frame(j, stringsAsFactors = F)

j$plant = ""

for (i in 1:length(j$j)){
  if (grepl("^\\s+\\* ", j$j[i]) == T){
    j$plant[i] = "species"
  }else if(grepl("^\\*", j$j[i]) == T){
    j$plant[i] = "species"
  }else if(grepl("^\\s+", j$j[i]) == T){
    j$plant[i] = "cultivar"
  }
}
j = j[-which(j$plant == ""),]

j$species = gsub("\\*", "", j$j)
j$species = str_trim(j$species)

# attache the two together and publish

realplants = unique(append(j$species, cultivar_list$x))

write.csv(realplants, "cultivar_list.csv", row.names = F)
