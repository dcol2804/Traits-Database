library(rvest)

library(stringr)

url = "http://www.treemovals.com.au"

plant_url_all = str_c(url, "/product_info.php?products_id=", 37:170)
# Get the trait_names
webpage = read_html(plant_url_all[1])
Titles_html = html_nodes(webpage, ".main td td td:nth-child(1) p")
Titles = html_text(Titles_html)
# get rid of the colon
Titles = gsub(":", "", Titles)

df = data.frame(species = character(), trait_name = character(), value = character(), stringsAsFactors = F)

#start scraping!
for (i in plant_url_all){
webpage = read_html(i)
botanical_name_html = html_nodes(webpage, "em")
botanical_name = html_text(botanical_name_html)

common_name_html = html_nodes(webpage, ".main h1")
common_name = html_text(common_name_html)
common_name = gsub(botanical_name, "", common_name)
common_name = gsub(" - ", "", common_name)

data1_html = html_nodes(webpage, ".main td td td p")
data1 = html_text(data1_html)


############################

data1 = data1_html = html_nodes(webpage, ".main td td td p")
data1 = html_text(data1_html)

data2 = data1[seq(2,36, 2)]

tempdf = data.frame(species = botanical_name, trait_name = Titles, value = data2) 

tempdfcn = data.frame(species = botanical_name, trait_name = "common_name", value = common_name)

tempdf = rbind(tempdfcn, tempdf)

df = rbind(df, tempdf)
}
df$study = "Treemovals"
df = select(df, study, species, trait_name, value)

write.csv(df, "Alpinenurseriesraw.csv", row.names = F)
