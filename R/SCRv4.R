# This is an analysis rather than a cleaing code to see how many traits there are etc
# It also matches the species to Hugh's list


# Now to deal with the unique species list
species = df1 %>% select(newspecies, species_number) %>% unique()

# comparing with Hugh's list
length(intersect(WPW_MODELLING_LIST_MARCH2019$Evergreen_taxon, species$newspecies))
length(intersect(WPW_MODELLING_LIST_MARCH2019$GBIF_taxon, species$newspecies))
length(intersect(WPW_MODELLING_LIST_MARCH2019$searchTaxon, species$newspecies))

# now for the trait sublevels themselves
m = df1

for (u in 1:length(df1$trait_name)){
  m$merged[u] = str_c(m$trait_name[u],m$value[u], collapse = " ")
}
# Take out traits with millions of values
m = m[!m$trait_name == "common_name",]
m = m[!m$trait_name == "max_height_nature",]
m = m[!m$trait_name == "max_height",]
m = m[!m$trait_name == "height",]
m = m[!m$trait_name == "min_height",]
m = m[!m$trait_name == "max_width",]
m = m[!m$trait_name == "width",]
m = m[!m$trait_name == "min_width",]
m = m[!m$trait_name == "min_width",]

#import the Traitlist4long excel file
# 
for (v in 1:length(Traitlist4long$Subtrait)){
  Traitlist4long$merged[v] = str_c(Traitlist4long$Trait_name[v],Traitlist4long$Subtrait[v], collapse = " ")
}

Traitlist4long$merged[1] = "x"


# 
w = as.data.frame(table(m$merged))

for (t1 in 1:length(w$Var1)){
  if (w$Var1[t1] %in% Traitlist4long$merged == T){
    w$trait_index[t1] = Traitlist4long[Traitlist4long$merged== w$Var1[t1],]$X__1
  }
}

write.csv(w, "traitnumbers1.csv")