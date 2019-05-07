library(stringr)

# x2 = gsub(" ", "5", ACTplantselector$Common_name)
x2 = str_match_all(Ozbreed$value, "[[:alnum:]|[:blank:]|[:punct:]]" )

x2[lengths(x2) == 0] <- NA_character_



z = character()

for (i in 1:length(Ozbreed$value) ){
  y = str_c(x2[[i]], collapse = "")
  
 z =  append(z, y)
}
############### Needed if chinese characters appear
# x2 = str_match_all(z, "[a-zA-Z]|[:blank:]")
# 
# z = character()
# for (i in 1:2736 ){
#   y = str_c(x2[[i]], collapse = "")
#   
#   z =  append(z, y)
# }

Ozbreed$value = z



