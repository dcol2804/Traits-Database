# get common names using taxize
ENTREZ_KEY="fabde8aa4fc944136f608c178038f4b7ce08"
Sys.setenv(ENTREZ_KEY = "fabde8aa4fc944136f608c178038f4b7ce08")

x5 = sci2comm(x2$matched_name[x2$data_source_title == "NCBI"], db = "ncbi", simplify = TRUE)


