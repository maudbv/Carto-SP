# Function to replace a taxonomy by another in a column listing obects

change_taxonomy = function (df = SPdata,
                            col = "Taxons",
                            index = data_taxons ,
                            col_init = "TAXONS TABLEAU",
                            col_finale = "TAXONS FINAUX") {
  
# extract column to update
l = df[,col]

# For each element of the index:
for (i in 1:nrow(index)) {

  l = sapply(l, FUN = function(m) {
    # Search for the old name and replace by new
    gsub(pattern = index[i,col_init],
           replacement = index[i,col_finale],
             x = m)
  })
}
# Return a corrected column (vector)
return(l)
}

