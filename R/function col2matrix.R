# Fonction générique pour transformer une colonne d'élements multiples en une matrice de comptage.
col2matrix = function(df = SPdata, 
                      column = "Taxons",
                      name_col = "Nom",
                      sep = ",",
                      id_cols = NULL,
                      id_cols_new = NULL) {
  
  require(dplyr, stringr)
  if (length(id_cols_new) != length(id_cols)) warning("number of new names does not match the number of id_cols")
  
  # coerce to dataframe
  df = as.data.frame(df)
  
  # Extract unique objects in column
  objects = na.omit(unique(unlist(str_split(as.character(df[, column]), pattern = sep))))

  # Corriger les espaces supplémentaires: 
  objects = unique(str_trim(objects))
  
  # Table pour résumer les taxons
  df_objects = as.data.frame(matrix(0,
                                   nrow(df),
                                   length(objects),
                                   dimnames = list(df[,name_col], objects)))
  df_objects$objects = df[, column]
  df_objects$names = df[,name_col]
  
  # Fill the indicence matrix:
  for (i in objects) {
    df_objects[grep(i, df_objects$objects), i] = 1
  }
  
  # OPTION: Add additional data if asked:
  if(length(id_cols)>0) {
  add <- select(df, any_of(c(name_col, id_cols)))
  
  # Rename columns if asked
  if(length(id_cols_new)>0)  {
    if (length(id_cols_new) != length(id_cols)) warning("number of new names does not match the number of id_cols")
  names(add) = c("names", id_cols_new)
  }  

  df_objects <-  dplyr::left_join(
    x = df_objects,
    y = add
    )
  }
  # Name rows by the project name
  rownames(df_objects) = df_objects$names
  
  # rename the project column
  names(df_objects)[ names(df_objects) == "names"] =  "project"
 
return(list(objects = objects, data = df_objects))
}
