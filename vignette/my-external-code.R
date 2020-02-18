
#----setup-fx
check_df_nulls <- function(df) {
  ## Function to check the number of NAs in a df
  df[df=='']
  Column.Name <- rep(NA, dim(df)[2])
  Col.Type <- sapply(df, typeof)
  Column.Type <- rep(NA, dim(df)[2])
  Number.NAs<-rep(NA, dim(df)[2])
  per.of.NAs<-rep(NA, dim(df)[2])
  
  
  for(i in 1:dim(df)[2])
  {
    #cat(sprintf('Column %.0f: %30s \t Number of NAs: %.0f \t Percent NA data: %.0f%% \n'
    #            , i,names(df)[i], length(which(is.na(df[,i]))),100*length(which(is.na(df[,i])))/dim(df)[1]))
    Column.Name[i] <- names(df)[i]
    Number.NAs[i]  <- length(which(is.na(df[,i])))
    Column.Type[i] <- Col.Type[[i]]
    
  }
  df_NAs <- data.frame(Column.Name, Column.Type, Number.NAs, per.of.NAs)
  df_NAs$per.of.NAs <- round(100*(Number.NAs/dim(df)[1]),0)
  
  return(df_NAs)
  
}
#----print-output
starwars <- data.frame(starwars)
cat(sprintf('The Starwars data set contains %.0f rows and %.0f columns \n
            Columns have the following type and number of NAs', dim(starwars)[1],dim(starwars)[2]))
print(check_df_nulls(starwars))

