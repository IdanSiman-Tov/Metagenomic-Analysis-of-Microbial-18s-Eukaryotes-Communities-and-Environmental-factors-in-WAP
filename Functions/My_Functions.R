


# Returns a DF what is a percentage of the columns 
Percentage_Function <- function(x){
  copy<-x
  for (y in 1:ncol(x)) {
    if(is.numeric(x[,y])){
      sum<-sum(x[,y])
      if(is.numeric(x[,y])){
        for (z in 1:nrow(x)){
          copy[z,y]<-((x[z,y]/sum))
        }
      }
    }
  }
  return(copy)
}



# Returns a DF that converts the 2 to n column and all the rows to the double data type
df_to_double<- function(x){
  if(is.double(x[1,1])){
  } else {
    a<-lapply(x[,2:ncol(x)], as.numeric)
    return(as.data.frame(a,check.names=FALSE))}
}



# Returns only 18s database dates
only_18s_dates<-function(df){
  if(ncol(df)>=20){
    return(subset (df, select = -c(3,4,5,7)))
    } else {return(df)}
  }



