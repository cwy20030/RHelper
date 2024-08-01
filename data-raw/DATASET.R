## code to prepare `DATASET` dataset goes here
# Preparing Sample Data ----------------
## SimSleep Data Set ----------------------------
set.seed(100000)

Cap <- "NCSM_"

nRandom <- sample(100:500,20)

nYear <- sample(1985:2021,20,replace = TRUE)

Base_Term <- "_Modified_K"


df_list <- rep(list(list()),20)
names(df_list) <- paste0(Cap,nRandom,"_",nYear,Base_Term)


for(i in names(df_list)){
  g <- abs(rnorm(1))
  print(g)
  df_list[[i]] <- data.frame("Age"=sample(18:85,100,replace = T),"Sex"=rbinom(n=100,size=1,prob=g))

}


################### Repeat Until all Sex values are set ###################

for(i in names(df_list)){
  g <- abs(rnorm(1))
  if(NA %in% df_list[[i]]$Sex){
    print(i)
    df_list[[i]]$Sex <- rbinom(n=100,size=1,prob=g)
  }

}
################### END ###################


rbinom_Random <- function(x,preavlence_range,..){
  p <- sample(preavlence_range,1)
  while(NA %in% x)  x <- rbinom(n=100,size=1,prob=p)
  x
}


for(i in names(df_list)){
  df_list[[i]]$K10 <- sample(0:15,100,replace=T)
  df_list[[i]]$Insomnia <- NA
  df_list[[i]]$Insomnia <- as.factor(rbinom_Random(df_list[[i]]$Insomnia,0.3:0.4))
  df_list[[i]]$Apnea <- NA
  df_list[[i]]$Apnea <- as.factor(rbinom_Random(df_list[[i]]$Apnea,0.15:0.3))

}

################### Repeat Until all Sex values are set ###################

for(i in names(df_list)){
  for(x in c("Apnea","Insomnia")){
    g <- abs(rnorm(1))
    if(NA %in% df_list[[i]][[x]]){
      print(i)
      df_list[[i]][[x]] <- as.factor(rbinom(n=100,size=1,prob=g))
    }

  }
}
################### END ###################




### Clean Environment --------
rm("Base_Term","Cap","x","i","g","nRandom","nYear","rbinom_Random")


SimSleep <- df_list

rm(df_list)






# https://rstudio4edu.github.io/rstudio4edu-book/data-pkg.html

usethis::use_data(SimSleep, overwrite = TRUE)

usethis::use_data(NCSM_205_1994_Modified_K, overwrite = TRUE)



























## RandomList ---------------------------

df_list = list(infert,swiss,women,DNase,sleep,data("NCSM_205_1994_Modified_K"))
df_list = lapply(df_list,as.data.frame)
names(df_list)  = c("infert","swiss","women","DNase","sleep","NCSM_205_1994_Modified_K")




# Define a function to generate a random list
generateRandomList <- function(max_depth = 3, max_length = 5,Data=df_list,...) {
  if (max_depth == 0) {
    # If the maximum depth is 0, return a random subset of a data frame
    df <- Data[[sample(length(Data), 1)]]  # Randomly select one of the data frames
    rows <- sample(nrow(df), sample(nrow(df), 1))  # Randomly select some rows
    cols <- sample(ncol(df), sample(ncol(df), 1))  # Randomly select some columns
    return(df[rows, cols])
  } else {
    # Otherwise, generate a list with a random length
    list_length <- sample(max_length, 1)
    lst <- vector("list", list_length)

    # For each element of the list, call the function recursively with a decreased maximum depth
    for (i in seq_len(list_length)) {
      lst[[i]] <- generateRandomList(max_depth - 1, max_length,Data=df_list)
    }

    return(lst)
  }
}

RandomDataList = generateRandomList()
usethis::use_data(RandomDataList, overwrite = TRUE)

