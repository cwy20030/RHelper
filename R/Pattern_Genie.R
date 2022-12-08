# Pattern Genie
#
# This function will loop over strings and find all possible patterns.
#
# @param Decomp strsplit characters
# @return export Patterns
#






Pattern_Genie <- function(Decomp){

  ######################### Try with all names ###############################
  dg <-  lapply(1:(length(Decomp)-1),function(m){

    # Using one pattern to try to match all the following
    s <- seq(m+1,length(Decomp))

    # This runs comparison between variables in the series of m+1
    dm <-  lapply(s,function(m2){
      if(length(Decomp[[m]])==length(Decomp[[m2]])){
        d <- Decomp[[m]] == Decomp[[m2]]
      } else {
        d <- Decomp[[m]] %in% Decomp[[m2]]
      }

      e <- rle(d)$lengths
      f <- which(e>1)
      g0 <- lapply(f,function(g){
        g1 <- sum(e[seq(1,g-1)])+1
        g2 <- sum(e[seq(1,g)])
        Decomp[[m]][g1:g2]
      })
      g0
    })

# Find unique pattern
    dm.1 <- unique(unlist(dm,recursive = F))
    # Remove NA
    dm.1 <- lapply( dm.1,function(i){
      g <- unlist(i,recursive = T)
      if(NA %in% g){
        g[!is.na(g)]
      } else {
        i
      }
    })
  # Convert back to string
    dm.name <- unlist(lapply(dm.1,paste0,collapse=""),recursive = F)
  # Test Pattern
    Comp <- lapply(Decomp,paste0,collapse="")
    dm.sum <- unlist(lapply(dm.name,function(ctest){
      temp <- sum(unlist(lapply(Comp,function(m3){
        ifelse(grep(ctest,m3),1,0)
      })))
      temp
    }))
    names(dm.sum) <- dm.name
    names(dm.sum)[which(dm.sum > max(dm.sum)/3 )] # at leat 1/3 of the data contains this pattern.

  })

  dg.1 <- unique(unlist(dg,recursive = F))

  #########################             END                 ###############################

   # This is the best pattern matched.

  return(as.character(dg.1))
}
