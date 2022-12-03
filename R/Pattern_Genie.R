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

    s <- seq(m+1,length(Decomp))

    dm <-  lapply(s,function(m2){
      if(length(Decomp[[m2-1]])==length(Decomp[[m2]])){
        d <- Decomp[[m2-1]] == Decomp[[m2]]
      } else {
        d <- Decomp[[m2-1]] %in% Decomp[[m2]]
      }

      e <- rle(d)$lengths
      f <- which(e>1)
      lapply(f,function(g){
        g1 <- sum(e[seq(1,g-1)])+1
        g2 <- sum(e[seq(1,g)])
        Decomp[[m2-1]][g1:g2]
      })
    })
    dm.1 <- unique(unlist(dm,recursive = F))
    dm.name <- unlist(lapply(dm.1,paste0,collapse=""))

    dm.sum <- unlist(lapply(dm.1,function(ctest){
      sum(unlist(lapply(Decomp,function(m3){
        h <- ctest %in% m3
        if(!isFALSE(rle(h)$values)) length(rle(h)$length)
      })))
    }))
    names(dm.sum) <- dm.name
    names(dm.sum)[which(dm.sum==length(Decomp))]

  })

  #########################             END                 ###############################

  dx_max_final <- names(summary(as.factor(unlist(dg))))   # This is the best pattern matched.

  return(as.character(dx_max_final))
}
