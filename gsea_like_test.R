calculate_area <- function( gvec ) {
  gvec.n <- cumsum(gvec - sum(gvec)/length(gvec) )
  area <- sum( abs(gvec.n) )
  
  return(area)
}

calculate_gsea_pvalue <- function( vec, ylab="vec", main="GSEA LIKE TEST", ntest=10000 ) {
  gvec <- sapply( vec, function(e) {if(e==T){1} else{-1}} )
  gvec.n <- cumsum(gvec - sum(gvec)/length(gvec) )
  
  sample_areas <- sapply( 1:ntest, function(i){ calculate_area(sample(gvec)) } )
  #hist(sample_areas)
  fw <- fitdist(sample_areas, "lnorm")
  #plot(fw)
  
  pval = plnorm( calculate_area(gvec), fw$estimate["meanlog"], fw$estimate["sdlog"], lower.tail = F )
  plot(gvec.n, main=main, ylab=ylab, xlab=paste( c("p-value:", pval, "comparing to", ntest) ) )
}
