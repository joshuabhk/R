calculate_area <- function( gvec, gvec.w ) {
  gvec.n <- cumsum(gvec - sum(gvec)/length(gvec) ) * gvec.w
  area <- sum( abs(gvec.n) )
  
  return(area)
}

calculate_height <- function( gvec, gvec.w ) {
  gvec.n <- cumsum(gvec - sum(gvec)/length(gvec) ) * gvec.w
  max( abs(gvec.n) )
}

plot_fitting <- function( data, meanlog, sdlog, breaks=25, npoints=200, dfunc=dlnorm, main="" ) {
  hist( data, breaks=breaks, probability=T, main=main )
  x <- seq( min(data), max(data), length=npoints )
  y <- dfunc(x, meanlog, sdlog)
  lines(x,y, col="red", lwd=2)
}

calculate_gsea_pvalue <- function( vec, w=1, ylab="vec", xlab="", main="GSEA LIKE TEST", ntest=10000 ) {
  require(fitdistrplus)
  
  vec.w <- w
  gvec <- sapply( vec, function(e) {if(e==T){1} else{-1}} )
  gvec.n <- cumsum(gvec - sum(gvec)/length(gvec) ) #* vec.w #display purpose only
  
  sample_areas <- sapply( 1:ntest, function(i){ calculate_area(sample(gvec), vec.w) } )
  #hist(sample_areas)
  #fw <- fitdist(sample_areas, "weibull")
  fw <- fitdist(sample_areas, "lnorm")
  #plot(fw)
  
  pval = plnorm( calculate_area(gvec,vec.w), fw$estimate[1], fw$estimate[2], lower.tail = F )
  #pval = pweibull( calculate_area(gvec,vec.w), fw$estimate[1], fw$estimate[2], lower.tail = F )
  
  plot_fitting( sample_areas, fw$estimate[1], fw$estimate[2], dfunc=dlnorm, main="Area Fit" )
  #plot_fitting( sample_areas, fw$estimate[1], fw$estimate[2], dfunc=dweibull )
  result <- list()
  result[['area']] <- calculate_area(gvec,vec.w)
  result[['area.fit']] <- fw
  result[[ 'area.pval']] <- pval
  print(summary(fw))
  
  sample_heights <- sapply( 1:ntest, function(i){ calculate_height(sample(gvec), vec.w) } )
  fw <- fitdist(sample_heights, "lnorm")
  pval = plnorm( calculate_height(gvec,vec.w), fw$estimate[1], fw$estimate[2], lower.tail = F )
  plot_fitting( sample_heights, fw$estimate[1], fw$estimate[2], dfunc=dlnorm, main="Height Fit" )
  result[['height']] <- calculate_height(gvec,vec.w)
  result[['height.fit']] <- fw
  result[['height.pval']] <- pval
  print(summary(fw))
  
  plot(gvec.n, type='l', lwd=5, main=main, ylab=ylab, xlab=xlab )
  return( result )
}
