cytoDiv <- function(df, Ncat = 256, Nbit = 16, do.plot=FALSE,...){
	
	jet.colors <- colorRampPalette(c("white","red","red4","tomato4","black"))
	# why is this necessary? does it convert to the correct format?
        filtered <- df

     # bin the data using a multivariate kernel smoother
     # ks package (appropriate for 1 to 6-D data)
     dens <- kde(filtered, H, h, gridsize, gridtype, xmin, xmax, supp=3.7, eval.points,
     binned=TRUE, bgridsize, positive=FALSE, adj.positive, w,compute.cont=FALSE, approx.cont=TRUE)


	# Plot the output 
    if(do.plot==TRUE){		
	zfacet <- dens$zden[-1,-1] + dens$zden[-1,-Ncat] + dens$zden[-Ncat,-1] + dens$zden[-Ncat,-Ncat]
	persp(dens$xords, dens$yords, dens$zden/sum(dens$zden), ticktype='detailed', xlab=paste(x.var), ylab=paste(y.var), zlab="Probability", col=jet.colors(100)[cut(zfacet,100)],theta=20, phi=65)     		
     		}
     	 
     	
    pi <- dens$zden/sum(dens$zden) # caculate probability per category
	p <- pi[!(pi==0)] # remove empty category
	N0 <- sum(p^0) # Number of categories
	H <- - sum(p*log(p)) # Shannon-Wiener Diversity index H'
	N2 <- sum(p^2) # Simpson's index
	D <- 1 - N2 # Simpson's Index of Diversity
	J <- H/log(N0) # Evenness
	
	indices <- data.frame(cbind(N0, H, N2, D, J))
	
	return(indices)
	
}
