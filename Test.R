data<-rnorm(10000,mean=10,sd=sqrt(5)) # Draw 10000 samples from N(10,25)
pdf("Histogram.pdf") # Create Histogram PDF
hist(data)
dev.off()
