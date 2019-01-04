# data input
species.dat<-read.table("clipboard",header=T,sep=",")

# list all unique species names
Species<-unique(species.dat$Species)

# specify output-file type and name

pdf(file="fish.pdf")

#loop for generating multiple plots for all species

for (j in Species)
{

# set plot arrangement for each page
par(mfrow=c(3,4))

categories=NULL

species<-subset(species.dat,Species==j)

categories<-unique(species$Variable)

for (i in categories)
{
  plot_data <- subset(species, Variable == i)
  plot(plot_data$X, plot_data$Y, xlab=c(i), ylab="log_abundance", type="l")
}
}
dev.off()
