dset1=c(199809,200665,199607,200270,199649)
dset2=c(522573,244456,139979,71531,21461)

chi2IsUniform<-function(dataset,significance=0.05){
  chi2IsUniform=(chisq.test(dataset)$p.value>significance)
}

for (ds in list(dset1,dset2)){
  print(c("Data set:",ds))
  print(chisq.test(ds))
  print(paste("uniform?",chi2IsUniform(ds)))
}
