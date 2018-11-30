#loading the 3-d arrays (historical simulation)
for(n in 1:N){
  assign(paste0(locations$town[n],"_hist"),
         readRDS(paste0(locations$town[n],"_hist.rdata")))
  print(paste(locations$town[n],"loaded"))
}

#loading the 3-d arrays (RCP 4.5)
for(n in 1:N){
  assign(paste0(locations$town[n],"_45"),
         readRDS(paste0(locations$town[n],"_45.rdata")))
  print(paste(locations$town[n],"loaded"))
}

#loading the 3-d arrays (RCP 8.5)
for(n in 1:N){
  assign(paste0(locations$town[n],"_85"),
         readRDS(paste0(locations$town[n],"_85.rdata")))
  print(paste(locations$town[n],"loaded"))
}