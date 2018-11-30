#saving the 3-d arrays (historical simulation)
for(n in 1:N){
  saveRDS(get(paste0(locations$town[n],"_hist")),
          paste0(locations$town[n],"_hist.rdata"))
  print(paste(locations$town[n],"saved"))
}

#saving the 3-d arrays (RCP 4.5)
for(n in 1:N){
  saveRDS(get(paste0(locations$town[n],"_45")),
          paste0(locations$town[n],"_45.rdata"))
  print(paste(locations$town[n],"saved"))
}

#saving the 3-d arrays (RCP 8.5)
for(n in 1:N){
  saveRDS(get(paste0(locations$town[n],"_85")),
          paste0(locations$town[n],"_85.rdata"))
  print(paste(locations$town[n],"saved"))
}