randIndividual = function(num_chromosomes, chromosome_length) {
  len = num_chromosomes*chromosome_length
  output = rep(0, len)
  for(i in 1:len) {
    if (runif(1,0,1) < 0.5) {
      output[i] = 1
    }
  }
  return(output)
}

cloneIndividual = function(ind) {
  len = length(ind)
  output = rep(0, len)
  for(i in 1:len) {
    if(ind[i]==1) {
      output[i] = 1
    }
  }
  return(output)
}

reproduce = function(ind1, ind2, num_chromosomes, chromosome_length) {
  
}