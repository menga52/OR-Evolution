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

compatible = function(ind1, ind2, num_chromosomes, chromosome_length, threshold) {
  # xor ind1, ind2
  # sum result, divide by num_chromosomes and chromosome_length
  # return result <= threshold
}

reproduce = function(parent1, parent2, num_chromosomes, chromosome_length) {
  # for each chromosome:
    # select one of the parents, fill in output with that parent
    # mutation locations = sample(c(0,1), num_chromosomes*chromosome_length, replace=TRUE, prob=c(1-mu, mu))
    # xor genome with mutation locations, return
  output = rep()
}

generation = function(population, num_chromosomes, chromosome_length) {
  pop_size = length(population)
  genome_size = num_chromosomes*chromosome_length
  dummy_individual = rep(0, genome_size)
  new_population = rep(pop_size, dummy_individual)
  for(i in 1:pop_size) {
    ind1 = dummy_individual
    ind2 = rep(1, genome_size)
    while(!compatible(ind1, ind2)) {
      ind1 = population[sample(c(1:pop_size), 1)]
      ind2 = population[sample(c(1:pop_size), 1)]
      # more realistic to prohibit asexual reproduction. consider.
    }
    new_population[i] = reproduce(ind1, ind2, num_chromosomes, chromosome_length)
  }
  return(new_population)
}
