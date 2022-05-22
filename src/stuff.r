# Code
# Danielle Villa and Andrew Meng

# produce a binary sequence of length num_chromosomes*chromosome_length
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

# copy rand_ind into matrix, run generation() to start with some divergence
generatePopulation = function(rand_ind, num_chromosomes, chromosome_length, pop_size, mu) {
  genome_size = num_chromosomes*chromosome_length
  population = matrix(nrow=pop_size, ncol=genome_size)
  for(i in 1:pop_size) {
    population = vecIntoMatr(population, i, rand_ind)
  }
  return(generation(population, num_chromosomes, chromosome_length, mu))
}

# useful for debugging
# get a second copy of a (assumed to be binary) vector
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

# determine whether two individuals diverge more or less than a given threshold
# never called, but useful for potential extensions
compatible = function(ind1, ind2, threshold) {
  # xor ind1, ind2
  # sum result, divide by num_chromosomes and chromosome_length
  # return result <= threshold
  return(mean(xor(ind1, ind2)) < threshold)
}

# simulate reproduction of two (distinct) parents
reproduce = function(parent1, parent2, num_chromosomes, chromosome_length, mu) {
  output = rep(0, num_chromosomes*chromosome_length)
  for(i in 1:num_chromosomes) {
    # for each chromosome, choose a parent uniformly at random
    if (runif(1,0,1)<0.5) {
      p = parent1
    }
    else {
      p = parent2
    }
    for(j in 1:chromosome_length) {
      # for each bit, determine if mutation occurs
      ind = (i-1)*chromosome_length+j
      gene = p[ind]
      if(runif(1,0,1) < mu) {
        gene = 1 - gene
      }
      output[ind] = gene
    }
  }
  return(output)
}

# this function wouldn't be necessary if we knew what we were doing
# but it outputs a row from a matrix as a vector
vecFromMatr = function(matr, row) {
  out = rep(0, ncol(matr))
  for(j in 1:ncol(matr)) {
    out[j] = matr[row, j]
  }
  return(out)
}

# same as above, but in reverse
# put a vector into a matrix as a row
# also return the matrix because R seems to be pass by value
vecIntoMatr = function(matr, row, vec) {
  for(j in 1:ncol(matr)) {
    matr[row, j] = vec[j]
  }
  return(matr)
}

# create an output generation from an input generation
generation = function(population, num_chromosomes, chromosome_length, mu) {
  pop_size = nrow(population) # number of individuals in the population
  genome_size = num_chromosomes*chromosome_length
  new_population = matrix(nrow=nrow(population), ncol=genome_size) # to be returned, mutated
  for(i in 1:pop_size) {
    indices = sample(c(2:pop_size), replace=FALSE) # indices of two distinct parents
    ind1 = vecFromMatr(population, indices[1]) # vector representation of each parent
    ind2 = vecFromMatr(population, indices[2])
    new_ind = reproduce(ind1, ind2, num_chromosomes, chromosome_length, mu) # vector representation of child
    new_population = vecIntoMatr(new_population, i, new_ind) # put child into new population
  }
  return(new_population)
}

# repeatedly call generation() to simulate gens generations
manyGenerations = function(gens, init_pop, chromosome_length, num_chromosomes, mu) {
  for(i in 1:gens) {
    init_pop = generation(init_pop, num_chromosomes, chromosome_length, mu)
  }
  return(init_pop)
}

# driver function. simulate two populations who share a common ancestor for many generations, then compare
comparison = function(gens, pop_size, chromosome_length, num_chromosomes, mu) {
  rand_ind = randIndividual(num_chromosomes, chromosome_length)
  pop1 = generatePopulation(rand_ind, num_chromosomes, chromosome_length, pop_size, mu)
  pop2 = generatePopulation(rand_ind, num_chromosomes, chromosome_length, pop_size, mu)
  pop1 = manyGenerations(gens, pop1, chromosome_length, num_chromosomes, mu)
  pop2 = manyGenerations(gens, pop2, chromosome_length, num_chromosomes, mu)
  return(sameSpecies(pop1, pop2))
}

# divergence measurement function
sameSpecies = function(pop1, pop2) {
  acc = 0
  for(i in 1:nrow(pop1)) {
    for(j in 1:nrow(pop2)) {
      acc = acc + sum(xor(pop1[i,], pop2[j,]))
    }
  }
  return(acc/(nrow(pop1)*nrow(pop2)*ncol(pop1)))
}

# also a driver
# call comparison function, create a histogram of the results
comparisons = function(gens, pop_size, chromosome_length, num_chromosomes, mu, iters) {
  vec = rep(0, iters)
  for(i in 1:iters) {
    vec[i] = comparison(gens, pop_size, chromosome_length, num_chromosomes, mu)
  }
  hist(vec, main="Empirical Divergence", xlab="divergence")
}

# NOT a driver
# like comparision, but takes in two initial populations
# outputs a sequence of divergences between the two populations over time
divergenceList = function(gens, pop1, pop2, chromosome_length, num_chromosomes, mu) {
  divergences = rep(0,gens)
  for(i in 1:gens) {
    pop1 = generation(pop1, num_chromosomes, chromosome_length, mu)
    pop2 = generation(pop1, num_chromosomes, chromosome_length, mu)
    divergences[i] = sameSpecies(pop1, pop2)
  }
  return(divergences)
}

# driver that calls the above function and plots a line graph
# displays divergence between two populations over time
plotDivergence = function(gens, chromosome_length, num_chromosomes, pop_size, mu) {
  ind1 = randIndividual(num_chromosomes, chromosome_length)
  pop1 = generatePopulation(ind1, num_chromosomes, chromosome_length, pop_size, mu)
  pop2 = generatePopulation(ind1, num_chromosomes, chromosome_length, pop_size, mu)
  divergences = divergenceList(gens, pop1, pop2, chromosome_length, num_chromosomes, mu)
  plot(divergences, type="l")
}