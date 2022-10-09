##################################### Applied Exercise 5 #####################################################################

boston = read.csv("boston_corrected.csv", header = TRUE)
boston = boston[, 7:20]
head(boston)

### Part 1    (A)  ###
mean(boston$CMEDV)

### Part 2      (B)   ###
sample.sd = sd(boston$CMEDV)
sample.sem = sample.sd / sqrt(dim(boston)[1])
sample.sem

### Part 3         (C)   ###
set.seed(312)

boot.fn = function(data, index){
  return(mean(data[index, "CMEDV"]))
}
boot(boston, boot.fn, 10000)

### Part 4          (D)     ###
t.test(boston$CMEDV)

### Part 5             (E)   ###
median(boston$CMEDV)

### Part 6              (F)   ###
set.seed(312)

boot.fn = function(data, index){
  return(median(data[index, "CMEDV"]))
}

boot(boston, boot.fn, 10000)

### Part 7              (G)         ###
quantile(boston$CMEDV, c(0.1))

### Part 8              (H)         ###
set.seed(312)

boot.fn = function(data, index){
  return(quantile(data[index, "CMEDV"], 0.1))
}

boot(boston, boot.fn, 10000)

