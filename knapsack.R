# Knapsack Problem in R
#
# Created by Jesse Singh on 2011-04-15
# hello@jessesingh.com
#
# There are five items to put in a knapsack, in this order:
# Box Number | Weight | Value 
#		1	 |   12   | $4
#		2	 |    1   | $2
#		3	 |    4   | $10
#		4	 |    1   | $1
#		5	 |    2   | $2
#
# The correct answer should be all boxes but box 1. That is, (0, 1, 1, 1, 1)

iterations <- 75
Max.Weight <- 15
number_of_rows <- 250

# The percentage of achieving the following reproductive result
crossover <- 0.95
mutation <- 1.0 - crossover

# mutation function
mutate <- function(parent1) {
	x1 <- sample(1:5, 1)
	child <- parent1
	if (parent1[x1] == 1) {
		child[x1] == 0
	}
	else {
		child[x1] == 1
	}
	return(child)
}

# crossover function
xover <- function(parent1, parent2) {
	x1 <- sample(1:5, 1)
	child1 <- c(3,3,3,3,3)
	child2 <- c(3,3,3,3,3)
	for (i in 1:x1) {
		child1[i] <- parent1[i]
		child2[i] <- parent2[i]
	}
	for (i in x1:5) {
		child1[i] <- parent2[i]
		child2[i] <- parent1[i]
	}
	rugrats <- c(child1, child2)
	return(rugrats)
}

# fitness function
fitness <- function(ch) {
	weight <- 0
	value <- 0
	if (ch[1] == "1") {
		weight <- weight + 12
		value <- value + 4
	}
	if (ch[2] == "1") {
		weight <- weight + 1
		value <- value + 2
	}
	if (ch[3] == "1") {
		weight <- weight + 4
		value <- value + 10
	}
	if (ch[4] == "1") {
		weight <- weight + 1
		value <- value + 1
	}
	if (ch[5] == "1") {
		weight <- weight + 2
		value <- value + 2
	}

	# Fitness test
	if (value > 15) {
		return(0)
	}
	else {
		return(value)
	}
}

# Initialition
pop <- mat.or.vec(number_of_rows, 5)
for (j in 1:number_of_rows) {
	chromosome <- c()
	for (i in 1:5) {
		r <- runif(1, 0.0, 1.0)
		if (r <= 0.5) {
			gene <- "1"
		}
		else {
			gene <- "0"
		}
		chromosome <- c(chromosome, gene)
	}
	pop[j,] <- chromosome
}
it <- 1
repeat {
	# Test Against Fitness Function
	fit <- mat.or.vec(nrow(pop), 2)

	for (j in 1:nrow(pop)) {
		fit[j,1] <- j
		fit[j,2] <- fitness(pop[j,])
	}

	totalFit <- sum(fit[,2])

	# Normalize
	for (j in 1:nrow(pop)) {
		fit[j,2] <- fit[j,2] / totalFit
	}

	# Selection Algorithm
	# I generate a random "weight" from 0 through 15 and divide by the total weight
	# calculated earlier. If fit[j,2] >= the random weight, then it is selected. This
	# is akin to a roulette wheel selection. This can be improved by creating a set
	# which contains the total possible weights.
	s <- c()
	repeat {
		r <- sample(0:15, 1)
		person <- sample(1:nrow(pop), 1)
		r <- r / totalFit
		if (fit[person,2] >= r) {
			s <- c(s, person, fit[person,2])
		}
		if (length(s) == nrow(pop) * 2) {
			break
		}
	}

#	for (j in 1:nrow(pop)) {
#		r <- sample(0:15, 1)
#		r <- r / totalFit
#		if (fit[j,2] >= r) {
#			s <- c(s, j, fit[j,2])
#		}
#	}

	# Create an array for the selected parents
	parents <- mat.or.vec(length(s)/2, 2)

	for (i in seq(1,length(parents),by=2)) {
		parents[(i+1)/2,1] <- s[i]
		parents[(i+1)/2,2] <- s[i+1]
	}

	# Generate a children generation
	children <- mat.or.vec(length(s)/2, 5)
	k <- 1
	repeat {
		r1 <- sample(1:nrow(parents), 1)
		r2 <- sample(1:nrow(parents), 1)
		p1 <- parents[r1]
		p2 <- parents[r2]
		chance <- runif(1, 0.0, 1.0)
		if (chance < crossover) {
			kids <- xover(pop[p1,], pop[p2,])	
			kid1 <- c()
			kid2 <- c()
			for (i in 1:5) {
				kid1 <- c(kid1, kids[i])
				kid2 <- c(kid2, kids[i+5])
			}
			children[k,] <- kid1
			children[k+1,] <- kid2
		}
		else {
			kid <- mutate(pop[p1,])
			children[k,] <- kid
		}
		k <- k + 1
		if (k == length(s)/2) {
			break
		}
	}
	it <- it + 1
	cat("Iteration: ", it, "\n")
	for (l in 1:nrow(children)) {
		cat(l, " : ", children[l,], "\n")
	}
	if (it == iterations) {
		break
	}
	pop <- children
}