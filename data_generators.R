## data generating distributions for 2017 Statistcal Methods Forum Data Challenge

# Specify a sample size
# The 3 functions below need to be run sequentially
# Generate the data.frame with the covariates W
# 

genW <- function(N) {
	W1 <- sample(LETTERS[1:4], size = N, replace = TRUE, prob = c(0.1, 0.2, 0.65, 0.05))
	W2 <- sample(LETTERS[1:4], size = N, replace = TRUE, prob = c(0.6, 0.2, 0.1, 0.1))
	W3 <- sample(LETTERS[1:3], size = N, replace = TRUE, prob = c(0.3, 0.3, 0.4))
	W4 <- sample(LETTERS[1:3], size = N, replace = TRUE, prob = c(0.7, 0.1, 0.2))
	W5 <- rbinom(N, 1, prob = ifelse(W4 == "A", 0.3, 0.6))
	W6 <- rbinom(N, 1, prob = ifelse(W2 == "C", 0.1*W5 + 0.2*(1-W5), 0.3))
	W7 <- rnorm(N, 2*(W1 == "A") + 3*(W1 == "D") + 4*W5 + W6, 2)
	W8 <- rnorm(N, W7, 2)
	W9 <- pmin(rpois(N, pmax(W8, 0.1)), 11)
	W10 <- rnorm(N, 0, 3)
	W11 <- W10 + .2*(W4 == "B") + .3*(W4 == "C") + 1.2*(W3 == "B" && W2 == "B") + .3*(W3 == "B" && W2 == "C") + -.8*(W3 == "C" && W1 == "A") + 0.08*W8*W10 + rnorm(N, 0, 2)
	W12 <- (W1 == "A") + -1*(W1 == "B") + -2*(W1 == "C") + -3.5*(W1 == "D") + 3*W7 + -2*W9 + 0.05*W7*W9 + 0.5*sqrt(abs(W10)) + 2*W5 + rnorm(N, 0, 2)
	W13 <- W6*W5 + W6 + W8 + sin(2*W8) + 3*(W1 == "A") + rnorm(N, 0, 2)
	W14 <- W9 + rpois(N, W9) + rnorm(N, 0, 1)
	W15 <- W8 + .06*W8^2 + -1*sqrt(abs(W8)) + W10 + rnorm(N, 0, 2)
	W16 <- pmin(rweibull(N, shape = 1/(W9 + 1), scale = pi), 13) + -0.3*W5*W9 + 0.5*W5*W8 + -1*W5 + rnorm(N, 0, 1)
	W17 <- W13 + .2*(W2 == "A") + .5*(W4 == "C") + 1.2*(W2 == "B" && W1 == "B") + .3*(W2 == "B" && W1 == "C") + -1.8*(W3 == "C" && W1 == "A") +  1.5*(W4 == "A") + -.5*(W4 == "B")*W5 + W5*(W4 %in% c("A", "B")) + rnorm(N, 0, 2)
	W18 <- W5 + W6 + W7 + (W1 %in% c("A", "B")) + 1.3*(W1 %in% c("C", "D")) + (W2 %in% c("A")) + 1.5*(W2 %in% c("B")) + 2*(W2 %in% c("C", "D")) + rnorm(N, 0, 1)
	W19 <- W5 + W6 - W5*W6 + W6*W8 + -1.3*W8 + W9 + W5*W9 + W5*W12 + W5*W6*W10 + .4*W10 + 0.3*W10^2 + -0.01*W10^3 + -0.7*W10*W5 + rnorm(N, 0, 1)
	W20 <- 1.3*sin(W10/pi) + rnorm(N, 0, 3)
	W21 <- (W1 %in% c("A", "C")) + 1.5*(W1 %in% c("B")) + 2*(W1 %in% c("D")) + -0.5*(W2 %in% c("A")) + -1.5*(W2 %in% c("B")) + -2*(W2 %in% c("C", "D")) + (W1 %in% c("A", "B"))*(W2 %in% c("B", "C")) + 2*W5 + W5*(W1 %in% c("A", "B", "C")) + W7 + rnorm(N, 0, 1)
	W22 <- pmin(rgamma(N, shape = 5), runif(N, 7, 12))
	W23 <- W22 + W9 + -0.04*W22*W9 + sqrt(W22)*W5 + W5 + rnorm(N, 0, 1)
	W24 <- W15 + W10 + 0.03*W15*W10 + 0.4*(W9 < 5) + 0.6*(W9 >= 5) + rnorm(N, 0, 1)
	W25 <- W16 + W6 + 0.06*W6*W16 + W14 + -0.06*W6*W14 + 0.008*W16*W14 + rnorm(N, 0, 2)
	OUT <- data.frame(W1=W1, W2=W2, W3=W3, W4=W4, W5=W5, W6=W6, W7=W7, W8=W8, W9=W9, W10=W10, W11=W11, W12=W12, W13=W13, W14=W14, W15=W15, W16=W16, W17=W17, W18=W18, W19=W19, W20=W20, W21=W21, W22=W22, W23=W23, W24=W24, W25=W25)
}

genA <- function(N, W, plot_it = FALSE) {
	probA <- plogis(.2 + (W$W2 == "A") + (W$W2 == "B") + -.5*(W$W2 == "C") + -.7*(W$W2 == "D") + -0.6*W$W5 + -.08*W$W5*W$W6 + 0.5*W$W9 - 0.03*W$W9*W$W6*W$W5 + -0.02*W$W16 - 0.3*W$W22 + 0.5*(W$W1 == "A" & W$W3 == "A") + 0.3*(W$W1 == "A" & W$W3 == "B") + -0.2*W$W25 + 0.3*W$W9 - 0.02*W$W9^2 + 0.3*W$W22*W$W5)
	OUT <- rbinom(N, 1, probA)
	if(plot_it) {
		print(summary(probA))
		print(summary(probA[OUT == 1]))
		print(summary(probA[OUT == 0]))
		require(ggplot2)
		p <- ggplot(data.frame(probA = probA, class = factor(OUT)), aes(probA, fill = class, group = class)) + geom_density(alpha = 0.4, position = "identity")
		print(p)
	}
	return(OUT)
}

genY <- function(N, A, W){
	OUT <- A + .3*A*W$W7 + -.4*A*W$W6 + W$W9 + -.1*W$W9^2 + 0.007*W$W9^3 + 2*(W$W1 == "A") + 2.3*(W$W1 == "B") + 1.2*(W$W1 == "C") + 2.5*(W$W1 == "D") + 1.2*(W$W3 == "B" && W$W2 == "B") + 1.2*(W$W2 == "C" && W$W3 == "C") + sqrt(abs(W$W7)) + 0.7*A*(W$W1 %in% c("A", "B")) + 1.3*A*(W$W1 %in% c("C")) - 2*A*W$W5*W$W6 - 0.5*A*W$W5*(1 - W$W6) + rnorm(N, 0, 1) 
}


## Example
N <- 400 # select sample size
W <- genW(N) # generate W data.frame
A <- genA(N, W, plot_it = FALSE)  # using W, generate treatment/exposure variable
Y <- genY(N, A, W) # using W and A, generate outcome
# mean(Y[A == 1]) - mean(Y[A ==0]) # incorrect treatment difference

OUT <- data.frame(ID = paste0("ID", 1:N), Y = Y, A, W)



