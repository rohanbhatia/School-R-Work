#Chapter 8.1, 8.2: Qs 1 3 9 13 15 17 19. Chapter 9.1: 1 3 5 9

ch8q1 <- function() {

	x = c(1.48, 1.26, 1.52, 1.56, 1.48, 1.46, 1.30, 1.28, 1.43, 1.43, 1.55, 1.57, 1.51, 1.53, 1.68, 1.37, 1.47, 1.61, 1.49, 1.43, 1.64, 1.51, 1.60, 1.65, 1.60, 1.64, 1.51, 1.51, 1.53, 1.74)

	print("Part A")
	stem(x)
	print("Yes, assumption of normality appears reasonable")

	print("Part B")
	#mean
	n = length(x)
	xbar = sum(x) / n

	#sample variance calculation
	diff = (x - xbar)*(x-xbar)
	s2 = sum(diff) / (n -1)
	print(s2)

	print("Part C")
	#confidence interval calculation
	c_i = 95
	calc_1 = 100 - c_i
	alpha = calc_1/100
	alpha_div_2 = alpha/2
	one_min_alpha_div_2 = 1 - alpha_div_2
	df = n-1
	chi_sq_alpha_div_2 = qchisq(alpha_div_2, df)
	chi_sq_one_min_alpha_div_2 = qchisq(one_min_alpha_div_2, df)
	l2 = (df*s2)/chi_sq_alpha_div_2
	l1 = (df*s2)/chi_sq_one_min_alpha_div_2
	print("L1, L2")
	print(l1)
	print(l2)

	print("Part D")
	#stdev CI calc
	newl1 = sqrt(l1)
	newl2 = sqrt(l2)
	print("L1, L2")
	print(newl1)
	print(newl2)

	print("Part E")
	#value of 0.2 surprising?
	print("Yes, because 0.2 is not in the CI from part D")


}


ch8q1()