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

ch8q3 <- function() {

	x = c(21.9, 23.4, 22.1, 22.1, 24.7, 24.6, 24.0, 24.1, 24.2, 26.5, 23.8, 25.3, 24.8, 24.8, 24.5, 27.8, 24.9, 27.2, 25.1, 25.5, 23.7, 26.5, 22.0, 26.7, 25.2, 23.1, 25.4)

	print("Part A")
	stem(x)
	print("Yes, assumption of normality appears reasonable")

	print("Part B")
	#sample mean
	n = length(x)
	xbar = sum(x) / n

	#sample variance calculation
	diff = (x - xbar)*(x-xbar)
	s2 = sum(diff) / (n-1)
	print(s2)

	print("Part C")
	#confidence interval calculation
	c_i = 99
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
	print("Try 95% CI")
	c_i = 80
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

}

ch8q13 <- function() {

x = c(2.0, 1.4, 3.5, 2.3, 3.2, 3.6, 0.1, 3.5, 2.2, 2.1, 2.4, 1.5, 2.2, 2.3, 2.7, 1.9, 1.7, 1.8, 3.1, 1.5, 1.5, 2.6, 2.8, 2.5, 2.5, 3.9, 0.8, 1.8, 3.3, 3.7)

print("Part A")
#sample mean
print("Sample Mean")
n = length(x)
xbar = sum(x) / n
print(xbar)
#sample variance calculation
print("Sample Variance")
diff = (x - xbar)*(x-xbar)
s2 = 0.89#sum(diff) / (n-1)
print(s2)


print("Part B")
df = n-1
t = qt(0.995, df)
l = t*(s2/sqrt(n))
print("L1, L2 are xbar +- :")
print(l)

print("Part C")
print("yes - we are 99% confident that x is within 2.35 +- 0.45. 6.6 is out of this range")

}

ch8q15 <- function() {

x = c(290, 610, 790, 670, 770, 420, 600, 350, 800, 920, 410, 810, 620, 560, 550, 610, 510, 390, 480, 630, 470, 380, 550, 570, 730, 680, 530, 650, 1000, 720)

print("Part A")
stem(x)
print("Yes, normality assumption appears to be met based on shape of stem and leaf plot")

print("Part B")
boxplot(x)
print("No: Q1 is 480, Q3 is 720, IQR is 240, 1.5*IQR is 360, f1 is 120, f3 is 1080. No vals above 1080 or below 120")

print("Part C")
#sample mean
n = length(x)
xbar = sum(x) / n
#sample variance calculation
s = sd(x)
df = n-1
t = qt(0.995, df)
l = t*(s/sqrt(n))
print("L1, L2 are xbar +- :")
print(l)

print("Part D")
print("Lower the confidence level")

}

ch9q1 <- function() {

print("Part A")
n = 50
k = 45
pbar = k/n
print(pbar)

print("Part B")
calc2 = (pbar * (1 - pbar))/n
calc3 = sqrt(calc2)
zmultiplier = qnorm(0.95, mean = 0, sd = 1)
l = calc3*zmultiplier
print(l)

print("Part C")
d = 0.02
n1 = ((zmultiplier/d)^2) * (pbar) * (1 - pbar)
print(n1)

}


ch9q1()




