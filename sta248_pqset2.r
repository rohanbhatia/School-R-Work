#Chapter 6 Qs 7, 9, 17, 23, 25, 27, 33

ch6_q7 <- function() {

	print("Chapter 6 Question 7")
	max=75.1
	min=16.3
	n_cat=7

	print("A: These data are covered by an interval of what length?")
	range=max-min
	print(range)

	print("B: Using the method outlined in this section, each category will be of what length?")
	cat_length = range/n_cat+0.1
	print(cat_length)

	print("C: What is the lower boundary for the first category?")
	lower_boundary=min-0.05
	print(lower_boundary)

	print("D: What are the boundaries for each of the seven categories?")
	count = 1
	boundary = lower_boundary
	while (count < 8) {
		print(paste0("Category: ", count))
		print(paste0("Lower Boundary: ", boundary))
		boundary = boundary + cat_length
		print(paste0("Upper Boundary: ", boundary))
		count = count + 1
	}
}

ch6_q9 <- function() {

	print("Chapter 6 Question 9")
	print("A: Create a stem and leaf plot for the given data")
	x  = c(0.2, 0.5, 0.7, 1.1, 1.2, 1.2, 1.3, 1.4, 1.4, 1.4, 1.5, 1.5, 1.6, 1.6, 1.7, 1.9, 2.0, 2.1, 2.1, 2.2, 2.3, 2.5, 2.6, 2.9, 2.8, 3.0, 3.1, 3.0, 3.7, 3.7, 4.0, 4.1, 4.5, 5.1, 5.8, 1.4)
	stem(x)

	print("B: Does the data suggest that the distribution of X is skewed? If so, give the direction of the skew")
	print("Yes. The stem and leaf plot suggests the data is skewed right")
	print("")


}

ch6_q17 <- function() {

	print("Chapter 6 Question 17")
	print("Consider these datasets:")
	one = c(1, 3, 2, 2, 5, 4, 4, 3, 3)
	two = c(1, 2, 4, 1, 2, 5, 2, 5, 1, 5, 5, 3)
	print(one)
	print(two)
	print("")
	
	print("A: Find the sample mean and sample median for each data set")
	mean_one = mean(one)
	median_one = median(one)
	mean_two = mean(two)
	median_two = median(two)
	print(paste0("Dataset 1 mean: ", mean_one, " median: ", median_one))
	print(paste0("Dataset 2 mean: ", mean_two, " median: ", median_two))
	
	print("B: Find the sample range for each data set")
	range_one = max(one) - min(one)
	range_two = max(two) - min(two)
	print(paste0("Dataset1: ", range_one))
	print(paste0("Dataset2: ", range_two))
	
	print("C: Find the sample variance and sample standard deviation for each dataset")
	var_one = var(one)
	sd_one = sqrt(var_one)
	var_two = var(two)
	sd_two = sqrt(var_two)
	print(paste0("Dataset1 var: ", var_one, " sd: ", sd_one))
	print(paste0("Dataset2 var: ", var_two, " sd: ", sd_two))
	
	print("D: Would you be surprised to hear someone claim these datasets were drawn from the same population?")
	print("My answer: no, book answer: yes")
	print("")

}

ch6_q23 <- function() {

	print("Chapter 6 Question 23")
	print("Consider the data of Exercise 9")
	x  = c(0.2, 0.5, 0.7, 1.1, 1.2, 1.2, 1.3, 1.4, 1.4, 1.4, 1.5, 1.5, 1.6, 1.6, 1.7, 1.9, 2.0, 2.1, 2.1, 2.2, 2.3, 2.5, 2.6, 2.9, 2.8, 3.0, 3.1, 3.0, 3.7, 3.7, 4.0, 4.1, 4.5, 5.1, 5.8, 1.4)

	print("(a) Find the mean and median for that data")
	mean = mean(x)
	median = median(x)
	print(paste0("Mean: ", mean, " Median: ", median))

	print("(b) Find the standard deviation and variance for these data")
	variance = var(x)
	sd = sqrt(variance)
	print(paste0("Standard deviation: ", sd, " Variance: ", variance))

	print("(c) What physical measurement unit is associated with each of the statistics in parts (a) and (b)?")
	print("Mean, median, sd are in minutes, Variance is unitless")
	print("")
}

ch6_q25 <- function() {

	print("Chapter 6 Question 25")
	x = c(6.2, 5.8, 4.6, 4.9, 7.1, 5.2, 8.1, 0.2, 3.4, 4.5, 8.0, 7.9, 6.1, 5.6, 5.5, 3.1, 6.8, 4.6, 3.8, 2.6, 4.5, 4.6, 7.7, 3.8, 4.1, 6.1, 4.1, 4.4, 5.2, 1.5)

	print("(a) Construct a stem and leaf diagram for these data. Is the assumption justified that X is normally distributed?")
	stem(x)
	print("Yes")

	print("(b) Approximate pop sd via the sample sd s")
	s = sqrt(var(x))
	print(paste0("Sample sd: ", s))

	print("(c) Find the sample range for these data, and use it to approximate pop sd. Compare your result to that obtained in (b)")
	range = max(x) - min(x)
	estimate = range/4
	print(paste0("Estimated sd using range: ", estimate))
	print("This is not far off the sample sd obtained in (b)")

	print("")	
}

ch6_q27 <- function() {

	print("Chapter 6 Question 27")
	print("Let X be normally distributed with mean 0 and variance 1")

	print("(a) Verify that q3 = 0 + 0.67(1), q1 = 0 - 0.67(1)")
	q3 = qnorm(0.75, mean=0, sd=1)
	q1 = qnorm(0.25, mean=0, sd=1)
	print(paste0("q3: ", q3, "    q1: ", q1))

	print("(b) Find the interquartile range for X")
	iqr = q3 - q1
	print(iqr)

	print("(c) Verify that the inner fences for X are f1 = 0 - 2.68(1) and f3 = 0 + 2.68(1)")
	f1 = q1 - 1.5*iqr
	f3 = q3 + 1.5*iqr
	print(paste0("f1: ", f1, "    f3: ", f3))

	print("(d) Verify that the probability that X will fall beyond the inner fences is approx 0.007")
	a = pnorm(f1, mean=0, sd=1)
	b = pnorm(f3, mean=0, sd=1)
	p = 1 - b + a
	print(p)

	print("")
}

ch6_q33 <- function() {
	print("Chapter 6 Question 33")

	x = c(15, 16, 17, 18, 17, 20, 16, 17, 18, 20, 18, 18, 19, 19, 17, 21, 17, 19, 18, 17, 22)

	print("(a)")
	stem(x, scale=2)
	print("Yes")

	k = fivenum(x)
	print("(b)")
	mean = mean(x)
	median = k[3]
	print(paste0("mean: ", mean, " median: ", median))

	print("(c)")
	var = var(x)
	sd = sqrt(var)
	print(paste0("sd: ", sd, " var: ", var))

	print("(d)")
	q1 = k[2]
	q3 = k[4]
	iqr = q3-q1
	print(paste0("Q1: ", q1, " Q3: ", q3, " IQR: ", iqr))
	print("")
}



#MAIN
#ch6_q33()









