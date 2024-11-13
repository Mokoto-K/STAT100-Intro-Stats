# chapter 7

# Finding the t-distribution
# What is the t-distro of -2.10?
#dt(-2.10, 18) / 2
# we divide by 2 as we only want the left side or -2.10 and not both sides.

# What is the t-distro of 1.66  
#dt(1.65, 20) / 2

# What is the t-distro of 3 above and below
#dt(3, 2)

# What proportion of t-distro falls above -1.79 with 19 df
#1 - dt(-1.79, 19) /2

#dt(2.09, 18) / 2
#dt(1.67, 14)
#dt(2.37, 99)


pt(7.7, 18)

pt(-1.75, 24)

pt(-2.10, 18)

1 - pt(1.65, 20)

(1 - pt(3, 2)) * 2

#Exercise 7.1 
qt(0.95, 5) #a

#b
n = 20
cl = 0.98
df = n - 1
qt(0.01, df)

#c 
n = 29
cl = 0.95
df = n - 1
qt(0.975, df)

#d 
n = 12
cl = 0.99
df = n - 1
qt(0.005, df)

############ Exercise 7.3
alpha = 0.05
# Function for detecting if we reject the null hyp or not
null_hyp = function(alpha, p_value) {
  if (p_value < alpha) {
    print('We reject the null hypothesis')
  } else {
    print('We do not reject he null hypothesis')  
    
  }
}

#a
n = 11
T = 1.91
df = n - 1
p_value = (1 - pt(T, df)) * 2
null_hyp(alpha, p_value)

#b
n = 17
T = -3.45
df = n - 1
p_value = pt(T, df) * 2
null_hyp(alpha, p_value)

#c 
n = 7
T = 0.83
df = n - 1
p_value = (1 - pt(T, df)) * 2
null_hyp(alpha, p_value)

#d
n = 28
T = 2.13
df = n - 1
p_value = (1 - pt(T, df)) * 2
null_hyp(alpha, p_value)

########## EXERCISE 7.5
n = 36
lower = 18.985
upper = 21.015
difference = upper - lower
margin_error = difference / 2
sample_mean = lower + margin_error

# Confidence lvl of 95% = 0.975 (5% distributed both sides)
cl =0.975
df = n - 1
t_score = qt(cl,df)

# Margin_error = t_score * (s/sqrt(n)) - rework the equation to equal s
std = margin_error / t_score * (n**0.5)

########## EXERCISE 7.7
#a
h0 = 8 # average newyoker gets 8 hours of sleep
ha < 8 # average newyorker gets less than 8 hours of sleep

#b 
µ = 8
n = 25
df = n - 1
x = 7.73
s = 0.77
se = s / (n**0.5)
z = (x - µ) / se

#c
p_value = pt(z, df) * 2 # * 2 because z score was neg

#d
null_hyp(alpha, p_value)

#e
t_score90 = qt(0.95, df)
me = t_score90 * se
lower = x - me
upper = x + me
# Smarter way to do this is to see that 90% conf level is 0.1 alpha score
# Since p_value is 0.093 which is smaller than alpha, we reject null, therefore
# 8 wouldnt be in the interval as it would have to be outside to be rejected

################## EXERCISE 7.9
µ = 60
n = 20
s = 8
df = n - 1
se = s / (n**0.5)

# we punch in and check a t value that will return 0.025 as we are looking for 
# 0.05 t value (*2 for both sides)
p_value = pt(-2.09, df)

# we then reverse engineer (algebra) the z score equation to get the answer
z = -2.09
x1 = µ + z * se
# or
z = 2.09
x2 = µ + z * se

########## EXERCISE 7.11

#a
h0 = 5
#ha ≠ 5

µ = 5
n = 20
df = n - 1
std = 2.2
x = 4.6
cl = 0.975
se = std/ n**0.5

Tz = (x - µ) / se

p_value = pt(Tz, df) * 2
null_hyp(alpha, p_value)

#b
t_star = qt(0.975, df)
margin_error = t_star * se

upper = x + margin_error
lower = x - margin_error

######### EXERCISE 7.13
# Brute force the sum for margin of error
# margin_error = z * std / n**0.5
# 10 = 1.96  * 100 / n**0.5 # 1.96 is 95 conf level