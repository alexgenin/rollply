# 
# Some tests for a formula parser
# 


a <- data.frame(x=runif(100),
                y=runif(100),
                z=as.factor(ifelse(rnorm(100)>0,'yes','no')))
form1 <- ~ round(x) + y | z

form2 <- ~ round(x) + y 

form3 <- ~ abs(x) + y | z

parse_roll_formula(form1)
parse_roll_formula(form2)
parse_roll_formula(form3)

