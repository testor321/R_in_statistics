setwd("D:/OneDrive/education/R/Statistic_in_R_part1")

my_calc  <- function(x, y){
  s  <- x + y
  return(s)
}

my_calc(x = 10, y = 15)
result  <- my_calc(10, 15)

my_calc_2  <- function(x, y){
  s  <- x + y
  d  <- x - y
  return(c(s, d))
}

my_calc_2(10, 15)


my_calc_3  <- function(x, y, z){
  s  <- x + y + z
  d  <- x - y - z
  return(c(s, d))
}

my_calc_3(1, 2, 3)

my_calc_3  <- function(x, y, z = 10){
  s  <- x + y + z
  d  <- x - y - z
  return(c(s, d))
}

my_calc_3(1, 2)


############
?rnorm
# generate normal distribution
distr1  <- rnorm(100)
hist(distr1)

# insert NA into first 30th positions
distr1[1:30]  <- NA
# replace IN to "mean" value
distr1[is.na(distr1)]  <- mean(distr1, na.rm = T)

my_na_rm  <- function(x){
  if (is.numeric(x)){
    # ?shapiro.test
    stat_test  <- shapiro.test(x)
    if (stat_test$p.value > 0.05){
      x[is.na(x)]  <- mean(x, na.rm = T)
      print("NA values were replaced with mean")
    } else{
      x[is.na(x)]  <- median(x, na.rm = T)
      print("NA values were replaced with median")
    }
    return(x)
  } else{
    print("X is not numeric")
  }
}

?rnorm
# normal distribution
d1  <- rnorm(2000)
?runif
# uniform distribution
d2  <- runif(2000)

d1[1:10]  <- NA
d2[1:10]  <- NA

d1  <- my_na_rm(d1)
head(d1)

d2  <- my_na_rm(d2)
head(d2)


# Напишите функцию, которая выводит номера позиций пропущенных наблюдений в векторе.
# На вход функция получает числовой вектор с пропущенными значениями.
# Функция возвращает новый вектор с номерами позиций пропущенных значений.
# Подсказка: чтобы проверить является ли наблюдение NA, воспользуйтесь функцией is.na(), 
# кстати, функция векторизирована, и аргументом может служить вектор произвольной длинны.
# Запись x == NA ни к чему осмысленному не приведет.
# Т.к. если x это NA, то команда x == NA также вернет NA, а не TRUE!
# > my_vector <- c(1, 2, 3, NA, NA)
# > NA.position(my_vector)
# [1] 4 5

NA.position <- function(x){
  res <- which(is.na(x))
}

my_vector <- c(1, 2, 3, NA, NA)
NA.position(my_vector)

NA.position <- function(x){
  c(1:length(x))[is.na(x)]
}

NA.position <- function(x){
  # put your code here  
  n <- length(x)
  prv <- 1:n
  mm <- is.na(x)
  print(prv[mm])
  
}

# Напишите функцию NA.counter для подсчета пропущенных значений в векторе.
# 
# На вход функция  NA.counter должна принимать один аргумент - числовой вектор. 
# Функция должна возвращать количество пропущенных значений.
# 
# > my_vector <- c(1, 2, 3, NA, NA)
# > NA.counter(my_vector)
# [1] 2 

NA.counter <- function(x){
  res <- which(is.na(x))
  length(res)
}

my_vector <- c(1, 2, 3, NA, NA, 4, NA, NA)
NA.counter(my_vector)

NA.counter <- function(x){    
	return(sum(is.na(x)))
}

NA.counter <- function(x){
  r <- ifelse(is.na(x), 1, 0)
  sum(r)
}

NA.counter <- function(x){
  length(which(is.na(x) ))
}

######################

setwd("D:/OneDrive/education/R/Statistic_in_R_part1/Grants data")
#
dir(pattern = "*.csv")

#
read_data  <- function(){
  df  <- data.frame()
  number  <<- 0
  for (i in dir(pattern = "*.csv")){
    temp_df  <- read.csv(i, stringsAsFactors = F)
    df  <- rbind(temp_df, df)
    number <<- number + 1
    }
  print(paste(as.character(number), "files were combined"))
  return(df)
}

grants2 <- read_data()

# NB
# <<- - saves variables in sesion level
# <-  saves environments on funclion level only

# Advanced method without for loop

read_data_advanced <- function(){
    df <- do.call(rbind, lapply(dir(pattern = "*.csv"), 
                                read.csv, stringsAsFactors = F))
    return(df)
}

df  <- data.frame(x = factor(1:5))
df1  <- data.frame(x = factor(7:8))
str(df)
str(df1)

df3  <- rbind(df, df1)
str(df3)
table(df3$x)

# Напишите функцию filtered.sum, которая на вход получает вектор с пропущенными, 
# положительными и отрицательными значениями и возвращает сумму положительных элементов вектора.
#  >  filtered.sum(c(1, -2, 3, NA, NA))
# [1] 4 

filtered.sum <- function(x){
  return(sum(x[which(x > 0)]))
}

filtered.sum <- function(x){    
  return(sum(x[x > 0], na.rm = T))
}

filtered.sum <- function(x){
  sum(ifelse(x > 0 & !is.na(x), x, 0))
}

filtered.sum <- function(x){
  sum(replace(x, x < 0 | is.na(x), 0))
}

filtered.sum(c(1, -2, 3, NA, NA))



# Напишите функцию outliers.rm, которая находит и удаляет выбросы. 
# Для обнаружения выбросов воспользуемся самым простым способом, с которым вы не раз встречались,
# используя график Box plot. Выбросами будем считать те наблюдения, 
# которые отклоняются от 1 или 3 квартиля больше чем на 1,5 *  IQR, где  IQR  - межквартильный размах.
# На вход функция получает числовой вектор x. Функция должна возвращать модифицированный
# вектор x с удаленными выбросами. 
# Ссылка на видео с объяснением, как на графике box-plot отображаются выбросы:
# https://stepic.org/lesson/%D0%9A%D0%B2%D0%B0%D1%80%D1%82%D0%B8%D0%BB%D0%B8-%D1%80%D0%B0%D1%81%D0%BF%...
# Полезные функции:
# IQR(x) - рассчитывает межквартильный размах вектора x
# quantile(x, probs = c(0.25, 0.75)) - рассчитывает первый и третий квартиль вектора x  

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

remove_outliers <- function(x) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}


outliers.rm <- function(x){
  mod_x <<- c()
  for(i in 1:length(x)){
    q1 <- quantile(x,probs=c(0.75))
    q2 <- quantile(x,probs=c(0.25))
    if (x[i]<(q1+ 1.5 * IQR(x)) & x[i]>(q2-1.5 * IQR(x))) {
      mod_x <<- append(mod_x, x[i])
    } 
  }
  return(mod_x)
}


df <- 1:10
outliers.rm(df)

outliers.rm <- function(x){    
  q <- quantile(x, 0.25) + quantile(x, 0.75)    
  return(x[abs(x - q/2) <= 2*IQR(x)])}

outliers.rm <- function(x){
  x[x>(quantile(x, probs = 0.25)-1.5*IQR(x))& x<(quantile(x, probs = 0.75)+1.5*IQR(x))]
}

outliers.rm <- function(x)
{
  q <- quantile(x, probs = c(0.25, 0.75))
  x <- x[!(x + 1.5*IQR(x) < q[1] | x - 1.5*IQR(x) > q[2])]
  return (x)
}

outliers.rm <- function(x)(x[abs(x - quantile(x, 0.25) - IQR(x)/2) <= 2*IQR(x)])

remove_outliers(as.numeric(unlist(strsplit("16.3 8.13 22.92 19.84 22.64 22.43 27.66 12.06 21.19 24.08 12.89 16.82 27.1 15.11 30.0 21.75 17.99 20.48 25.77 18.49 16.68 22.2 17.17 11.9 17.48 7.37 18.84 27.57 21.24 26.33 19.56 17.11 11.29 12.1 16.95 15.8", " "))))

