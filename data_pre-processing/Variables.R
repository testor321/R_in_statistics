#Step 2: Variable assiment
my_var1  <- 42
my_var2  <- 35.25


my_var1 + 100
my_var1 + my_var2 - 12
my_var3  <- my_var1^2 + my_var2^2
my_var3

#Step 3: Logical opperartions
my_var3 > 200
my_var3 > 3009
my_var1 == my_var2
my_var1 != my_var2
my_var3 >= 200
my_var3 <= 200

my_new_var  <- my_var1 == my_var2


#Step 6, 7, 10, 11: Vectors
1 : 67
my_vector1  <- 1:67
my_vector2  <- c(-32, 45, 67, 12.78, 129, 0, -65)
#Создайте переменную the_best_vector, в которой хранятся числа от 1 до 5000 и затем числа от 7000 до 10000.
the_best_vector <- c(1:5000,7000:10000)

# В уже созданной переменной my_numbers сохранен вектор из 20 целых чисел.
# Ваша задача создать новую переменную my_numbers_2, в которой будет сохранен
# 2, 5, 7, 9, 12, 16 и 20 элемент вектора my_numbers.
my_numbers <- c(6,13,13,1,18,15,6,13,19,17,19,17,18,6,0,15,10,19,10,15)
my_numbers_2 <- my_numbers[c(2,5,7,9,12,16,20)]

my_vector1[1]
my_vector1[3]

my_vector2[2]

my_vector2[c(1,2,3)]
my_vector2[1:3]
my_vector2[c(1,5,6,7,10)]

my_vector1 + 10
my_vector2 + 56

my_vector2 == 0
my_vector1 > 30

x  <- 23
my_vector1 > 23
x == 23
#
my_vector1 > x

my_vector2 > 0
my_vector2[my_vector2 > 0]
my_vector2[my_vector2 < 0]
my_vector2[my_vector2 == 0]

my_vector1[my_vector1 > 20 & my_vector1 < 30]
my_numbers  <- my_vector1[my_vector1 > 20 & my_vector1 < 30]
positive_numbers  <- my_vector2[my_vector2 > 0]


v1  <- c(165, 178, 180, 181, 167, 178, 187, 167, 187)
mean_v1  <- mean(v1)
v1[v1 > mean_v1]
greater_than_mean  <- v1[v1 > mean_v1]
# В уже созданной переменной my_vector хранится вектор из 20 целых чисел.
# Найдите сумму всех элементов вектора , которые больше 10. Сохраните сумму в переменную my_sum.
# sum(x) - сумма элементов числового вектора x
my_vector <- c(8, 13, 9, 18, 7, 2, 15, 2, 8, 18, 6, 8, 9, 6, 12, 11, 3, 1, 2, 14)
my_sum <- sum(my_vector[my_vector > 10])


#Step 13: Lists and dataframes
age  <- c(16, 18, 22, 27)
is_maried  <- c(F, F, T, T)
# example
data <- list(age, is_maried)
data
data[[1]][1]
data[[2]][4]

name  <- c("Olga", "Maria", "Nastya", "Polina")

data <- list(age, is_maried, name)
data

my_data  <- data.frame(Name = name, Age = age, Status = is_maried)
my_data
typeof(my_data)

# В векторе  my_vector отберите только те наблюдения,
# которые отклоняются от среднего меньше чем на одно стандартное отклонение.
# Сохраните эти наблюдения в новую переменную my_vector_2.  
# При этом исходный вектор my_vector оставьте без изменений.

# mean(x)среднее значение вектора x
# sd(x) стандартное отклонение вектора x
# abs(n) абсолютное значение числа n 

# Найти среднее значение mean(x)
# Найти стандартное отклонение sd(x)
# Отклонение от среднего значения может быть как в одну сторону +, так в другую сторону -, значит:
# Найти mean(x) + sd(x)
# Найти mean(x) - sd(x)
# Значения mean(x) + sd(x) и mean(x) - sd(x) являются числами-границами,
# в пределах которых должны лежать искомые значения my_vector2, значит:
# Найти значения my_vector < mean(x) + sd(x)
# Найти значения my_vector > mean(x) - sd(x)
# С помощью & объединить два вектора
# (т.е. сделать пересечение множеств значений my_vector < mean(x) + sd(x) и my_vector > mean(x) - sd(x))

my_vector <- c(21, 18, 21, 19, 25, 20, 17, 17, 18, 22, 17, 18, 18, 19, 19, 27, 21, 20, 24, 17, 15, 24, 24, 29, 19, 14, 21, 17, 19, 18, 18, 20, 21, 21, 19, 19, 17, 21, 13, 17, 13, 23, 15, 23, 24, 16, 17, 25, 24, 22)

my_vector < mean(my_vector) + sd(my_vector)
my_vector > mean(my_vector) - sd(my_vector)

my_vector_2 <- my_vector[my_vector > mean(my_vector) - sd(my_vector) & my_vector < mean(my_vector) + sd(my_vector)]
my_vector_2

my_vector_2 <- my_vector[abs(my_vector - mean(my_vector)) < sd(my_vector)]
my_vector_2

mean_my_vector <- mean(my_vector)
sd_my_vector <- sd(my_vector)
my_vector_2 <- my_vector[abs(my_vector - mean_my_vector) < sd_my_vector]
my_vector_2
