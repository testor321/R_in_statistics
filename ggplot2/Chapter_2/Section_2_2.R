setwd("D:/OneDrive/education/R/ggplot2/Chapter_2")
# В этом файле приведены примеры кода из раздела 2.2 книги
# Мастицкий С. Э. (2016) Визуализация данных с помощью ggplot2. - М.: ДМК Пресс
# Во всех примерах предполагается, что пакет ggplot2 уже загружен с помощью
# команды library(ggplot2), а таблица dreissena находится в рабочей среде R
# (см. разд. 1.4)

library(ggplot2)

?qplot
# Arguments
# x, y, ... :	 Aesthetics passed into each layer
# data :	 Data frame to use (optional). If not specified, will create one, extracting vectors from the current environment.
# facets :	 faceting formula to use. Picks facet_wrap() or facet_grid() depending on whether the formula is one- or two-sided
# margins :	See facet_grid: display marginal facets?
# geom :	Character vector specifying geom(s) to draw. Defaults to "point" if x and y are specified, and "histogram" if only x is specified.
# xlim, ylim :	X and y axis limits
# log :	Which variables to log transform ("x", "y", or "xy")
# main, xlab, ylab :	Character vector (or expression) giving plot title, x axis label, and y axis label respectively.
# asp :	The y/x aspect ratio
# stat, position : DEPRECATED

# ------------------------------- Рисунок 2.1 ----------------------------------
#
qplot(x = Length, y = Infection, data = dreissena)

# ------------------------------- Рисунок 2.2 ----------------------------------
qplot(x = log(Length),
      y = log(Infection + 1), data = dreissena)
# поскольку некоторые значения Infection равны 0,
# логарифмирование выполнено для (Infection + 1)
#

# ------------------------------- Рисунок 2.3 ----------------------------------
qplot(log(Length), log(Infection + 1), data = dreissena, colour = Month)
qplot(log(Length), log(Infection + 1), data = dreissena, shape = Lake)

# ------------------------------- Рисунок 2.4 ----------------------------------
qplot(Length, Infection, alpha = I(1/2), data = dreissena)
qplot(Length, Infection, alpha = I(1/4), data = dreissena)
qplot(Length, Infection, alpha = I(1/8), data = dreissena)

# ------------------------------- Рисунок 2.5 ----------------------------------
qplot(log(Length), log(Infection + 1), data = dreissena,
      size = Day, alpha = I(0.25), colour = I("magenta"))

