#Assignment 3
#Ana-Maria Barosan 12545619
#Part 2

#Q3.2.1.
remind_me <- function() {
  shopping_list <- c("milk", "fruit", "eggs", "orange juice")
  return(shopping_list)
}
remind_me()
#https://github.com/anamarb/programming.git

cheat <- function(exercise_num) {
  if (exercise_num == 1) {
    grades <-
      rnorm(60, 7, 1) #about 60 students in the course, with the general mean in the Research Master around 7
    return(hist(grades))#plot the grades in histogram
  } else if (exercise_num == 2) {
    temperature <-
      read.csv(
        url(
          "https://raw.githubusercontent.com/hannesrosenbusch/schiphol_class/master/schiphol_data.csv"
        )
      ) #read data from a cvs file online
    plot(temperature$TMAX,
         main = "Scatterplot Temperature Schiphol",
         xlab = "time ",
         ylab = "TMAX") #plot the specific variable on the scatter plot and assign names for the x and y labels
  } else if (exercise_num == 3) {
    install.packages("ggplot2")
    library("ggplot2")
    install.packages("tidyverse")
    
    install.packages("titanic")
    library("titanic")
    titanic <- titanic_train
    p <-
      ggplot(data = titanic, aes(x = as.factor(Sex), fill = as.factor(Survived))) +
      geom_bar() +
      scale_fill_brewer(palette = "Set1") +
      labs(x = "Sex", y = "Count", fill = "How did it go?")
    p + scale_fill_discrete(name = "How did it go?", labels = c("dead", "alive"))
  } else {
    return("Invalid exercise number")
  }
}

#test: cheat(1)

#Q3.2.2
#  install.packages("plotrix")
library(plotrix)
make_art <- function(seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  library(plotrix)
  # Create a blank plot
  plot(
    0,
    0,
    type = "n",
    xlim = c(0, 10),
    ylim = c(0, 10),
    xlab = "",
    ylab = "",
    axes = FALSE
  )
  
  # Add random shapes to the plot
  for (i in 1:20) {
    x1 <- runif(1, 0, 10)
    y1 <- runif(1, 0, 10)
    x2 <- runif(1, 0, 10)
    y2 <- runif(1, 0, 10)
    shape <- sample(c("circle", "rectangle", "polygon"), 1)
    if (shape == "circle") {
      r <- runif(1, 0, 1)
      draw.circle(x1, y1, r)
    } else if (shape == "rectangle") {
      h <- runif(1, 0, 1)
      w <- runif(1, 0, 1)
      rect(x1, y1, x2, y2, col = hsv(runif(1), 0.5, 1))
    } else if (shape == "polygon") {
      n <- sample(3:6, 1)
      x <- runif(n, 0, 10)
      y <- runif(n, 0, 10)
      polygon(x, y, col = hsv(runif(1), 0.5, 1))
    }
  }
}

#make_art()
#make_art(12) test

