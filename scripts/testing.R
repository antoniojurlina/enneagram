library(tidyverse)
library(ggthemes)

praxis <- tibble(letter = c("A", "B", "C", "D", "E", "F", "G", "H", "I"),
                 count  = c(20, 16, 15, 12, 18, 18, 14, 15, 16),
                 type   = c("Nine", "Six", "Three", "One", "Four", "Two", "Eight", "Five", "Seven"),
                 triad  = c("The Instinctive Triad", "The Thinking Triad", "The Feeling Triad",
                            "The Instinctive Triad", "The Feeling Triad", "The Feeling Triad",
                            "The Instinctive Triad", "The Thinking Triad", "The Thinking Triad"),
                 number = c(9, 6, 3, 1, 4, 2, 8, 5, 7))

ggplot(praxis, aes(x = factor(type, 
                              levels = c("One", "Two", "Three", 
                                         "Four", "Five", "Six",
                                         "Seven", "Eight", "Nine")),
                   y = count)) +
  coord_polar(start = 20*pi/180) +
  geom_col(aes(fill = triad), color = "grey40", alpha = 0.7) +
  geom_hline(yintercept = c(8, 12, 16, 20, 24), linetype = "dashed", size = 0.3, color = "grey40") +
  geom_text(aes(2.4*pi, 7.5, label = "low", vjust = -1, angle = -299), size = 3.5, color = "grey40") +
  geom_text(aes(2.4*pi, 11.5, label = "below average", vjust = -1, angle = -299), size = 3.5, color = "grey40") +
  geom_text(aes(2.4*pi, 15.5, label = "median", vjust = -1, angle = -299), size = 3.5, color = "grey40") +
  geom_text(aes(2.4*pi, 19.5, label = "above average", vjust = -1, angle = -299), size = 3.5, color = "grey40") +
  geom_text(aes(2.4*pi, 23.5, label = "high", vjust = -1, angle = -299), size = 3.5, color = "grey40") +
  scale_color_ptol() +
  scale_fill_ptol() +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(size = 13, face = "bold"),
        legend.position = "bottom")

row <- which(praxis$count == max(praxis$count))

praxis$type[row]

praxis$triad[row]

if (praxis$number[row] == 9) {
  wing1 <- 8
  wing2 <- 1
  
  if(praxis$count[praxis$number == wing1] > praxis$count[praxis$number == wing2]) {
    wing <- wing1
  } else {
    wing <- wing2
  }

} else if (praxis$number[row] == 1) {
  wing1 <- 9
  wing2 <- 2
  
  if(praxis$count[praxis$number == wing1] > praxis$count[praxis$number == wing2]) {
    wing <- wing1
  } else {
    wing <- wing2
  }
  
} else {
  wing1 <- praxis$number[row]-1
  wing2 <- praxis$number[row]+1
  
  if(praxis$count[praxis$number == wing1] > praxis$count[praxis$number == wing2]) {
    wing <- wing1
  } else {
    wing <- wing2
  }
}

praxis$count[praxis$number == wing1]

praxis

wing



  