library(tidyverse)
a <- read.delim("clipboard",dec = ",")

names(a) <- c(as.character(0:20))

b <- c(2,7.5,12.5,17.5,22.5,27.5,32.5,37.5,42.5,57.5,
      52.5,57.5,62.5,67.5,72.5,77.5,82.5,87.5,92.5,97.5)

x <- t(a)
x <- as.data.frame(x)
m <- x$V4[2:21]
media <- as.numeric(m)
x <- as.data.frame(cbind(media,b))
x$index <- as.character(1:20)
x$media_2 <- x$media/6

ggplot(data = x, mapping = aes(x=b,
                               y=media_2,label=index), 
       col="gray")+
  geom_line(col="gray")+
  geom_point(col="gray2")+
  theme_minimal()+
  geom_vline(xintercept = c(47.5,50,52.5),
             col="red")+
  geom_hline(yintercept = 1.536667,
             col="red")+
  labs(x = "Distancia [cm]",
       y = "Periodo médio [s]",
       title = "Variação do periodo ao longo dos furos",
       caption = "Os autores, 2021")+
  geom_text(vjust = -0.5)


x$T_quadrado <- x$media * x$media

regressão <- lm(media~T_quadrado,
                x)
plot(x=x$T_quadrado, y= x$media)
abline(regressão)
par(mfrow = c(2, 2))
plot(regressão)

x$media
x$period_1_osc <- x$media/6

function(d){
  vec <- vector()
  for (i in 1:length(d)) {
    vec[i] <- (d^2)- (((9.22^2)*9.81)/(4*(pi^2))+R^2;
  }
}

raizes_parabol_1 <- (4* pi^2 *(48*10^-2 + 17.5*10^-2))/(1.541667^2)
raizes_parabol_2 <- (4* pi^2 *(46.75*10^-2 + 16.25*10^-2))/(1.541667^2)

(x$media_2[1]+x$media_2[7])/2

pen_eq <- ((1.541667^2)*raizes_parabol_1)/(4*pi^2)

dlm <- ((1.45^2)*(10.4))/(8*(pi^2))

(8*pi^2*0.277^2)/(1.45^2)
(2*pi*dlm/(1.45)^2)

48/100

sqrt(0.48*0.175)


I <- (1.107*((0.29^2)+(0.02^2)))
sd(x$b,
   T)

sd(x$media,na.rm = T)

periodo <- x$media[5]

d <- 0.277 

sqrt((((4*(pi^2)*(2*d^2))/(d*periodo^3))^2+((4*(pi^2)*(2*d^2))/(periodo^2*d^2))^2))
     