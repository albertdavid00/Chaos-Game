if (!require("shiny"))
    install.packages("shiny")
if (!require("shinyRGL"))
    install.packages("shinyRGL")
if (!require("rgl"))
    install.packages("rgl")
if (!require("shinyWidgets"))
    install.packages("shinyWidgets")

library(shiny)
library(shinyRGL)
library(rgl)
library(shinyWidgets)


ui <- fluidPage(
    setBackgroundColor(color = "ghostwhite"),
    
    titlePanel("Chaos Game: Tridimensional"),
    
    sidebarLayout(
        mainPanel(rglwidgetOutput(
            'thewidget', width = "100%", height = 512
        )),
        
        sidebarPanel(
            selectizeInput(
                'shape',
                'Forma',
                list(
                    `Tetraedru` = 'tetra',
                    `Cub` = 'cube',
                    `Octaedru` = 'octa',
                    `Dodecaedru` = 'dodec'
                ),
                selected = 'tetra'
            ),
            
            sliderInput(
                "ratio",
                min = .01,
                max = .99,
                step = .01,
                value = .5,
                label = "Ratia distantei fata de varf:"
            ),
            
            sliderInput(
                "points",
                min = 5000,
                max = 50000,
                step = 500,
                value = 50000,
                label = "Numarul punctelor:",
                # animate = animationOptions(200, loop = TRUE)
            ),
            
            actionButton('gen', 'Genereaza')
        )
    )
)

tetra.gen <- function(ratio, points){
    endpoints <- matrix(ncol = 4, nrow = 4)
    
    endpoints[1, ] <- c(1,  1,  0, -1 / sqrt(2))
    endpoints[2, ] <- c(2, -1,  0, -1 / sqrt(2))
    endpoints[3, ] <- c(3,  0,  1,  1 / sqrt(2))
    endpoints[4, ] <- c(4,  0, -1,  1 / sqrt(2))
    # generam punctele intre 0 si 1
    targets <- runif(points)
    #mapam punctele la cate un endpoint
    targets[which(targets >  3/4                 )] <- 4
    targets[which(targets >  2/4 & targets <= 3/4)] <- 3
    targets[which(targets >  1/4 & targets <= 2/4)] <- 2
    targets[which(targets <= 1/4                 )] <- 1
    
    coords <- matrix(ncol = 4, nrow = (points + 1))
    # pornim cu un punct din primul cadran al figurii
    coords[1, ] <- c(runif(1) * 2 - 1, runif(1) * 2 - 1, runif(1) * 2 - 1, 0)
    
    for (current in 1:points) {
        nextP <- current + 1
        
        # cautam endpoint-ul pentru punctul curent
        target <- which(endpoints[, 1] == targets[current])
        
        # luam coordonatele endpoint-ului
        x <- endpoints[target, 2]
        y <- endpoints[target, 3]
        z <- endpoints[target, 4]
        
        # determinam noi coordonate aflate la raportul r de endpoint-ul nostru fata de punctul curent
        x.new <- ratio * x + (1 - ratio) * coords[current, 1]
        y.new <- ratio * y + (1 - ratio) * coords[current, 2]
        z.new <- ratio * z + (1 - ratio) * coords[current, 3]
        
        coords[nextP, ] <- c(x.new, y.new, z.new, 0)
    }
    
    # determinam cel mai apropiat endpoint pentru punctele noastre ca sa stim unde il vom distribui
    for (current in 1:(points+1)){
        # calculam distantele endpointurilor fata de punctele noastre
        d1 <- sqrt((coords[current,1]-endpoints[1,2])**2 + (coords[current,2]-endpoints[1,3])**2 + (coords[current,3]-endpoints[1,4])**2)
        d2 <- sqrt((coords[current,1]-endpoints[2,2])**2 + (coords[current,2]-endpoints[2,3])**2 + (coords[current,3]-endpoints[2,4])**2)
        d3 <- sqrt((coords[current,1]-endpoints[3,2])**2 + (coords[current,2]-endpoints[3,3])**2 + (coords[current,3]-endpoints[3,4])**2)
        d4 <- sqrt((coords[current,1]-endpoints[4,2])**2 + (coords[current,2]-endpoints[4,3])**2 + (coords[current,3]-endpoints[4,4])**2)
        
        if (min(d1,d2,d3,d4)==d1) coords[current,4]<-1
        if (min(d1,d2,d3,d4)==d2) coords[current,4]<-2
        if (min(d1,d2,d3,d4)==d3) coords[current,4]<-3
        if (min(d1,d2,d3,d4)==d4) coords[current,4]<-4
    }
    
    return(list(endpoints, coords))
}


cube.gen <- function(ratio, points){
    endpoints <- matrix(ncol = 4, nrow = 8)
    
    endpoints[1, ] <- c(1, -1, -1, -1)
    endpoints[2, ] <- c(2,  1, -1, -1)
    endpoints[3, ] <- c(3,  1,  1, -1)
    endpoints[4, ] <- c(4, -1,  1, -1)
    endpoints[5, ] <- c(5, -1, -1,  1)
    endpoints[6, ] <- c(6,  1, -1,  1)
    endpoints[7, ] <- c(7,  1,  1,  1)
    endpoints[8, ] <- c(8, -1,  1,  1)
    
    
    targets <- runif(points)
    
    targets[which(targets >  7/8                 )] <- 8
    targets[which(targets >  6/8 & targets <= 7/8)] <- 7
    targets[which(targets >  5/8 & targets <= 6/8)] <- 6
    targets[which(targets >  4/8 & targets <= 5/8)] <- 5
    targets[which(targets >  3/8 & targets <= 4/8)] <- 4
    targets[which(targets >  2/8 & targets <= 3/8)] <- 3
    targets[which(targets >  1/8 & targets <= 2/8)] <- 2
    targets[which(targets <= 1/8                 )] <- 1
    
    coords <- matrix(ncol = 4, nrow = (points + 1))
    
    coords[1,] <- c(runif(1) * 2 - 1, runif(1) * 2 - 1, runif(1) * 2 - 1, 0)
    
    for (current in 1:points) {
        nextP <- current + 1
        
        target <- which(endpoints[, 1] == targets[current])
        
        x <- endpoints[target, 2]
        y <- endpoints[target, 3]
        z <- endpoints[target, 4]
        
        x.new <- ratio * x + (1 - ratio) * coords[current, 1]
        y.new <- ratio * y + (1 - ratio) * coords[current, 2]
        z.new <- ratio * z + (1 - ratio) * coords[current, 3]
        
        coords[nextP, ] <- c(x.new, y.new, z.new, 0)
    }
    
    for (current in 1:(points+1)){
        d1 <- sqrt((coords[current,1]-endpoints[1,2])**2 + (coords[current,2]- endpoints[1,3])**2 + (coords[current,3]- endpoints[1,4])**2)
        d2 <- sqrt((coords[current,1]-endpoints[2,2])**2 + (coords[current,2]- endpoints[2,3])**2 + (coords[current,3]- endpoints[2,4])**2)
        d3 <- sqrt((coords[current,1]-endpoints[3,2])**2 + (coords[current,2]- endpoints[3,3])**2 + (coords[current,3]- endpoints[3,4])**2)
        d4 <- sqrt((coords[current,1]-endpoints[4,2])**2 + (coords[current,2]- endpoints[4,3])**2 + (coords[current,3]- endpoints[4,4])**2)
        d5 <- sqrt((coords[current,1]-endpoints[5,2])**2 + (coords[current,2]- endpoints[5,3])**2 + (coords[current,3]- endpoints[5,4])**2)
        d6 <- sqrt((coords[current,1]-endpoints[6,2])**2 + (coords[current,2]- endpoints[6,3])**2 + (coords[current,3]- endpoints[6,4])**2)
        d7 <- sqrt((coords[current,1]-endpoints[7,2])**2 + (coords[current,2]- endpoints[7,3])**2 + (coords[current,3]- endpoints[7,4])**2)
        d8 <- sqrt((coords[current,1]-endpoints[8,2])**2 + (coords[current,2]- endpoints[8,3])**2 + (coords[current,3]- endpoints[8,4])**2)
        
        if (min(d1,d2,d3,d4,d5,d6,d7,d8)==d1) coords[current,4]<-1
        if (min(d1,d2,d3,d4,d5,d6,d7,d8)==d2) coords[current,4]<-2
        if (min(d1,d2,d3,d4,d5,d6,d7,d8)==d3) coords[current,4]<-3
        if (min(d1,d2,d3,d4,d5,d6,d7,d8)==d4) coords[current,4]<-4
        if (min(d1,d2,d3,d4,d5,d6,d7,d8)==d5) coords[current,4]<-5
        if (min(d1,d2,d3,d4,d5,d6,d7,d8)==d6) coords[current,4]<-6
        if (min(d1,d2,d3,d4,d5,d6,d7,d8)==d7) coords[current,4]<-7
        if (min(d1,d2,d3,d4,d5,d6,d7,d8)==d8) coords[current,4]<-8
    }
    
    return(list(endpoints, coords))
}

octa.gen <- function(ratio, points){
    endpoints <- matrix(ncol = 4, nrow = 6)
    
    endpoints[1, ] <- c(1, 1, 0, 0)
    endpoints[2, ] <- c(2,  -1, 0, 0)
    endpoints[3, ] <- c(3,  0,  1, 0)
    endpoints[4, ] <- c(4, 0,  -1, 0)
    endpoints[5, ] <- c(5,  0,  0,  1)
    endpoints[6, ] <- c(6,  0,  0, -1)
    
    
    targets <- runif(points)
    
    targets[which(targets >  5/ 6                 )] <- 6
    targets[which(targets >  4/ 6 & targets <= 5/6)] <- 5
    targets[which(targets >  3/ 6 & targets <= 4/6)] <- 4
    targets[which(targets >  2/ 6 & targets <= 3/6)] <- 3
    targets[which(targets >  1/ 6 & targets <= 2/6)] <- 2
    targets[which(targets <= 1/6                  )] <- 1
    
    coords <- matrix(ncol = 4, nrow = (points + 1))
    
    coords[1, ] <- c(runif(1) * 2 - 1, runif(1) * 2 - 1, runif(1) * 2 - 1, 0)
    
    for (current in 1:points) {
        nextP <- current + 1
        
        target <- which(endpoints[, 1] == targets[current])
        
        x <- endpoints[target, 2]
        y <- endpoints[target, 3]
        z <- endpoints[target, 4]
        
        x.new <- ratio * x + (1 - ratio) * coords[current, 1]
        y.new <- ratio * y + (1 - ratio) * coords[current, 2]
        z.new <- ratio * z + (1 - ratio) * coords[current, 3]
        
        coords[nextP, ] <- c(x.new, y.new, z.new, 0)
    }
    
    for (current in 1:(points+1)){
        d1 <- sqrt((coords[current,1]-endpoints[1,2])**2 + (coords[current,2]- endpoints[1,3])**2 + (coords[current,3]- endpoints[1,4])**2)
        d2 <- sqrt((coords[current,1]-endpoints[2,2])**2 + (coords[current,2]- endpoints[2,3])**2 + (coords[current,3]- endpoints[2,4])**2)
        d3 <- sqrt((coords[current,1]-endpoints[3,2])**2 + (coords[current,2]- endpoints[3,3])**2 + (coords[current,3]- endpoints[3,4])**2)
        d4 <- sqrt((coords[current,1]-endpoints[4,2])**2 + (coords[current,2]- endpoints[4,3])**2 + (coords[current,3]- endpoints[4,4])**2)
        d5 <- sqrt((coords[current,1]-endpoints[5,2])**2 + (coords[current,2]- endpoints[5,3])**2 + (coords[current,3]- endpoints[5,4])**2)
        d6 <- sqrt((coords[current,1]-endpoints[6,2])**2 + (coords[current,2]- endpoints[6,3])**2 + (coords[current,3]- endpoints[6,4])**2)
        
        if (min(d1,d2,d3,d4,d5,d6)==d1) coords[current,4]<-1
        if (min(d1,d2,d3,d4,d5,d6)==d2) coords[current,4]<-2
        if (min(d1,d2,d3,d4,d5,d6)==d3) coords[current,4]<-3
        if (min(d1,d2,d3,d4,d5,d6)==d4) coords[current,4]<-4
        if (min(d1,d2,d3,d4,d5,d6)==d5) coords[current,4]<-5
        if (min(d1,d2,d3,d4,d5,d6)==d6) coords[current,4]<-6
    }
    
    return(list(endpoints,coords))
}


dodec.gen <- function(ratio, points){
    endpoints <- matrix(ncol = 4, nrow = 20)
    
    psi <- (1 + sqrt(5)) / 2
    
    endpoints[1, ]  <-  c(1,       1,     -1,      1)
    endpoints[2, ]  <-  c(2,       1,      1,     -1)
    endpoints[3, ]  <-  c(3,     psi,      0, -1/psi)
    endpoints[4, ]  <-  c(4,       1,      1,      1)
    endpoints[5, ]  <-  c(5,      -1,      1,      1)
    endpoints[6, ]  <-  c(6,      -1,      1,     -1)
    endpoints[7, ]  <-  c(7,   1/psi,    psi,      0)
    endpoints[8, ]  <-  c(8,       0,  1/psi,    psi)
    endpoints[9, ]  <-  c(9,    -psi,      0, -1/psi)
    endpoints[10, ] <-  c(10,      0, -1/psi,   -psi)
    endpoints[11, ] <-  c(11,      0, -1/psi,    psi)
    endpoints[12, ] <-  c(12,      0,  1/psi,   -psi)
    endpoints[13, ] <-  c(13, -1/psi,    psi,      0)
    endpoints[14, ] <-  c(14,   -psi,      0,  1/psi)
    endpoints[15, ] <-  c(15,     -1,     -1,      1)
    endpoints[16, ] <-  c(16, -1/psi,   -psi,      0)
    endpoints[17, ] <-  c(17,    psi,      0,  1/psi)
    endpoints[18, ] <-  c(18,      1,     -1,     -1)
    endpoints[19, ] <-  c(19,  1/psi,   -psi,      0)
    endpoints[20, ] <-  c(20,     -1,     -1,     -1)
    
    
    targets <- runif(points)
    
    targets[which(targets > 19/20                             )]<- 20
    targets[which(targets > 18/20 & targets <= 19/20)]<- 19
    targets[which(targets > 17/20 & targets <= 18/20)]<- 18
    targets[which(targets > 16/20 & targets <= 17/20)]<- 17
    targets[which(targets > 15/20 & targets <= 16/20)]<- 16
    targets[which(targets > 14/20 & targets <= 15/20)]<- 15
    targets[which(targets > 13/20 & targets <= 14/20)]<- 14
    targets[which(targets > 12/20 & targets <= 13/20)]<- 13
    targets[which(targets > 11/20 & targets <= 12/20)]<- 12
    targets[which(targets > 10/20 & targets <= 11/20)]<- 11
    targets[which(targets >  9/20 & targets <= 10/20)]<- 10
    targets[which(targets >  8/20 & targets <=  9/20)]<- 9
    targets[which(targets >  7/20 & targets <=  8/20)]<- 8
    targets[which(targets >  6/20 & targets <=  7/20)]<- 7
    targets[which(targets >  5/20 & targets <=  6/20)]<- 6
    targets[which(targets >  4/20 & targets <=  5/20)]<- 5
    targets[which(targets >  3/20 & targets <=  4/20)]<- 4
    targets[which(targets >  2/20 & targets <=  3/20)]<- 3
    targets[which(targets >  1/20 & targets <=  2/20)]<- 2
    targets[which(targets <= 1/20                             )]<- 1
    
    coords <- matrix(ncol = 4, nrow = (points + 1))
    
    coords[1, ] <- c(runif(1) * 2 - 1, runif(1) * 2 - 1, runif(1) * 2 - 1, 0)
    
    for (current in 1:points) {
        nextP <- current + 1
        
        target <- which(endpoints[, 1] == targets[current])
        
        x <- endpoints[target, 2]
        y <- endpoints[target, 3]
        z <- endpoints[target, 4]
        
        x.new <- ratio * x + (1 - ratio) * coords[current, 1]
        y.new <- ratio * y + (1 - ratio) * coords[current, 2]
        z.new <- ratio * z + (1 - ratio) * coords[current, 3]
        
        coords[nextP, ] <- c(x.new, y.new, z.new, 0)
    }
    
    for (current in 1:(points+1)){
        d1 <- sqrt((coords[current,1]-endpoints[1,2])**2 + (coords[current,2]- endpoints[1,3])**2 + (coords[current,3]- endpoints[1,4])**2)
        d2 <- sqrt((coords[current,1]-endpoints[2,2])**2 + (coords[current,2]- endpoints[2,3])**2 + (coords[current,3]- endpoints[2,4])**2)
        d3 <- sqrt((coords[current,1]-endpoints[3,2])**2 + (coords[current,2]- endpoints[3,3])**2 + (coords[current,3]- endpoints[3,4])**2)
        d4 <- sqrt((coords[current,1]-endpoints[4,2])**2 + (coords[current,2]- endpoints[4,3])**2 + (coords[current,3]- endpoints[4,4])**2)
        d5 <- sqrt((coords[current,1]-endpoints[5,2])**2 + (coords[current,2]- endpoints[5,3])**2 + (coords[current,3]- endpoints[5,4])**2)
        d6 <- sqrt((coords[current,1]-endpoints[6,2])**2 + (coords[current,2]- endpoints[6,3])**2 + (coords[current,3]- endpoints[6,4])**2)
        d7 <- sqrt((coords[current,1]-endpoints[7,2])**2 + (coords[current,2]- endpoints[7,3])**2 + (coords[current,3]- endpoints[7,4])**2)
        d8 <- sqrt((coords[current,1]-endpoints[8,2])**2 + (coords[current,2]- endpoints[8,3])**2 + (coords[current,3]- endpoints[8,4])**2)
        d9 <- sqrt((coords[current,1]-endpoints[9,2])**2 + (coords[current,2]- endpoints[9,3])**2 + (coords[current,3]- endpoints[9,4])**2)
        d10<- sqrt((coords[current,1]-endpoints[10,2])**2 + (coords[current,2]-endpoints[10,3])**2 + (coords[current,3]-endpoints[10,4])**2)
        d11<- sqrt((coords[current,1]-endpoints[11,2])**2 + (coords[current,2]-endpoints[11,3])**2 + (coords[current,3]-endpoints[11,4])**2)
        d12<- sqrt((coords[current,1]-endpoints[12,2])**2 + (coords[current,2]-endpoints[12,3])**2 + (coords[current,3]-endpoints[12,4])**2)
        d13<- sqrt((coords[current,1]-endpoints[13,2])**2 + (coords[current,2]-endpoints[13,3])**2 + (coords[current,3]-endpoints[13,4])**2)
        d14<- sqrt((coords[current,1]-endpoints[14,2])**2 + (coords[current,2]-endpoints[14,3])**2 + (coords[current,3]-endpoints[14,4])**2)
        d15<- sqrt((coords[current,1]-endpoints[15,2])**2 + (coords[current,2]-endpoints[15,3])**2 + (coords[current,3]-endpoints[15,4])**2)
        d16<- sqrt((coords[current,1]-endpoints[16,2])**2 + (coords[current,2]-endpoints[16,3])**2 + (coords[current,3]-endpoints[16,4])**2)
        d17<- sqrt((coords[current,1]-endpoints[17,2])**2 + (coords[current,2]-endpoints[17,3])**2 + (coords[current,3]-endpoints[17,4])**2)
        d18<- sqrt((coords[current,1]-endpoints[18,2])**2 + (coords[current,2]-endpoints[18,3])**2 + (coords[current,3]-endpoints[18,4])**2)
        d19<- sqrt((coords[current,1]-endpoints[19,2])**2 + (coords[current,2]-endpoints[19,3])**2 + (coords[current,3]-endpoints[19,4])**2)
        d20<- sqrt((coords[current,1]-endpoints[20,2])**2 + (coords[current,2]-endpoints[20,3])**2 + (coords[current,3]-endpoints[20,4])**2)
        
        if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d1) coords[current,4]<-1
        if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d2) coords[current,4]<-2
        if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d3) coords[current,4]<-3
        if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d4) coords[current,4]<-4
        if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d5) coords[current,4]<-5
        if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d6) coords[current,4]<-6
        if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d7) coords[current,4]<-7
        if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d8) coords[current,4]<-8
        if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d9) coords[current,4]<-9
        if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d10) coords[current,4]<-10
        if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d11) coords[current,4]<-11
        if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d12) coords[current,4]<-12
        if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d13) coords[current,4]<-13
        if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d14) coords[current,4]<-14
        if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d15) coords[current,4]<-15
        if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d16) coords[current,4]<-16
        if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d17) coords[current,4]<-17
        if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d18) coords[current,4]<-18
        if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d19) coords[current,4]<-19
        if (min(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20)==d20) coords[current,4]<-20
    }
    
    return(list(endpoints,coords))
}

server <- function(input, output, session) {
    all.list <- reactive({
        if (input$shape == "tetra") {
            return(tetra.gen(input$ratio*(input$gen>-1), input$points*(input$gen>-1)))
        }
        if (input$shape == "cube") {
            return(cube.gen(input$ratio*(input$gen>-1), input$points*(input$gen>-1)))
        }
        if (input$shape == "octa") {
            return(octa.gen(input$ratio*(input$gen>-1), input$points*(input$gen>-1)))
        }
        if (input$shape == "dodec") {
            return(dodec.gen(input$ratio*(input$gen>-1), input$points*(input$gen>-1)))
        }
    })
    
    output$thewidget <- renderRglwidget({
        try(close3d())
        endpoints <- all.list()[[1]]
        coords    <- all.list()[[2]]
        
        if (max(coords[, 4] == 4)) {
            set1 <- coords[coords[, 4] == 1, 1:3]
            set2 <- coords[coords[, 4] == 2, 1:3]
            set3 <- coords[coords[, 4] == 3, 1:3]
            set4 <- coords[coords[, 4] == 4, 1:3]
            
            points3d(set1[, 1], set1[, 2], set1[, 3], size = 1.25, col = "red")
            points3d(set2[, 1], set2[, 2], set2[, 3], size = 1.25, col = "yellow")
            points3d(set3[, 1], set3[, 2], set3[, 3], size = 1.25, col = "blue")
            points3d(set4[, 1], set4[, 2], set4[, 3], size = 1.25, col = "green")
            
        }
        
        if (max(coords[, 4] == 6)) {
            set1 <- coords[coords[, 4] == 1, 1:3]
            set2 <- coords[coords[, 4] == 2, 1:3]
            set3 <- coords[coords[, 4] == 3, 1:3]
            set4 <- coords[coords[, 4] == 4, 1:3]
            set5 <- coords[coords[, 4] == 5, 1:3]
            set6 <- coords[coords[, 4] == 6, 1:3]
            
            points3d(set1[, 1], set1[, 2], set1[, 3], size = 1.25, col = "red")
            points3d(set2[, 1], set2[, 2], set2[, 3], size = 1.25, col = "yellow")
            points3d(set3[, 1], set3[, 2], set3[, 3], size = 1.25, col = "blue")
            points3d(set4[, 1], set4[, 2], set4[, 3], size = 1.25, col = "green")
            points3d(set5[, 1], set5[, 2], set5[, 3], size = 1.25, col = "purple")
            points3d(set6[, 1], set6[, 2], set6[, 3], size = 1.25, col = "pink")
            
        }
        
        if (max(coords[, 4] == 8)) {
            set1 <- coords[coords[, 4] == 1, 1:3]
            set2 <- coords[coords[, 4] == 2, 1:3]
            set3 <- coords[coords[, 4] == 3, 1:3]
            set4 <- coords[coords[, 4] == 4, 1:3]
            set5 <- coords[coords[, 4] == 5, 1:3]
            set6 <- coords[coords[, 4] == 6, 1:3]
            set7 <- coords[coords[, 4] == 7, 1:3]
            set8 <- coords[coords[, 4] == 8, 1:3]
            
            points3d(set1[, 1], set1[, 2], set1[, 3], size = 1.25, col = "red")
            points3d(set2[, 1], set2[, 2], set2[, 3], size = 1.25, col = "yellow")
            points3d(set3[, 1], set3[, 2], set3[, 3], size = 1.25, col = "blue")
            points3d(set4[, 1], set4[, 2], set4[, 3], size = 1.25, col = "green")
            points3d(set5[, 1], set5[, 2], set5[, 3], size = 1.25, col = "purple")
            points3d(set6[, 1], set6[, 2], set6[, 3], size = 1.25, col = "pink")
            points3d(set7[, 1], set7[, 2], set7[, 3], size = 1.25, col = "cyan")
            points3d(set8[, 1], set8[, 2], set8[, 3], size = 1.25, col = "orange")
        }
        
        if (max(coords[, 4] == 20)) {
            set1  <- coords[coords[, 4] == 1,  1:3]
            set2  <- coords[coords[, 4] == 2,  1:3]
            set3  <- coords[coords[, 4] == 3,  1:3]
            set4  <- coords[coords[, 4] == 4,  1:3]
            set5  <- coords[coords[, 4] == 5,  1:3]
            set6  <- coords[coords[, 4] == 6,  1:3]
            set7  <- coords[coords[, 4] == 7,  1:3]
            set8  <- coords[coords[, 4] == 8,  1:3]
            set9  <- coords[coords[, 4] == 9,  1:3]
            set10 <- coords[coords[, 4] == 10, 1:3]
            set11 <- coords[coords[, 4] == 11, 1:3]
            set12 <- coords[coords[, 4] == 12, 1:3]
            set13 <- coords[coords[, 4] == 13, 1:3]
            set14 <- coords[coords[, 4] == 14, 1:3]
            set15 <- coords[coords[, 4] == 15, 1:3]
            set16 <- coords[coords[, 4] == 16, 1:3]
            set17 <- coords[coords[, 4] == 17, 1:3]
            set18 <- coords[coords[, 4] == 18, 1:3]
            set19 <- coords[coords[, 4] == 19, 1:3]
            set20 <- coords[coords[, 4] == 20, 1:3]
            
            points3d(set1 [, 1], set1 [, 2], set1 [, 3], size = 1.25, col = "red")
            points3d(set2 [, 1], set2 [, 2], set2 [, 3], size = 1.25, col = "yellow")
            points3d(set3 [, 1], set3 [, 2], set3 [, 3], size = 1.25, col = "blue")
            points3d(set4 [, 1], set4 [, 2], set4 [, 3], size = 1.25, col = "green")
            points3d(set5 [, 1], set5 [, 2], set5 [, 3], size = 1.25, col = "purple")
            points3d(set6 [, 1], set6 [, 2], set6 [, 3], size = 1.25, col = "pink")
            points3d(set7 [, 1], set7 [, 2], set7 [, 3], size = 1.25, col = "cyan")
            points3d(set8 [, 1], set8 [, 2], set8 [, 3], size = 1.25, col = "orange")
            points3d(set9 [, 1], set9 [, 2], set9 [, 3], size = 1.25, col = "black")
            points3d(set10[, 1], set10[, 2], set10[, 3], size = 1.25, col = "grey")
            points3d(set11[, 1], set11[, 2], set11[, 3], size = 1.25, col = "light green")
            points3d(set12[, 1], set12[, 2], set12[, 3], size = 1.25, col = "orange")
            points3d(set13[, 1], set13[, 2], set13[, 3], size = 1.25, col = "black")
            points3d(set14[, 1], set14[, 2], set14[, 3], size = 1.25, col = "cyan")
            points3d(set15[, 1], set15[, 2], set15[, 3], size = 1.25, col = "purple")
            points3d(set16[, 1], set16[, 2], set16[, 3], size = 1.25, col = "pink")
            points3d(set17[, 1], set17[, 2], set17[, 3], size = 1.25, col = "green")
            points3d(set18[, 1], set18[, 2], set18[, 3], size = 1.25, col = "blue")
            points3d(set19[, 1], set19[, 2], set19[, 3], size = 1.25, col = "yellow")
            points3d(set20[, 1], set20[, 2], set20[, 3], size = 1.25, col = "red")
        }
        # facem vizibile endpoint-urile
        points3d(endpoints[, 2], endpoints[, 3], endpoints[, 4], size = 6, col = "red")
        rglwidget()
    })
}

shinyApp(ui = ui, server = server)
