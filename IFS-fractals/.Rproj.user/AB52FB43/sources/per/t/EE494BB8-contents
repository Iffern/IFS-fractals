#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(foreach)
library(ggplot2)
library(gganimate)
library(gifski)
library(colourpicker)

ui <- fluidPage(

    # Application title
    titlePanel("IFS Fractals Generator"),

    
    sidebarLayout(
        sidebarPanel(
            selectInput("fractal",h3("Pick one of the excemple fractals..."),
                        choices=list("I want to pick coefficients by myself!","Barnsley Fern", "Heighway Dragon","Sierpinski Triangle",
                                     "Sierpinski Carpet","Sierpinski Pentagon","Cantor Labirynth","Tree",
                                     "Ice Crystal","Feather","Twig"),selected="I want to pick coefficients by myself!"),
            h3("...or choose coefficients by yourself!"),
            h4("x'=ax+by+c",align="center"),
            h4("y'=dx+ey+f",align="center"),
            splitLayout(
            numericInput("a","a",0.0,step=0.001),
            numericInput("b","b",0.0,step=0.001),
            numericInput("c","c",0.0,step=0.001)),
            splitLayout(
                numericInput("d","d",0.0,step=0.001),
                numericInput("e","e",0.0,step=0.001),
                numericInput("f","f",0.0,step=0.001)),
            numericInput("reps",h3("Number of iterations"),value=20000),
            colourInput("color",h3("Select color"),value="black"),
            actionButton("start","Start")
        ),

        
        mainPanel(
           imageOutput("selected_fractal", height = "800px")
        )
    )
)


server <- function(input, output,session) {
    observeEvent(input$start, {
    output$selected_fractal <- renderImage({
        
       points = data.frame()
       if(input$fractal=="I want to pick coefficients by myself!"){
           fractal <- function(reps){
               x <- 0
               y <- 0
               idx <- 0
               points = data.frame("X"=x,"Y"=y,"order"=idx)
               for(p in 1:reps){
                   xp <- input$a*x + input$b*y + input$c
                   yp <- input$d*x + input$e*y + input$f
                   
                   idx <- idx+1
                   points[nrow(points)+1,] = c(xp,yp,idx)
                   x <- xp
                   y <- yp
               }
               points
           }
           points <- fractal(input$reps)
       }
       if(input$fractal=="Barnsley Fern"){
           fractal_fern <- function(reps){
               prob <- runif(reps)
               x <- 0
               y <- 0
               idx <- 0
               points = data.frame("X"=x,"Y"=y,"order"=idx)
               for(p in prob){
                   xp <- 0
                   yp <- 0
               if (p <= 0.01) {
                   xp <- 0
                   yp <- 0.16*y
               } else if (p <= 0.86) {
                   xp <- 0.85*x+0.04*y
                   yp <- -0.04*x+0.85*y+1.6
               } else if (p <= 0.93) {
                   xp <- -0.15*x + 0.28*y
                   yp <- 0.26*x + 0.24*y + 0.44
               } else {
                   xp <- 0.2*x - 0.26*y
                   yp <- 0.23*x + 0.22*y + 1.6
               }
               idx <- idx+1
               points[nrow(points)+1,] = c(xp,yp,idx)
               x <- xp
               y <- yp
               }
               points
           }
           points <- fractal_fern(input$reps)
       }
       else if(input$fractal=="Heighway Dragon"){
           dragon_fractal <- function(reps){
               prob <- runif(reps)
               x <- 0
               y <- 0
               idx <- 0
               points = data.frame("X"=x,"Y"=y,"order"=idx)
               for(p in prob){
                   if(p<0.5){
                        xp <- 0.5*x + 0.5*y - 0.27
                        yp <- -0.5*x + 0.5*y
                   }
                   else{
                       xp <- -0.5*x + 0.5*y + 0.14 
                       yp <- -0.5*x - 0.5*y
                   }
                   idx <- idx+1
                   points[nrow(points)+1,] = c(xp,yp,idx)
                   x <- xp
                   y <- yp
               }
               points
           }
           points <- dragon_fractal(input$reps)
       }
        else if(input$fractal=="Sierpinski Triangle"){
            triangle_fractal <- function(reps){
                prob <- runif(reps)
                x <- 0
                y <- 0
                idx <- 0
                points = data.frame("X"=x,"Y"=y,"order"=idx)
                for(p in prob){
                    if(p<=0.33){
                        xp <- 0.5*x - 0.25
                        yp <- 0.5*y - 0.25
                    }
                    else if(p<=0.66){
                        xp <- 0.5*x + 0.25
                        yp <- 0.5*y - 0.25
                    }
                    else{
                        xp <- 0.5*x 
                        yp <- 0.5*y + 0.25
                    }
                    idx <- idx+1
                    points[nrow(points)+1,] = c(xp,yp,idx)
                    x <- xp
                    y <- yp
                }
                points
            }
            points <- triangle_fractal(input$reps)
        }
       else if(input$fractal=="Sierpinski Carpet"){
           carpet_fractal <- function(reps){
               prob <- runif(reps)
               x <- 0
               y <- 0
               idx <- 0
               points = data.frame("X"=x,"Y"=y,"order"=idx)
               for(p in prob){
                   xp <- 0
                   yp <- 0
                   if(p<0.125){
                       xp <- x/3 - 1/3
                       yp <- y/3 + 1/3
                   }
                   else if(p<0.25){
                       xp <- x/3
                       yp <- y/3 + 1/3
                   }
                   else if(p<0.375){
                       xp <- x/3 + 1/3
                       yp <- y/3 + 1/3
                   }
                   else if(p<0.5){
                       xp <- x/3 + 1/3
                       yp <- y/3
                   }
                   else if(p<0.625){
                       xp <- x/3 + 1/3
                       yp <- y/3 - 1/3
                   }
                   else if(p<0.75){
                       xp <- x/3
                       yp <- y/3 - 1/3
                   }
                   else if(p<0.875){
                       xp <- x/3 - 1/3
                       yp <- y/3 - 1/3
                   }
                   else{
                       xp <- x/3 - 1/3
                       yp <- y/3
                   }
                   idx <- idx+1
                   points[nrow(points)+1,] = c(xp,yp,idx)
                   x <- xp
                   y <- yp
               }
               points
           }
           points <- carpet_fractal(input$reps)
       }
       else if(input$fractal=="Sierpinski Pentagon"){
           carpet_fractal <- function(reps){
               prob <- runif(reps)
               x <- 0
               y <- 0
               idx <- 0
               points = data.frame("X"=x,"Y"=y,"order"=idx)
               for(p in prob){
                   xp <- 0
                   yp <- 0
                   if(p<0.2){
                       xp <- x*0.38 + 0.235
                       yp <- y*0.38 - 0.323
                   }
                   else if(p<0.4){
                       xp <- x*0.38 - 0.235
                       yp <- y*0.38 - 0.323
                   }
                   else if(p<0.6){
                       xp <- x*0.38
                       yp <- y*0.38 + 0.4
                   }
                   else if(p<0.8){
                       xp <- x*0.38 + 0.38
                       yp <- y*0.38 + 0.123
                   }
                   else{
                       xp <- x*0.38 - 0.38
                       yp <- y*0.38 + 0.123
                   }
                   idx <- idx+1
                   points[nrow(points)+1,] = c(xp,yp,idx)
                   x <- xp
                   y <- yp
               }
               points
           }
           points <- carpet_fractal(input$reps)
       }
       else if(input$fractal=="Cantor Labirynth"){
           carpet_fractal <- function(reps){
               prob <- runif(reps)
               x <- 0
               y <- 0
               idx <- 0
               points = data.frame("X"=x,"Y"=y,"order"=idx)
               for(p in prob){
                   xp <- 0
                   yp <- 0
                   if(p<1/3){
                       xp <- x/3
                       yp <- y/3 + 1/3
                   }
                   else if(p<2/3){
                       xp <- -y/3 - 1/3
                       yp <- x
                   }
                   else{
                       xp <- y/3 + 1/3
                       yp <- -x
                   }
                   idx <- idx+1
                   points[nrow(points)+1,] = c(xp,yp,idx)
                   x <- xp
                   y <- yp
               }
               points
           }
           points <- carpet_fractal(input$reps)
       }
       else if(input$fractal=="Tree"){
           tree_fractal <- function(reps){
               prob <- runif(reps)
               x <- 0
               y <- 0
               idx <- 0
               points = data.frame("X"=x,"Y"=y,"order"=idx)
               for(p in prob){
                   xp <- 0
                   yp <- 0
                   if(p<0.25){
                       xp <- x*0.01
                       yp <- y*0.45
                   }
                   else if(p<0.5){
                       xp <- -x*0.01
                       yp <- -y*0.45 + 0.4
                   }
                   else if(p<0.75){
                       xp <- x*0.42 - 0.42*y
                       yp <- x*0.42 + y*0.42 + 0.4
                   }
                   else{
                       xp <- x*0.42 + 0.42*y
                       yp <- -x*0.42 + y*0.42 + 0.4
                   }
                   idx <- idx+1
                   points[nrow(points)+1,] = c(xp,yp,idx)
                   x <- xp
                   y <- yp
               }
               points
           }
           points <- tree_fractal(input$reps)
       }
       else if(input$fractal=="Ice Crystal"){
           crystal_fractal <- function(reps){
               prob <- runif(reps)
               x <- 0
               y <- 0
               idx <- 0
               points = data.frame("X"=x,"Y"=y,"order"=idx)
               for(p in prob){
                   xp <- 0
                   yp <- 0
                   if(p<0.25){
                       xp <- x*0.255 + 0.3726
                       yp <- y*0.255 + 0.6714
                   }
                   else if(p<0.5){
                       xp <- x*0.255 + 0.1146
                       yp <- y*0.255 + 0.2232
                   }
                   else if(p<0.75){
                       xp <- x*0.255 + 0.6306
                       yp <- y*0.255 + 0.2232
                   }
                   else{
                       xp <- x*0.37 -y*0.642 + 0.6356
                       yp <- x*0.642 +y*0.37 - 0.00061
                   }
                   idx <- idx+1
                   points[nrow(points)+1,] = c(xp,yp,idx)
                   x <- xp
                   y <- yp
               }
               points
           }
           points <- crystal_fractal(input$reps)
       }
       else if(input$fractal=="Feather"){
           feather_fractal <- function(reps){
               prob <- runif(reps)
               x <- 0
               y <- 0
               idx <- 0
               points = data.frame("X"=x,"Y"=y,"order"=idx)
               for(p in prob){
                   xp <- 0
                   yp <- 0
                   if(p<0.25){
                       xp <- x*0.7 + y*0.109682 + 0.05
                       yp <- -x*0.109504 + y*0.893292 + 0.1
                   }
                   else if(p<0.5){
                       xp <- x*0.058474 - y*0.573783 - 0.18
                       yp <- x*0.191261 + y*0.175423 - 0.21
                   }
                   else if(p<0.75){
                       xp <- x*0.011
                       yp <- y*0.3 - 0.35
                   }
                   else{
                       xp <- -x*0.067485 + y*0.579556 + 0.21
                       yp <- x*0.292311 + y*0.155291 - 0.21
                   }
                   idx <- idx+1
                   points[nrow(points)+1,] = c(xp,yp,idx)
                   x <- xp
                   y <- yp
               }
               points
           }
           points <- feather_fractal(input$reps)
       }
       else if(input$fractal=="Twig"){
           twig_fractal <- function(reps){
               prob <- runif(reps)
               x <- 0
               y <- 0
               idx <- 0
               points = data.frame("X"=x,"Y"=y,"order"=idx)
               for(p in prob){
                   xp <- 0
                   yp <- 0
                   if(p<1/3){
                       xp <- x*0.4 - y*0.2 + 0.13
                       yp <- -y*0.4 - 0.05
                   }
                   else if(p<2/3){
                       xp <- x*0.431604 + y*0.353533 + 0.2
                       yp <- x*0.416795 - y*0.421324 + 0.15
                   }
                   else{
                       xp <- x*0.470197 - 0.26
                       yp <- x*0.096513 - 0.053
                   }
                   idx <- idx+1
                   points[nrow(points)+1,] = c(xp,yp,idx)
                   x <- xp
                   y <- yp
               }
               points
           }
           points <- twig_fractal(input$reps)
       }
            outfile <- tempfile(fileext='.gif')
            
           anim = ggplot(points,aes(X,Y)) + 
               geom_count(show.legend = FALSE, colour =input$color, size=1.5, aes(group = seq_along(order)))+ theme_bw() + 
               theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black")) + transition_reveal(along = order)
           
           anim_save("outfile.gif", animate(anim, height=900, width=900,renderer = gifski_renderer(loop=FALSE)))
           
           list(src = "outfile.gif",
                contentType = 'image/gif'
                # width = 400,
                # height = 300,
                # alt = "This is alternate text"
           )}
      ,deleteFile = TRUE)})}

# Run the application 
shinyApp(ui = ui, server = server)
