## Importation
#' PlotDens Function
#'
#' Allow beginner students in statistic to visualize and compare a normal and student distribution.
#'
#' @param a Starting of the density area
#' @param b End of the density area
#' @param mu Mean of the normal density
#' @param var Variance of the normal density
#' @param df Degree og freedom of the student density
#' @param output The plot you want to disply (1 = both, 2= only normal distribution, 3= Only student distribution)
#'
#' @return
#' @export
#'
#' @import ggplot2
#'
#'
#' @examples PlotDens( a = 2, b=2, mu=1, var=1.7, df=4, output=1)
#'
PlotDens <- function(a = -1, b = 1 , mu=0  , var=1 , df=1 , output = 1) {
  theme_set(theme_minimal())
  title_norm = paste("Normal density\nP(a < X < b)  = ", round(pnorm(b, mu , var) - pnorm(a, mu, var), 3))
  title_st = paste("Student density\nP(a < X < b)  = ", round(pt(b, df) - pt(a, df), 3))

  l = ggplot() +
        annotate("text" , x = a, y=-0.01, label='a') +
        annotate('text', x=b ,y=-0.01, label='b')+
        theme(legend.position = "top")+
        labs(x = '' , y= 'density', color='')

    if(output==1){ # Both
      print(l+
              xlim(-5,5)+
              stat_function(fun=dnorm, aes(colour = title_norm), args=list(mu , var)) +
              stat_function(fun = dnorm , xlim=c(a,b) , geom='area', alpha=0.3, fill='red', args=list(mu , var))+
              stat_function(fun=dt, aes(colour = title_st), args=list(df)) +
              stat_function(fun = dt , xlim=c(a,b) , geom='area', alpha=0.3, fill='lightblue', args=list(df)) +
              geom_segment(aes(x = a , xend = a , y = 0 , yend = dnorm(a, mu , var)) , linetype = 2)+
              geom_segment(aes(x = b , xend = b , y = 0 , yend = dnorm(b, mu , var)) , linetype = 2)
      )
    }
    else if(output==2){ # Normal
      print(l+
              xlim(-5,5)+
              stat_function(fun=dnorm, aes(colour = title_norm), args=list(mu , var)) +
              stat_function(fun = dnorm , xlim=c(a,b) , geom='area', alpha=0.5, fill='red', args=list(mu , var))+
              geom_segment(aes(x = a , xend = a , y = 0 , yend = dnorm(a, mu , var)) , linetype = 2)+
              geom_segment(aes(x = b , xend = b , y = 0 , yend = dnorm(b, mu , var)) , linetype = 2)
      )

    }
    else if (output == 3){ # Student
      print(l+
              xlim(-5,5)+
              stat_function(fun=dt, aes(colour = title_st), args=list(df)) +
              stat_function(fun = dt , xlim=c(a,b) , geom='area', alpha=0.5, fill='lightblue', args=list(df)) +
              geom_segment(aes(x = a , xend = a , y = 0 , yend = dt(a, df)) , linetype = 2)+
              geom_segment(aes(x = b , xend = b , y = 0 , yend = dt(b, df)) , linetype = 2)
      )
    }
  }


