library(dplyr)
data <- mutate(expand.grid( x = c( 'a', 'b', 'c')
                    , y = c( 'd', 'e', 'f')
                    , .rep = 1:10
                    ), v = rnorm(90))
long <- summarise(group_by(data, x, y),N=n(), sum=sum(v))

spread_each(long, y, N, sum)
