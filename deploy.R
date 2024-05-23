#Publish

#PATH 		= "/Users/uc/Desktop/R Code/final"
#setwd( PATH )

install.packages("rsconnect")
library(rsconnect)

rsconnect::setAccountInfo(name='yung0cheng',
                          token='DDEE440A3B4F98D482143611777668B3',
                          secret='TO7ujL/ZllKehmKGYlLzotLNCjF1RmXIhDndf6cQ')

rsconnect::deployApp(PATH)
