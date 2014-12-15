library(ggplot2)

dmnd <- diamonds
dmnd$carat_band <- ifelse(dmnd$carat<0.5,'0-0.5',ifelse(dmnd$carat<1,'0.5-1.0',ifelse(dmnd$carat<1.5,'1.0-1.5',ifelse(dmnd$carat<2,'1.5-2.0',ifelse(dmnd$carat<2.5,'2.0-2.5',ifelse(dmnd$carat<3.0,'2.5-3.0',ifelse(dmnd$carat>=3,'3.0+','ERR')))))))

#this is the most interesting plot
dmnd.a <- dmnd
dmnd.a <- filter(dmnd.a,dmnd.a$color == 'D' | dmnd.a$color == 'E')
dmnd.a <- filter(dmnd.a,dmnd.a$cut != 'Fair' & dmnd.a$cut != 'Good')
dmnd.a <- filter(dmnd.a,dmnd.a$clarity == 'SI1')# || dmnd.a$clarity == 'SI2' || dmnd.a$clarity == 'VS1' || dmnd.a$clarity == 'VS2')
qplot(carat,price,data=dmnd.a,geom='point',color=carat_band)
qplot(clarity,price,data=dmnd,geom='point')
qplot(carat,price,data=dmnd,geom='point',color=clarity,xlim=c(0,3))



qplot(carat,price,data=dmnd,deom='point',color=carat_band,xlim=c(0,3))

#some other plots
#qplot(carat,price,data=dmnd,deom='point',color=cut)
#qplot(clarity,price,data=dmnd,deom='point',color=carat_band,ylim=c(0,5000))

#see what kind of value I got for Lainey's center stone
#myr <- dmnd
#myr <- filter(myr,myr$carat >= 0.65 & myr$carat <= 0.85)
#myr <- filter(myr,myr$cut=='Very Good')
#myr <- filter(myr,myr$color=='E')
#myr <- filter(myr,myr$clarity=='SI2')
#qplot(carat,price,data=myr,deom='point',color=carat_band)
