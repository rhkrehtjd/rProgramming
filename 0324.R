plot(0:10,0:10,xlab="",ylab="",xaxt="n",yaxt="n", type="n")
rect(6,6,9,9)
corners <- function(){
  # locator(1)은 마우스로 찍을 점 하나의 위치 
  coos <- c(unlist(locator(1)), unlist(locator(1)))
  rect(coos[1], coos[2], coos[3], coos[4])
}
corners()
corners()

arrows(1,1,3,8)
arrows(1,9,5,9,code=3)
arrows(3,8,4,8,code=2)

click.arrows <- function(){
  coos <- c(unlist(locator(1)), unlist(locator(1)))
  arrows(coos[1],coos[2],coos[3],coos[4])
}
click.arrows()

locations = locator(6)
polygon(locations, col = "lavender")

