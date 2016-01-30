carptopla <- function(x,y){
  toplam=x+y
  carpim=x*y
  print(x)
  #return(c(toplam,carpim)) 
  return(list(Toplam=toplam,
              Carpim=carpim))
}

a=carptopla(3,7)
a$Toplam
help(read.table)
