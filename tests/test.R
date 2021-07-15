library(cns)

stopifnot(

  cns("white red")    == '#FF0000',
  cns("white green")  == '#00FF00',
  cns("white blue")   == '#0000FF',
  cns("white yellow") == '#FFFF00',

  cns("orange") == cns('brown')

)

