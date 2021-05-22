# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
# cycle, program, answers, control, lab
program <- function() {

  response <- c(color_code='#ffffff0', piv=runif(1, min=0, max=100))
  print(response)
}


# Funcion evaluacionPreliminar()
# Entradas: IdLaboratorio
#           cicloReporte
#           IdPrograma
#           IdReporte
#           IdAnalizador

piv_preliminar <- function(i_ciclo,i_programa,i_reporte,i_analizador) {
  id_reporte<-c(i_reporte)
  #Leer de base de datos resultados de analitos para calcular iv
  iv_analitos<-runif(10,min=0,max=300)
  iv_analitos<-round(iv_analitos)
  #Calcular promedio del programa
  piv<-mean(iv_analitos)
  piv<-round(piv)
  #identificar analitos
  id_analitos<-c("41653-7","2345-7","55420-4","4548-4","27353-2","14957-5","2514-8","5792-7","9057-1","55400-6")
  df <- data.frame(id_reporte,piv,id_analitos,iv_analitos)

  return(df)
}
