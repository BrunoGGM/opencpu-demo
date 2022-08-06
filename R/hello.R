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

# test function
test <- function(object) {
  
  print(object->analitos)
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


radar <- function(i_ciclo,i_programa,i_reporte,i_analizador) {
  iv_analitos <- as.data.frame(matrix(round(runif(20,min=0,max=300)) , ncol=20))
  colnames(iv_analitos) <- c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12","A13","A14","A15","A16","A17","A18","A19","A20")

  iv_analitos <- rbind(rep(300,10) , rep(0,10) , iv_analitos)

  # The default radar chart
  radarchart(iv_analitos,pcol='pink',pfcol=rgb(0.9,0.2,0.5,0.3),cglcol='grey',cglty=1)
}

plot_test <- function() {
  set.seed(42)
  data("mtcars")
  dat2 <- subset(mtcars, wt > 3 & wt < 4)
  # Hide all of the text labels.
  dat2$car <- ""
  # Let's just label these items.
  ix_label <- c(2,3,16)
  dat2$car[ix_label] <- rownames(dat2)[ix_label]

  g1 <- ggplot(dat2, aes(wt, mpg, label = car)) +
    geom_point(color = ifelse(dat2$car == "", "grey50", "red")) +
    geom_text_repel()

  print(g1)
}

gitstats <- function (id = "hadley", type = c("users", "orgs"), max = 20) {

  type <- match.arg(type, choices=c('users','orgs'))
  max <- min(max, 100)

  #the 'gh' package automatically paginates
  url <- file.path("https://api.github.com", type, id, "repos")
  res <- gh::gh(url, type = "owner", .limit = Inf)
  out <- jsonlite:::simplify(res, flatten = TRUE)

  #resort factor)
  out <- out[order(out$watchers, decreasing = TRUE)[seq_len(max)],
             c("name", "watchers", "forks", "open_issues")]
  out$name <- factor(out$name, levels = rev(out$name))

  #reshape to "long" dataframe"
  names(out) <- c("Repo", "Stars", "Forks", "Issues")
  out2 <- reshape2::melt(out, id = 1)

  #create ggplot object
  time <- format(Sys.time(), tz = 'UTC', usetz = TRUE)
  ggplot(out2, aes(Repo, value)) + geom_bar(stat="identity") + coord_flip() +
    facet_wrap(~variable, scales = "free_x") + xlab("") + ylab("") +
    ggtitle(sprintf("Github stats from: '%s' (%s)", id, time))

}

