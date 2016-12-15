
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Estadistica Computacional"),
  h2("Augusto Hernandez"),
  h4("112835"),
  

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      radioButtons("tarea", label="Escoge tarea",
        choices = c(
          "Aceptacion Rechazo"="aceptacionRechazo",
          "Funcion Inversa"="funInv", 
          "Monte Carlo"="montecarlo",
          "Bayes Lin Reg"="mcmc"
          ),
        selected="aceptacionRechazo"
      )
    ),
    # Show a plot of the generated distribution
    mainPanel(
      conditionalPanel(
        condition="input.tarea=='aceptacionRechazo'",
        h2("Aceptacion-Rechazo"),
        textInput(
          inputId="expresion1", 
          label="Función f",
          value="function(x) 2*x"),
        selectInput(
          inputId="expresion2", 
          label="Función g",
          choices=c("Uniforme(xmin, xmax)"="unif", "Exponencial(1) truncada a (xmin,xmax)"="exp", "Normal(0,1) truncada a (xmin,xmax)"="norm")
        ),
        sliderInput("xmin", "xmin", min=-30, max=30, value=0),
        sliderInput("xmax", "xmax", min=-30, max=20, value=1),
        sliderInput("M", "M", min=0.1, max=100, value=1),
        numericInput("nsim", "Número de simulaciones", value=100),
        actionButton("button1", "Correr"),
        plotOutput("Grafica"),
        h3("Resultados"),
        p("Tasa de éxito", textOutput("tasa_exito")),
        plotOutput("hist_sim"),
        sliderInput("nbins", "nbins", value=20, min=10, max=100)
        
      ),
      #Tarea 1 
      conditionalPanel(
        condition="input.tarea=='funInv'",
        h2("Tarea 1: Funcion Inversa"),
        numericInput("nsim_fi", "Número de simulaciones", value=100),
        numericInput("lambda", "Valor lamda", value=2),
        h4("Vector de simulaciones"),
        dataTableOutput("tabla_datos"),
        h4("Histograma con simulaciones"),
        plotOutput("hist_sim_inv"),
        sliderInput("nbins_i", "nbins_i", value=20, min=10, max=100)
      ),
      
      #Tarea  2
      conditionalPanel(
         condition="input.tarea=='montecarlo'",
         h2("Tarea 2: Monte Carlo"),
         numericInput("a", "Limite inferior", value=0),
         numericInput("b", "Limite superior", value=2),
         numericInput("nsim_mc", "Número de simulaciones", value=100),
         textInput(
           inputId="funcionmc", 
           label="Función f",
           value="function(x) 2*x"),
        sliderInput("alpha", "nivel_conf", value=.1, min=.01, max=.5),
        numericInput("nmin", "numero minimo", min=2, max=10000, value=1000),
        numericInput("nmax", "numero maximo", min=2, max=10000, value=10000),
        plotOutput("grafica_sim"),
        textOutput("Resultado")
        
        
              ),
      


#Tarea 4,5,6
conditionalPanel("input.tarea=='mcmc'",
                 
                 h5("Este conjunto de datos contiene informacion recopilada por el Servicio del Censo de los Estados Unidos sobre la vivienda en la zona de la Misa de Boston. Se obtuvo del archivo StatLib (http://lib.stat.cmu.edu/datasets/boston) y se ha utilizado ampliamente en la literatura para comparar algoritmos."),
                 dataTableOutput('table'),
                 
                 selectInput("dep", "Variable Dependiente:", 
                             choices = c('medv'=1,'lstat'=2,'crim'=3,'indus'=4,'black'=5,'pratio'=6)), 
                 checkboxGroupInput("checkGroup", label = "Variables Independientes, elige diferente que la dependiente", 
                                    choices = c('valor medio de la casa'=1,'porcentaje de pob en pobreza'=2,'criminalidad'=3,
                                                'indusstria'=4,'habitantes afroamericanos'=5,'% de alumnos x maestro'=6),
                                    selected = 3:4),
                 sliderInput('long4', label = 'Numero de cadenas', value = 5, min = 1, max = 10, step = 1),
                 sliderInput('N4', label = 'Longitud de la cadena', min = 1000, max = 10000, value =1000, step = 1000),
                 actionButton('Simular', 'Simular'),
                 fluidRow(
                   h5('A continuacion se muestra un analisis exploratorio de la base de datos Boston.'),
                   plotOutput('boston1'),
                   h6('Algo de llamar la atencion en esta grafica es como rm y age no parecen tener una relacion positiva, pues se podria pensar que cuando hay m??s ni??os se necesitan m??s cuartos, aunque tamb??en el n??mero de hijos por familia esta indirectamente relacionada al ingreso. Por ello tamb??en se podr??a explicar la relaci??n que guardan ptratio y medv. Por otro lado me llam?? la atenci??n la relaci??n de lstat con tax pues no se esperaria ver una relacion negativa, y de lstat con medv positiva, al menos moralmente hablando.'),
                   plotOutput('boston2'),
                   h6('Hay varias variables que se comportan como distribuciones conocidas, por ejemplo a age podriamos asignarle una exponencial, al igual que black; A chas, se le puede asignar una beta aunque se ve insuficiente. Luego a dis e istatlas podriamos asociar a una poisson, a medv tambien o a una gamma. Y a rm le asociaria una binomial o una normal si se tiene suficiente densidad en los datos. Y a estas variables les aplicara un boxplot ya que al poderlas asociar a distribuciones conocidas los estad??sticos reflejados en el boxplot son mas parsimoniosos, de otra manera podr??amos tener valores importantes y considerarlos outlayers o perder de vista huecos importantes en los datos como en el caso de la variable nox.'),
                   plotOutput('boston3'),
                   h6('En este grafico combinado para la variable medv, podemos ver primeramente en el piso gracias a la ayuda del rug, el porque de los outlayers que teniamos en el boxplot, es porque cerca del cincuenta hay una segunda joroba formando una especie de distribuci??n bimodal, aunque en comparativa mucho menor a la moda. Adem??s podemos ver como en algunas partes se generan minimos locales, esto justifica la existencia de pequeños gaps en el strichart'),
                   h5('RESULTADO:'),
                   withMathJax(uiOutput('tableMH2')),
                   plotOutput('boston4'),
                   h4('Histogramas por parametro despues de simular.'),
                   plotOutput('ploH'),
                   h4('Simulacion de las cadenas por parametro.'),
                   dataTableOutput("tablechain"))
),
tabPanel('Datos', dataTableOutput('data'))



        
    )
      
    )
  )
)
