#----------------------Shiny module---------------------------

esInput <- function(ui){
  sidebarPanel(width=5,
               tabsetPanel(
                 id=NS(ui,"test"),
                 tabPanel("observed-value",
                          sliderInput(NS(ui,"level")," \\( level \\) \\( \\alpha :\\)", min=0,max=1,value = 0.05),
                          numericInput(NS(ui,"n"),"Sample 1 Size",min = 1, max = 1000, value = 15),
                          realdataInput(NS(ui,"1"))
                 ),
                 tabPanel("Z",
                          p(style="font-size:20px","Test Statistic :",jax("\\bar{x}","\\sim","N(\\mu_0,\\frac{\\sigma^2}{n})")),
                          testInput(NS(ui,"Z"))
                 ),
                 tabPanel(title="Normal Variance",value="Chi",
                          p(style="font-size:20px","Test Statistic :",jax("(n-1)\\frac{S^2_{n-1}}{\\sigma^2_0}","\\sim","\\chi^2_{n-1}")),
                          testInput(NS(ui,"Chi"))
                 ),
                 tabPanel("t",
                          p(style="font-size:20px","Test Statistic :",jax("\\sqrt{n} \\frac{\\bar{x}-\\mu_0}{S_{n-1}}","\\sim","t_{n-1}")),
                          testInput(NS(ui,"t"))
                 ),
                 
               ),
  )
}
esOutput <- function(ui){
  mainPanel(width=7,
            div(class="card",
                div(class="card-header",
                    fluidRow(
                      span(style="margin-right:100px",
                           actionButton(NS(ui,"help"),NULL,icon = icon("info-sign",lib="glyphicon"), 
                                        style = "width:50px;", class = "btn btn-outline-info btn-lg")),
                      actionButton(NS(ui,"getdata"), "Generate Real Data", class="btn btn-success"),
                      actionButton(NS(ui,"show1"), "Show Type1 error", class="btn btn-danger"),
                      actionButton(NS(ui,"show2"), "Show Type2 error", class="btn btn-info"),
                      actionButton(NS(ui, "show3"), "Show Rejection Area", class="btn btn-warning"),
                    )
                ),
                div(class="card-body",
                    tableOutput(NS(ui,"res")),
                    plotOutput(NS(ui,"graph")),
                )
            )
  )
}
dsInput <- function(ui){
  sidebarPanel(width=5,
               tabsetPanel(
                 id=NS(ui,"test"),
                 tabPanel("observed-value",
                          sliderInput(NS(ui,"level")," \\( level \\) \\( \\alpha :\\)", min=0,max=1,value = 0.05),
                          tabsetPanel(id = NS(ui,"group"),type="tabs",
                                      tabPanel(paste0("Sample1"),
                                               numericInput(NS(ui,"n"),paste0("Sample ","1"," Size"),min = 1, max = 1000, value = 15),
                                               realdataInput(NS(ui,"1"))
                                      ),
                                      tabPanel("Sample2",
                                               numericInput(NS(ui,"m"),"Sample 2 Size",min = 1, max = 1000, value = 15),
                                               realdataInput(NS(ui,"2"))
                                      ),
                          ),
                 ),
                 tabPanel(title="Z",value = "Z2",
                          p(style="font-size:20px","Test Statistic : ",jax("\\bar{x_1}-\\bar{x_2}","\\sim","N(\\mu_1-\\mu_2,\\frac{\\sigma^2_1}{n}+\\frac{\\sigma^2_2}{m})")),
                          testInput(NS(ui,"Z2"))
                 ),
                 tabPanel("F",
                          p(style="font-size:20px","Test Statistic : ",jax("\\frac{S^2_{n-1}}{S^2_{m-1}} \\frac{\\sigma^2_2}{\\sigma^2_1} ","\\sim","F_{n-1,m-1}")),
                          testInput(NS(ui,"F"))         
                 ),
                 tabPanel(title="t",value= "t2",
                          p(style="font-size:20px","Test Statistic : ",jax("\\frac{\\bar{x_1}-\\bar{x_2}-(\\mu_1 - \\mu_2)}{S_\\omega}\\sqrt{\\frac{mn}{m+n}}","\\sim","t_{n+m-2}")),
                          testInput(NS(ui,"t2"))
                 )
               )
  )
}
dsOutput <- function(ui){
  mainPanel(width=7,
            div(class="card",
                div(class="card-header",
                    fluidRow(
                      span(style="margin-right:100px",
                           actionButton(NS(ui,"help"),NULL,icon = icon("info-sign",lib="glyphicon"), 
                                        style = "width:50px;", class = "btn btn-outline-info btn-lg")),
                      actionButton(NS(ui,"getdata"), "Generate Real Data", class="btn btn-success"),
                      actionButton(NS(ui,"show1"), "Show Type1 error",class="btn btn-danger"),
                      actionButton(NS(ui,"show2"), "Show Type2 error",class="btn btn-info"),
                      actionButton(NS(ui, "show3"), "Show Rejection Area", class="btn btn-warning")
                    )
                ),
                div(class="card-body",
                    tableOutput(NS(ui,"res")),
                    plotOutput(NS(ui,"graph")),
                )
            )
  )
}
testInput <- function(id){
  params_name <- params_list(id)
  h0<-params_name$h0
  h<-params_name$h
  params_name$h0<-NULL
  params_name$h<-NULL
  tagList(
    fluidRow(style="margin:0 auto",
             span("\\( type \\  of \\  H_0 : \\)",style="margin-right: 15px;margin-left:20%"),
             radioButtons(NS(id,"h0type"),NULL,
                          choiceNames = list(jax(h,"=",h0),jax(h,"\\leq",h0),jax(h,"\\geq",h0)),
                          choiceValues = list("=","<",">"),inline=TRUE
             )),
    htmlOutput(NS(id,"h1type")),
    br("    "),
    numericInput(NS(id,"h0"),jax(h0),value=1),
    purrr::map(params_name, ~paramsInput(id,.x))
  )
}
realdataInput <- function(id){
  tagList(
    selectizeInput(NS(id,"pop"),"Population",choices = dist,options = list(placeholder = "Distribution")),
    htmlOutput(NS(id,"real"))
  )
}