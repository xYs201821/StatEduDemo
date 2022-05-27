

#-------------------------------------------------------------
ui <- fluidPage(
  titlePanel("An Elementary Statistic Education Demo","Stat-Edu-Demo"),
  theme = bslib::bs_theme(bootswatch = "minty"),
  tabsetPanel(id="menu",
              tabPanel(value="info", title="About this site",
              tags$div(style="margin-left:5%;margin-right:5%;height:600px;overflow:scroll",withMathJax(includeMarkdown("README.md")))
              ),
              #中心极限定理验证及动画演示
              tabPanel(value="clt", title="随机样本",
                       br(),
                       cltUi("clt")
              ),
              #单样本假设检验
              tabPanel(value="es", title="假设检验1",
                       br(),
                       sidebarLayout(
                        esInput("es"),
                        esOutput("es")
                       )
              ),
              #两样本假设检验
              tabPanel(value="ds", title="假设检验2",
                      br(),
                      sidebarLayout(
                        dsInput("ds"),
                        dsOutput("ds")
                      )
              )
  )
)

server <- function(input, output, session) {
    #test area
  library(markdown)
  library(purrr)
  require(shiny)
  require(ggplot2)
  require(rlang)
  require(dplyr)
  observeEvent(input$menu,{
    showModal(modalDialog(
      title = "Important message",
      "Please check the distribution of both Sample 1 and 2 before you start a test in Hypothesis Test 2",
      easyClose = TRUE
    ))
  },once = TRUE)
  cltServer("clt")
  esServer("es")
  esServer("ds")
}
shinyApp(ui, server)
