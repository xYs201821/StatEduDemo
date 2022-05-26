#----------------------Shiny module---------------------------

cltUi <- function(ui){
  
  sidebarLayout(
    sidebarPanel(width=6,
                 h4("Population Control"),
                 selectizeInput(NS(ui,"poplist"),"Population",choices = dist,multiple = TRUE,options = list(placeholder = "Distribution")),
                 tabsetPanel(id = NS(ui,"popgroup"),tabPanel("Home Page", htmlOutput(NS(ui,"homepage")),htmlOutput(NS(ui,"plotctrl")),type = "pills")),
                 
    ),
    mainPanel(width=6,
              span(style="margin-right:150px",
                   actionButton(NS(ui,"help"),NULL,icon = icon("info-sign",lib="glyphicon"), 
                                style = "width:50px;", class = "btn btn-outline-info btn-lg")),
              actionButton(NS(ui,"hidebutton"),"Show",icon = icon("expand",lib="glyphicon"), style = "width:200px;", class = "btn btn-outline-success btn-lg"),
              div(class="card",style="text-align:center;padding:15px",
                  tabsetPanel(id = NS(ui,"plottype"),type="pills",
                              tabPanel("density",plotOutput(NS(ui,"density"))),
                              tabPanel("histogram",plotOutput(NS(ui,"hist"))),
                              tabPanel("violin",plotOutput(NS(ui,"violin"))),
                              tabPanel("qq",plotOutput(NS(ui,"qq"))),
                              tabPanel("stat",plotOutput(NS(ui,"stat")))
                  )),
              
    )
  )
}

cltServer <- function(id){
  moduleServer(id, function(input,output,session) {
    observeEvent(input$help,{
      showModal(modalDialog(
        title = "Users Guide",
        includeMarkdown("help/clt.md"),
        easyClose = TRUE,
        size = "xl",
        footer = modalButton("I get it")
      ))
    })
    # Action Button activate
    buttonswitch<-function(id,astring,bstring,state){
      if (state)
        updateActionButton(session,id,astring)
      else
        updateActionButton(session,id,bstring)
    }
    hideplot<-reactiveVal(TRUE)
    observeEvent(input$hidebutton,{
      hideplot(!hideplot())
      buttonswitch("hidebutton","Show","Hide",hideplot())
    })
    output$paramsmenu <- renderUI({
      purrr::map(as.character(seq(input$nparam)),~numericInput(NS(id,paste0("par",.x)),paste0("par",.x),value=0))
    })
    #plot control menu
    output$plotctrl<-renderUI({
      validate(need(input$plottype,""))
      validate(need(input$poplist,""))
      tagList(
        h4("Plot Control"),
        sliderInput(NS(id,"bw"),"binwidth",min=0,max=1,value=0.1),
        radioButtons(NS(id,"pos"),"Plot Position:", c("identity","stack","dodge2"),inline = TRUE),
        fluidRow(
          numericInput(NS(id, "axis1"), "Axis Lower Range:",value=-5),
          numericInput(NS(id, "axis2"), "Axis Upper Range:",value=5),
          column(4,
                 sliderInput(NS(id, "alpha"), "alpha:", min = 0, max = 1 ,value=0.4))),
        
      )
    })
    # parameter UI
    output$homepage<-renderUI({
      tagList( 
        sliderInput(NS(id,"n"),"Sample size",min = 10,max = 10000,value = 50,animate=TRUE),
        numericInput(NS(id,"rep"),"Replications",value = 10),
        checkboxInput(NS(id,"standard"),"Standardization",value=TRUE)
      )
    })
    #append parameter control panel
    observeEvent(input$poplist,{ 
      validate(need(length(input$poplist) > length(par_ctrl()),"")) # poplist 变化时，与目前的总体列表进行比较，更新控制面板
      purrr::map(input$poplist[!input$poplist %in% par_ctrl()],
                 ~appendTab("popgroup",tabPanel(.x,  #添加分页
                                                parainput(.x,id),
                                                textInput(NS(id,paste0(.x)),"Statistic",value="mean(x)",placeholder = "R expr of test statistic")
                 )
                 )
      )
      par_ctrl(input$poplist) #server端,记录总体列表
    })
    #remove parameter control panel
    observe({
      validate(need(length(input$poplist) < length(par_ctrl()),""))  # poplist 变化时，与目前的总体列表进行比较，更新控制面板
      purrr::map(par_ctrl()[!par_ctrl() %in% input$poplist],~removeTab("popgroup",.x))  #删除分页
      par_ctrl(input$poplist)
    })
    par_ctrl<-reactiveVal(NULL)
    #generate r.v series
    df <- purrr::map(dist1,~reactive({
      pop<-.x
      if (! pop %in% par_ctrl())
        return(NULL)
      npar <- ifelse(.x=="any",input$nparam,3)
      params<-purrr::map(paste0("par",as.character(seq(npar))),~input[[paste0(pop,.x)]])
      names(params)<-paste0("par",as.character(seq(npar)))
      if (pop=="any")
      {
        params$m<-input$bound
        params$supp<-input$supp
        params$f<-input$pdf
      }
      tmp <- replicate(input$rep,list(generate(input$n,pop,params)))
      tmp1 <- purrr::map_dbl(tmp,~mean(.x))
      if (input$standard)
        tmp1<-standardize(input$n,pop,params,tmp1)
      t<-purrr::map_dbl(tmp,~rlang::eval_tidy(str2lang(input[[pop]]),data = params,new_data_mask(env(x = .x,n = input$n))) )
      # t为统计量计算的结果,str2lang将R表达式文本转成R表达式,根据params值与每组样本的值.x,调用eval_tidy计算统计量的值
      list(test = data.frame(val=t,pop=pop), clt = data.frame(val=tmp1,pop=pop))
    }))
    if (FALSE){ #注释，旧写法
      df<-reactive({
        validate(need(length(input$poplist) > 0,""))
        validate(need(!hideplot(),""))
        d<-input$poplist
        names(d)<-d
        purrr::map_dfr(d,~({
          pop<-.x
          npar <- ifelse(.x=="any",input$nparam,3)
          params<-purrr::map(paste0("par",as.character(seq(npar))),~input[[paste0(pop,.x)]])
          names(params)<-paste0("par",as.character(seq(npar)))
          if (pop=="any")
          {
            params$m<-input$bound
            params$supp<-input$supp
            params$f<-input$pdf
          }
          tmp <- replicate(input$rep,list(generate(input$n,pop,params)))
          tmp1 <- purrr::map_dbl(tmp,~mean(.x))
          if (input$standard)
            tmp1<-standardize(input$n,pop,params,tmp1)
          t<-purrr::map_dbl(tmp,~rlang::eval_tidy(str2lang(input[[pop]]),data = params,new_data_mask(env(x = .x,n = input$n))) )
          list(test = data.frame(val=t,pop=pop), clt = data.frame(val=tmp1,pop=pop))
        }))
        #A<-rbind(A,data.frame(val=tmp, pop = c(d[i])))
        # purrr::map_dfr(input$poplist,~data.frame(val=A[[.x]],pop=c(.x)))
      })
    }
    is_concrete <- function(d){
      flag = TRUE
      for (i in seq_along(d))
      {
        if (d[i]=="any")
          next
        a<-input[[paste0(d[i],"par1")]]
        b<-input[[paste0(d[i],"par2")]]
        c<-input[[paste0(d[i],"par3")]]
        if (!logical(d[i],a,b,c))
          flag = FALSE
      }
      return(flag)
    }
    output$density<-renderPlot({
      validate(need(!hideplot(),""))
      # ifelse(is_concrete(input$poplist),"",return(NULL))
      data <- purrr::map_dfr(dist1,~df[[.x]]())
      p <- ggplot(data$clt,aes(x=val,fill=pop)) + geom_density(adjust=1,alpha = input$alpha, position = input$pos, bw = input$bw)
      p <- p+xlim(input$axis1,input$axis2)
      p
    })
    output$hist<-renderPlot({
      validate(need(!hideplot(),""))
      ifelse(is_concrete(input$poplist),"",return(NULL))
      data <- purrr::map_dfr(dist1,~df[[.x]]())
      p<-ggplot(data$clt,aes(x=val,fill=pop))+geom_histogram(alpha=input$alpha, position = input$pos, binwidth = input$bw)
      p <- p+xlim(input$axis1,input$axis2)
      p
    })
    output$violin<-renderPlot({
      validate(need(!hideplot(),""))
      validate(need(input$poplist,""))
      # ifelse(is_concrete(input$poplist),"",return(NULL))
      data <- purrr::map_dfr(dist1,~df[[.x]]())
      p<-ggplot(data$clt,aes(y=val,x=pop,fill=pop))+geom_violin(alpha=input$alpha,  position = input$pos)+geom_jitter(aes(y=val,x=pop),width = 0.4,height=0.01)
      p
    })
    output$qq<-renderPlot({
      validate(need(!hideplot(),""))
      #  ifelse(is_concrete(input$poplist),"",return(NULL))
      data <- purrr::map_dfr(dist1,~df[[.x]]())
      p<-ggplot(data$clt,aes(sample=val,color=pop))+geom_qq(alpha=input$alpha,  position = input$pos)+geom_qq_line()
      p <- p+xlim(input$axis1,input$axis2)
      p
    })
    output$stat<-renderPlot({
      validate(need(!hideplot(),""))
      data <- purrr::map_dfr(dist1,~df[[.x]]())
      p<-ggplot(data$test,aes(x=val,fill=pop))+geom_density(alpha=input$alpha, position = input$pos, bw = input$bw)
      p <- p+xlim(input$axis1,input$axis2)
      p
    })
  })
}