#----------------------Shiny module---------------------------

realdataServer <- function(id,parent_id,size){
  moduleServer(id,function(input,output,session){
    output$real <- renderUI({
      parainput(input$pop,NS(parent_id,id))
    })
    reactive({
      pop<-input$pop
      npar <- ifelse(pop=="any",input$nparam,3)
      params<-purrr::map(paste0("par",as.character(seq(npar))),~input[[paste0(pop,.x)]])
      names(params)<-paste0("par",as.character(seq(npar)))
      if (pop=="any")
      {
        params$m<-input$bound
        params$supp<-input$supp
        params$f<-input$pdf
      }
      params$name <- pop
      params
    })
  })
}
#the common server function for all hypothesis testing process
esServer <- function(id){  
  moduleServer(id,function(input,output,session){
    #the real distribution
    #the type of selected test
    observeEvent(input$help,{
      showModal(modalDialog(
        title = "Users Guide",
        includeMarkdown(paste0("help/",id,".md")),
        easyClose = TRUE,
        size = "xl",
        footer = modalButton("I get it")
      ))
    })
    type_test<-reactiveVal(NULL)
    observeEvent(input$test,{
      switch(input$test,
             "Z" = type_test("Z"),
             "Chi" = type_test("Chi"),
             "F" = type_test("F"),
             "Z2" = type_test("Z2"),
             "t" = type_test("t"),
             "t2" = type_test("t2"),
             "true-value" = type_test(type_test())
      )
    })
    
    size <- reactive(
      list(n = max(0,input$n), m = max(0,input$m) )
    )
    # Ui Initial----------
    uiServer("Z")
    uiServer("Z2")
    uiServer("Chi")
    uiServer("t")
    uiServer("t2")
    uiServer("F")
    #--------------
    test_res <- reactive({
      testServer(type_test(),size,reactive(input$level),real_test)
    })
    test_data <- reactive(dataServer(type_test(),size))
    pop1 <- realdataServer("1",id,size)
    pop2 <- realdataServer("2",id,size)
    real_test <- reactive(Call_testStat(type_test(),real_data,size))
    real_data <- reactiveVal(NULL)
    observeEvent(input$getdata,{
      tmp1<-NULL
      if (size()$n>0) tmp1 <- generate(size()$n,pop1()$name,pop1())
      tmp2<-NULL
      if (size()$m>0) tmp2 <- generate(size()$m,pop2()$name,pop2())
      real_data(list(x=tmp1,y=tmp2))
    })
    output$graph <- renderPlot({
      req(type_test())
      plotServer(type_test(),test_data,real_data,size,level = reactive(input$level),
                 show = reactive(list(one=input$show1, two=input$show2, three=input$show3, real=input$getdata)),
                 test = test_res,real_test)
    },res = 96)
    output$res <- renderTable({
      req(type_test())
      req(test_res())
      req(real_test())
      req(test_data())
      data.frame(test_Statistic = real_test() ,
                 Accept_Area_Lower = test_res()$lower,
                 Accept_Area_Upper = test_res()$upper, 
                 p_value = test_res()$p)
    },align="c",width="auto",hover=TRUE,bordered = TRUE,striped = TRUE, digits = 4)
  })
}



#调整UI
uiServer <- function(id){
  #动态调整UI
  moduleServer(id,function(input,output,session){
    params<- params_list(id)  #id like "Z"
    h0<-params$h0
    h<-params$h
    output$h1type <- renderUI({
      validate(need(input$h0type, "Please choose \\(H_0\\)"))
      switch(input$h0type,
             "=" = tags$span(
               span(jax("H_0:", h, "=", h0),style="float:left"),
               span(jax("H_1:", h, "\\neq", h0), style="float:right")
             ),
             "<" = tags$span(
               span(jax("H_0:", h, "\\leq", h0),style="float:left"),
               span(jax("H_1:", h, ">", h0), style="float:right")
             ),
             ">" = tags$span(
               span(jax("H_0:", h, "\\geq", h0),style="float:left"),
               span(jax("H_1:", h, "<", h0), style="float:right")
             ),
      )
    })
    observe({
      validate(need(input$h0type, ""))
      input$h0type
      input$h0
      params_value <- purrr::map(params,~isolate(input[[.x]]))
      switch(id,
             "Z" = {
               switch(input$h0type,
                      "=" = {
                        updateSliderInput(session, inputId = "test0", min=input$h0, max=input$h0, value=input$h0)
                        updateSliderInput(session, inputId = "test1", min=input$h0-2*params_value$par1, max=input$h0+2*params_value$par1 ,value=input$h0+params_value$par1)
                      },
                      "<" = {
                        updateSliderInput(session, inputId = "test0", min=input$h0-3*params_value$par1, max=input$h0, value=input$h0)
                        updateSliderInput(session, inputId = "test1", min=input$h0, max=input$h0+3*params_value$par1, value=input$h0+params_value$par1)
                      },
                      ">" = {
                        updateSliderInput(session, inputId = "test0", min=input$h0, max=input$h0+3*params_value$par1, value=input$h0)
                        updateSliderInput(session, inputId = "test1", min=input$h0-3*params_value$par1, max=input$h0, value=input$h0-params_value$par1)
                      },
               )},
             "Chi" = {
               switch(input$h0type,
                      "=" = {
                        updateSliderInput(session, inputId = "test0", min=input$h0, max=input$h0, value=input$h0)
                        updateSliderInput(session, inputId = "test1", min=0, max=input$h0+2 ,value=input$h0+1)
                      },
                      "<" = {
                        updateSliderInput(session, inputId = "test0", min=0, max=input$h0, value=input$h0)
                        updateSliderInput(session, inputId = "test1", min=input$h0, max=input$h0+3, value=input$h0+1) 
                      },
                      ">" = {
                        updateSliderInput(session, inputId = "test0", min=input$h0, max=input$h0+3, value=input$h0)
                        updateSliderInput(session, inputId = "test1", min=0, max=input$h0, value=input$h0/2) 
                      }
               )
             },
             "F" = {
               switch(input$h0type,
                      "=" = {
                        updateSliderInput(session, inputId = "test0", min=input$h0, max=input$h0, value=input$h0)
                        updateSliderInput(session, inputId = "test1", min=0, max=input$h0+2 ,value=input$h0+1)
                      },
                      "<" = {
                        updateSliderInput(session, inputId = "test0", min=0, max=input$h0, value=input$h0)
                        updateSliderInput(session, inputId = "test1", min=input$h0, max=input$h0+3, value=input$h0+1) 
                      },
                      ">" = {
                        updateSliderInput(session, inputId = "test0", min=input$h0, max=input$h0+3, value=input$h0)
                        updateSliderInput(session, inputId = "test1", min=0, max=input$h0, value=input$h0/2) 
                      }
               )
             },
             "t" = {
               switch(input$h0type,
                      "=" = {
                        updateSliderInput(session, inputId = "test0", min=input$h0, max=input$h0, value=input$h0)
                        updateSliderInput(session, inputId = "test1", min=input$h0-2*params_value$par1, max=input$h0+2*params_value$par1 ,value=input$h0+params_value$par1)
                      },
                      "<" = {
                        updateSliderInput(session, inputId = "test0", min=input$h0-3*params_value$par1, max=input$h0, value=input$h0)
                        updateSliderInput(session, inputId = "test1", min=input$h0, max=input$h0+3*params_value$par1, value=input$h0+params_value$par1)
                      },
                      ">" = {
                        updateSliderInput(session, inputId = "test0", min=input$h0, max=input$h0+3*params_value$par1, value=input$h0)
                        updateSliderInput(session, inputId = "test1", min=input$h0-3*params_value$par1, max=input$h0, value=input$h0-params_value$par1)
                      },
               )
             },
             "Z2" = {
               switch(input$h0type,
                      "=" = {
                        updateSliderInput(session, inputId = "test0", min=input$h0, max=input$h0, value=input$h0)
                        updateSliderInput(session, inputId = "test1", min=input$h0-2*(params_value$par1+params_value$par2), max=input$h0+2*(params_value$par1+params_value$par2) ,value=input$h0+(params_value$par1+params_value$par2))
                      },
                      "<" = {
                        updateSliderInput(session, inputId = "test0", min=input$h0-3*(params_value$par1+params_value$par2), max=input$h0, value=input$h0)
                        updateSliderInput(session, inputId = "test1", min=input$h0, max=input$h0+3*(params_value$par1+params_value$par2), value=input$h0+(params_value$par1+params_value$par2))
                      },
                      ">" = {
                        updateSliderInput(session, inputId = "test0", min=input$h0, max=input$h0+3*(params_value$par1+params_value$par2), value=input$h0)
                        updateSliderInput(session, inputId = "test1", min=input$h0-3*(params_value$par1+params_value$par2), max=input$h0, value=input$h0-(params_value$par1+params_value$par2))
                      },
               )
             },
             "t2" = {
               switch(input$h0type,
                      "=" = {
                        updateSliderInput(session, inputId = "test0", min=input$h0, max=input$h0, value=input$h0)
                        updateSliderInput(session, inputId = "test1", min=input$h0-2*params_value$par1, max=input$h0+2*params_value$par1 ,value=input$h0+params_value$par1)
                      },
                      "<" = {
                        updateSliderInput(session, inputId = "test0", min=input$h0-3*params_value$par1, max=input$h0, value=input$h0)
                        updateSliderInput(session, inputId = "test1", min=input$h0, max=input$h0+3*params_value$par1, value=input$h0+params_value$par1)
                      },
                      ">" = {
                        updateSliderInput(session, inputId = "test0", min=input$h0, max=input$h0+3*params_value$par1, value=input$h0)
                        updateSliderInput(session, inputId = "test1", min=input$h0-3*params_value$par1, max=input$h0, value=input$h0-params_value$par1)
                      },
               )
             }
      )
    })
  })
}
#获得生成图像的数据
dataServer <- function(id,size){
  moduleServer(id,function(input,output,session){
    params<-params_list(id)
    params_value<-reactive(purrr::map(params,~input[[.x]]))
    #生成数据
    alter_df<-reactive({
      req(params_value())
      par_tmp<-params_value()
      par_tmp$test<-par_tmp$test1
      tmp<-generate2(id,param_func(id),par_tmp,size(),input$h0)
      data.frame(ax= tmp$x,val = tmp$y, type=rep("alter"))
    })
    null_df<-reactive({
      req(params_value())
      par_tmp<-params_value()
      par_tmp$test<-par_tmp$test0
      tmp<-generate2(id,param_func(id),par_tmp,size(),input$h0)
      data.frame(ax= tmp$x,val = tmp$y, type=rep("null"))
    })
    list(
      null=null_df, 
      alter=alter_df
    )
  })
} 

#calculate the results of hypothesis testing
testServer <- function(id, size, level, real_test){
  params<-params_list(id)
  moduleServer(id,function(input,output,session){
    switch(id,
           "Z" = ztest(size(),level(),input$h0,input[[params$par1]],input$h0type, real_test()),
           "Chi" = chitest(size(),level(),input$h0,input[[params$par1]],input$h0type, real_test()),
           "F" = ftest(size(),level(),input$h0,input[[params$par2]],input[[params$par3]],input$h0type, real_test()),
           "t" = ttest(size(),level(),input$h0type,real_test()),
           "Z2" = z2test(size(),level(),input$h0,input[[params$par1]],input[[params$par2]],input$h0type,real_test()),
           "t2" = t2test(size(),level(),input$h0type,real_test())
    )
  })
}

#produce the plot of theoretical testing
plotServer <- function(id, test_data, real_data, size, level,show,test,real_test){
  moduleServer(id,function(input,output,session){
    #generate the basic layer plot
    params <- params_list(id)
    g<-reactive({
      arrow_height<-max(c(test_data()$null()$val,test_data()$alter()$val))
      p<-ggplot(rbind(test_data()$null(),test_data()$alter()), aes(x=ax, y=val, group=type,fill=type)) + geom_area(alpha=0.1)+geom_line()
      if (show()$one %% 2 == 1){
        p<-p + geom_area(data = test_data()$null()[purrr::map_lgl(test_data()$null()$ax,~(.x>test()$upper)) ,],aes(x = ax,y = val), fill="brown", alpha=0.6)
        p<-p + geom_area(data = test_data()$null()[purrr::map_lgl(test_data()$null()$ax,~(.x<test()$lower)) ,],aes(x = ax,y = val), fill="brown", alpha=0.6)
      }
      if (show()$two %% 2 == 1){
        p<-p + geom_area(data = test_data()$alter()[purrr::map_lgl(test_data()$alter()$ax,~((.x<=test()$upper)&(.x>=test()$lower))) ,],aes(x = ax,y = val), fill="blue", alpha=0.6)
      }
      if (!is.na(real_test())){
        p<-p + geom_vline(xintercept = real_test(),linetype="dotdash",size=2)
      }
      if (show()$three %% 2 ==1){
        if (test()$lower!=-Inf){
          p <- p + geom_segment(x=-Inf,xend=test()$lower,y=arrow_height,yend=arrow_height,inherit.aes=FALSE, color="orange",
                                size=1.5, arrow = arrow(length = unit(0.5, "cm"))) + geom_vline(xintercept = test()$lower,size=1)
        }
        if (test()$upper!=Inf){
          p <- p + geom_segment(x=Inf,xend=test()$upper,y=arrow_height,yend=arrow_height,inherit.aes=FALSE, color="orange",
                                size=1.5, arrow = arrow(length = unit(0.5, "cm"))) + geom_vline(xintercept = test()$upper,size=1)
        }
        p <- p + geom_segment(x=test()$lower,xend=test()$upper,y=arrow_height,yend=arrow_height,inherit.aes=FALSE, color="green",
                              size=1.5, alpha = 0.4) + geom_vline(xintercept = test()$upper,size=1)
      }
      p
    })
    g()
  })
}

