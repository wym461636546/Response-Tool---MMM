#------------set work directory and load packages---#
library(shiny)
library(tidyverse)
library(shinydashboard)
library(DT)
library(readxl)
library(lubridate)
library(data.table)
#library(tidyr)
#library(janitor)
#library(hrbrthemes)
library(ggplot2)
library(patchwork)
library(lattice)
library(latticeExtra)
#-----------------------read your files------------#
#-----------------------raw support----------------#
support<-read_excel("my_data.xlsx",
                    sheet = "my_data")
#------------My data has 14 columns------------------#
names(support)<-c("week", paste("V",1:13,sep = "_"))
support<-setDT(support)
support<-support[,week:=ymd(week)]
#------------------replace NA------------------------#
support<-support%>%
  mutate(across(2:14, replace_na,0))

#exclude the week out of scope
support<-setDT(support)[weekend<=ymd("2022-01-02"),]
names(support)[1]<-"week"
#-------------extract some basic info------------#
basic_stats<-function(var, my_support){
  support<-my_support
  my_range<-range(support[get(var)>0, ..var])
  names(my_range)<-c("min_weekly","max_weekly")
  avg_weekly<-colMeans(support[get(var)>0, ..var])
  names(avg_weekly)<-"avg_weekly"
  basic_data<-c(my_range,avg_weekly)
  return(basic_data)
}

#-------------plot raw curves--------------------#
plot_raw_curve<-function(var,my_support,scale,hl,v,d,s){
  hl<-(0.5)^(1/hl)
  support<-my_support
  x<-support[,..var]/scale
  y<-stats::filter(x,hl,sides = 1, 
                   method="recursive")
  curve<-v*pweibull(y,shape = s, scale = 1/d,
                    lower.tail = TRUE)
  my_raw_reach<-data.frame(week=support$weekend,
                           raw_actvity = x,
                           adstock_activity=y,
                           lift=curve)
  names(my_raw_reach)<-c("week","raw_activity",
                         "adstock_activity","lift")
  obj1<-xyplot(raw_activity+adstock_activity ~ week, 
               data = my_raw_reach, 
               type="l", lwd=2,
               col=c("steelblue","cadetblue1"),
               xlab = "week",
               ylab = "weekly activity",
               scales=list(x=list(rot=45)),
               key=list(corner = c(1,1),
                        text = list(c("RAW", "ADSTOCK")),
                        lines = list(type = c("l", "l"), 
                                     col = c("steelblue","cadetblue1"),
                                     pch = 2, lwd = 4,cex=0.5,
                                     font=0.5,size=3)))
  obj2<-xyplot(lift ~ week,data = my_raw_reach, 
               lwd=2,
               type="l", col="violetred",
               key=list(corner = c(1,0),
                        text = list(c("LIFT")),
                        lines = list(type = c("l"), 
                                     col = c("violetred"),
                                     pch = 2, lwd = 4,cex=0.5,
                                     font=0.5,size=3)))
  doubleYScale(obj1, obj2, add.ylab2 = TRUE, 
               use.style=FALSE )
}
#------------plot predict lift---------------------#
plot_preidcted_curve<-function(var,my_support,scale,hl,v,d,s){
  support<-my_support
  my_limit<-max(support[,..var])/scale
  x<-seq(from=1, 
         to=ceiling(my_limit), 
         by=0.5)
  hl<-(0.5)^(1/hl)
  y<-stats::filter(x,hl,sides = 1, 
                   method="recursive")
  curve<-v*pweibull(y,shape = s, scale = 1/d,
                    lower.tail = TRUE)
  margin<-v*dweibull(y,shape = s, scale = 1/d,
                     log=FALSE)
  eff<-curve/y
  results<-data.frame(raw_grp=x,
                      adstock_grp=as.numeric(y),
                      curve= as.numeric(curve),
                      margin=as.numeric(margin),
                      effectiveness =as.numeric(eff))
  p1<-results%>%ggplot(aes(x=adstock_grp))+
    geom_line(aes(y=curve), colour="azure1")+
    scale_y_continuous(name = "response lift",
                       limits = ~c(0,max(.x)))+
    theme_ft_rc() 
  
  p2<-results%>%ggplot(aes(x=adstock_grp))+
    geom_line(aes(y= margin), colour="bisque2")+
    geom_line(aes(y= effectiveness), colour="cadetblue1")+
    scale_y_continuous(name = "Marginal & Eff",
                       limits = ~c(0,max(.x)))+
    theme_ft_rc()
  p1+p2
}
#--------all three lines at one chart--------------#
plot_3preidcted_curves<-function(var,my_support,scale,hl,v,d,s){
  support<-my_support
  my_limit<-max(support[,..var])/scale
  x<-seq(from=1, 
         to=ceiling(my_limit), 
         by=0.5)
  hl<-(0.5)^(1/hl)
  y<-stats::filter(x,hl,sides = 1, 
                   method="recursive")
  curve<-v*pweibull(y,shape = s, scale = 1/d,
                    lower.tail = TRUE)
  margin<-v*dweibull(y,shape = s, scale = 1/d,
                     log=FALSE)
  eff<-curve/y
  results<-data.frame(raw_grp=x,
                      adstock_grp=as.numeric(y),
                      curve= as.numeric(curve),
                      margin=as.numeric(margin),
                      effectiveness =as.numeric(eff))
  obj1<-xyplot(margin + effectiveness ~ adstock_grp, results, 
               main="Response Curves",
               type = "l", col=c("steelblue", "#69b3a2") ,
               lwd=2)
  
  obj2<-xyplot(curve~adstock_grp, results,
               type="l", col="violetred")
  doubleYScale(obj1, obj2, add.ylab2 = TRUE, 
               use.style=FALSE)
}
#--------------get the threshold points-------------#
threshold_points<-function(var,my_support,scale,hl,v,d,s){
  support<-my_support
  avg_weekly<-colMeans(support[get(var)>0, ..var])/scale
  names(avg_weekly)<-"avg_weekly"
  my_limit<-max(support[,..var])/scale
  x<-seq(from=1, 
         to=ceiling(my_limit), 
         by=0.5)
  hl<-(0.5)^(1/hl)
  y<-stats::filter(x,hl,sides = 1, 
                   method="recursive")
  curve<-v*pweibull(y,shape = s, scale = 1/d,
                    lower.tail = TRUE)
  margin<-v*dweibull(y,shape = s, scale = 1/d,
                     log=FALSE)
  eff<-curve/y
  results<-data.frame(raw_grp=x,
                      adstock_grp=as.numeric(y),
                      curve= as.numeric(curve),
                      margin=as.numeric(margin),
                      effectiveness =as.numeric(eff))
  a<-results[max(results$effectiveness)==results$effectiveness,c("margin")]
  d<-results[max(results$margin)==results$margin,c("effectiveness")]
  my_margin<-results[abs(results$margin-a)<=sort(abs(results$margin-a))[2],c("raw_grp")]
  names(my_margin)<-c("A","C")
  my_eff<-results[abs(results$effectiveness-d)<=sort(abs(results$effectiveness-d))[2],c("raw_grp")]
  names(my_eff)<-c("B","D")
  my_points<-c(my_margin,my_eff, avg_weekly)
  return(my_points[c("A","B","C","D","avg_weekly")])
  # return(results)
}
#--------------for curves chart-------------------#
curve_show_threshold<-function(var,my_support,scale,hl,v,d,s){
  support<-my_support
  avg_weekly<-colMeans(support[get(var)>0, ..var])/scale
  my_limit<-max(support[,..var])/scale
  x<-seq(from=1, 
         to=ceiling(my_limit), 
         by=0.5)
  hl<-(0.5)^(1/hl)
  y<-stats::filter(x,hl,sides = 1, 
                   method="recursive")
  curve<-v*pweibull(y,shape = s, scale = 1/d,
                    lower.tail = TRUE)
  margin<-v*dweibull(y,shape = s, scale = 1/d,
                     log=FALSE)
  eff<-curve/y
  results<-data.frame(raw_grp=x,
                      adstock_grp=as.numeric(y),
                      curve= as.numeric(curve),
                      margin=as.numeric(margin),
                      effectiveness =as.numeric(eff))
  a<-results[max(results$effectiveness)==results$effectiveness,c("margin")]
  d<-results[max(results$margin)==results$margin,c("effectiveness")]
  my_margin<-results[abs(results$margin-a)<=sort(abs(results$margin-a))[2],c("raw_grp","adstock_grp","curve")]
  rownames(my_margin)<-c("A","C")
  my_eff<-results[abs(results$effectiveness-d)<=sort(abs(results$effectiveness-d))[2],c("raw_grp","adstock_grp","curve")]
  rownames(my_eff)<-c("B","D")
  avg_eff<-results[abs(results$raw_grp-avg_weekly)<=sort(abs(results$raw_grp-avg_weekly))[1],c("raw_grp","adstock_grp","curve")]
  rownames(avg_eff)<-c("avg_weekly")
  my_points<-bind_rows(my_margin, my_eff,avg_eff)
  my_points<-my_points[c("A","B","C","D","avg_weekly"),]
  crutial_points<-paste(rownames(my_points),
               as.numeric(my_points[,c("raw_grp")]),
               collapse = ", ")
  #plot
   p1<-results%>%ggplot()+
    geom_line(aes(x=adstock_grp,y=curve), 
              colour="steelblue",size=1.2)+
    scale_y_continuous(name = "response lift",
                       limits = ~c(0,max(.x)))+
     labs(title = "Response/reach with Crucial Points",
          subtitle = paste("Please Select GRP Scale at First;  ",crutial_points))+
    theme_bw() + 
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black"))
  p1+geom_point(data=my_points, aes(adstock_grp, curve),size=2.5,
                colour=c("coral3","deepskyblue",
                         "firebrick","dimgray","deeppink"))+
    geom_text(data=my_points,aes(adstock_grp, curve, 
                         label=c("A","B","C","D","Avg_weekly")),
              check_overlap = TRUE, colour="black",
              nudge_x = 0.05,hjust="left",
              vjust="top")
  # return(results)
}

#------------------test-----------------------#
  basic_stats("V_1",
            my_support = support)
plot_raw_curve("V_1",
               my_support = support, hl=0.3,
               scale = 1000, v= 0.046930462000,
               d= 0.0000267,
               s= 1.760488775000)

plot_preidcted_curve("V_1",
               my_support = support, hl=0.3,
               scale = 1000, v= 0.046930462000,
               d= 0.0000267,
               s= 1.760488775000)
plot_3preidcted_curves("V_1",
                       my_support = support, hl=0.3,
                       scale = 10000, v= 0.046930462000,
                       d= 0.000267,
                       s= 1.760488775000)

threshold_points("V_1",
                 my_support = support, hl=0.3,
                 scale = 1000, v= 0.046930462000,
                 d= 0.0000267,
                 s= 1.760488775000)
curve_show_threshold("V_1",
                     my_support = support, hl=0.3,
                     scale = 10000, v= 0.046930462000,
                     d= 0.000267,
                     s= 1.760488775000)

#----------define shiny APP----------------#
# names(support)
dataset<-support

ui<-dashboardPage(
  dashboardHeader(title = "Practice Curves"),
  dashboardSidebar(
    sidebarMenu(
       menuItem("General", tabName = "General"),
       menuItem("Response", tabName = "Response"),
       menuItem("MKT Activity", tabName = "MKT_Activity")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("General",
              fluidPage(title = "Key_Threshold",
            sidebarPanel(
             selectInput(inputId = "grp_scale",
                  label = "grp_scale",
                  choices = c(1,10,100,1000,10000,
                          100000,1000000,10000000)),
             selectInput(inputId = "Variable",
                         label = "Variable",
                         choices =  names(support)[-1]), 
             numericInput(inputId = "HL",
                  label = "HL",
                  min=0.1, max=4,value= 0.5),
            numericInput(inputId = "Volume",
                  label = "Volume",
                  min=0, max=1,value= 0.01),
            numericInput(inputId = "Driver",
                  label = "Driver",
                  min=0, max=1,value=0.001),
            numericInput(inputId = "Symmetry",
                  label = "Symmetry",
                  min=1, max=5,value=1.5)),
      mainPanel(
        plotOutput("Thresholds"))
              )),
      tabItem("Response",
              fluidPage(title = "Response_Chart",
              sidebarPanel(width=3,
                selectInput(inputId = "grp_scale_1",
                    label = "grp_scale",
                    choices = c(1,10,100,1000,10000,
                   100000,1000000,10000000)),
             selectInput(inputId = "Variable_1",
                         label = "Variable",
                         choices =  names(support)[-1]),
             numericInput(inputId = "HL_1",
                          label = "HL",
                          min=0.1, max=4,value= 0.5),
              numericInput(inputId = "Volume_1",
                           label = "Volume",
                           min=0, max=1,value= 0.01),
              numericInput(inputId = "Driver_1",
                          label = "Driver",
                          min=0, max=1,value=0.001),
              numericInput(inputId = "Symmetry_1",
                          label = "Symmetry",
                          min=1, max=5,value=1.5)),
              mainPanel(
                plotOutput("predicted_actual"),
                width=9
              )
              )),
      tabItem("MKT_Activity", 
          fluidPage(h1("MKT RAW Activity"),
          dataTableOutput("mytable")
       ))
    ))
)

#-----------------define server---------------------#
server<-function(input, output){
  dataset<-support
  output$Thresholds<-renderPlot(
    {
      curve_show_threshold(as.character(input$Variable),
                             my_support = dataset,
                             hl=as.numeric(input$HL),
                             scale = as.numeric(input$grp_scale), 
                             v= as.numeric(input$Volume),
                             d= as.numeric(input$Driver),
                             s= as.numeric(input$Symmetry))
    })
  output$predicted_actual<-renderPlot(
    {
      plot_3preidcted_curves(as.character(input$Variable_1),
                             my_support = dataset,
                             hl=as.numeric(input$HL_1),
                             scale = as.numeric(input$grp_scale_1), 
                             v= as.numeric(input$Volume_1),
                             d= as.numeric(input$Driver_1),
                             s= as.numeric(input$Symmetry_1))
    })
  output$mytable<-DT::renderDataTable(dataset,
                options = list(pageLength = 10, 
                width="100%",
                scrollX = TRUE))
}

shinyApp(ui, server)

