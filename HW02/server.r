library(shiny)
library(dplyr)
library(plotly)


shinyServer(function(input, output) {
  datasetInput <- reactive({
    if (input$dataset == "Injuries"){
      
    data=x[1:input$n,]%>%
      filter(District.Name == input$loc)%>%
      group_by(Month) %>%  
      summarise(Accidents = sum(Mild.injuries, na.rm=TRUE))
   
    
    }else if (input$dataset == "Victims"){
    
      data=x[1:input$n,]%>%
        filter(District.Name == input$loc)%>%
        group_by(Month) %>%  
        summarise(Victims = sum(Victims, na.rm=TRUE))
    } else if (input$dataset == "Vehicles"){
      
      data=x[1:input$n,]%>%
        filter(District.Name == input$loc)%>%
        group_by(Month) %>%  
        summarise(Vehicles = sum(Vehicles.involved, na.rm=TRUE))
    }
    
    
  })
  
  
  
  output$lineplot <- renderPlotly({
    yy=datasetInput()
    if (input$dataset == "Injuries"){
    plot_ly(yy, x = ~Month, y = ~Accidents, type = 'scatter', mode = 'lines')
    }else if (input$dataset == "Victims"){
      plot_ly(yy, x = ~Month, y = ~Victims, type = 'scatter', mode = 'lines')
    } else if (input$dataset == "Vehicles"){
      plot_ly(yy, x = ~Month, y = ~Vehicles, type = 'scatter', mode = 'lines')
    }  
  })
  
  
  
  output$barplot <- renderPlotly({
    dataset = datasetInput()
    if (input$dataset == "Injuries"){
      plot_ly(dataset, x = ~Month, y = ~Accidents, type = 'bar')
    }else if (input$dataset == "Victims"){
      plot_ly(dataset, x = ~Month, y = ~Victims, type = 'bar')
    } else if (input$dataset == "Vehicles"){
      plot_ly(dataset, x = ~Month, y = ~Vehicles, type = 'bar')
    }  
    
 
    
  })
  
  
  
  output$pieplot <- renderPlotly({
    
    yy = datasetInput()
    if (input$dataset == "Injuries"){
      plot_ly(yy, labels = ~Month, values = ~Accidents, type = 'pie')%>%
        layout(showlegend = T,title = 'Vehicles',xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }else if (input$dataset == "Victims"){
      plot_ly(yy, labels = ~Month, values = ~Victims, type = 'pie')%>%
        layout(showlegend = T,title = 'Vehicles',xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    } else if (input$dataset == "Vehicles"){
      plot_ly(yy, labels = ~Month, values = ~Vehicles, type = 'pie')%>%
      layout(showlegend = T,title = 'Vehicles',xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    }  
  })
})
