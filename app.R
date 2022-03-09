################################################################################
#                                                                              #
#                         BUILDING SHINY APP                                   #                                                                                                                                     #
################################################################################
#Following this tutorial: https://deanattali.com/blog/building-shiny-apps-tutorial/
#Other very useful tutorial: https://shiny.rstudio.com/tutorial/
#Working with plotly and shiny: https://plotly-r.com/linking-views-with-shiny.html
library(scales)
library(shiny)
library(tidyr)
library(ggplot2)
library(dplyr)
library(openxlsx)
library(plotly)
library(shinythemes)
library(shinydashboard)
library(rsconnect)
library(DT)

library(dplyr)


string <- "<p>This interactive tool is designed to show the predicted impact of the Covid-19 pandemic on cancer outcomes in Canada.
         The tool displays results from a mathematical modelling study that predicts the impact of cancer treatment and diagnosis
         delays during the Covid-19 pandemic in Canada on cancer incidence and deaths:</p>"

stringp2 <-"<br><p>This tool shows more detailed results than those available in the manuscript (e.g. outcomes by cancer type, province) and allows
         you to modify some of the key assumptions to see how the results change accordingly.This tool is not meant to be a healthcare system capacity planning tool. The scenarios of <span>&#177;</span>10<span>&#37;</span> system capacity increase are
         meant to illustrate the health effects that could be expected over time if the volume of cancer diagnostic procedures and 
         treatments could be increased or if these remain below pre-pandemic normal levels. The model does not explore what resources 
         would be needed to increase system capacity nor the most cost-effective manner to do so.</p>"


stringp21 <-"
         <p>The results in this interactive tool are based on a simulation model with the following features:</p>
         
         <ul>
            <li>The model simulates individual cancer cases and their healthcare trajectory </li>
            <li>Cancer incidence and survival are based on Canadian incidence rates and survival statistics prior to the pandemic (2015-2017).Cancer incidence and survival depend on age, sex, site, and cancer stage.
            </li>
            <li>Patients in the model can receive surgery, radiotherapy, chemotherapy, and combinations thereof as treatments.
            </li>
           <li>The COVID-19 pandemic leads to delays in the diagnosis of a cancer and in the scheduling of treatment for that cancer. Missed diagnoses get placed on a diagnostic backlog and delayed treatments get placed on a treatment backlog, which can be cleared when diagnostic and treatment capacity increase.</li>
            <li>The total duration of delays experienced by a patient increases their risk of dying of cancer. In most simulations, a 4-week delay leads to a 6% increase in the rate of cancer death. This value can be changed in sensitivity analyses.</li>
            <li>Life years lost are calculated based on the difference between a person's death date with pandemic-related delays compared to their expected death date without delays.</li>
         </ul>  " 



                               
string3<- "<li>Model developed by Dr Tal&iacute;a Malag&oacute;n and Dr Eduardo Franco (McGill University) for the McGill Task Force on the Impact of COVID-19 on Cancer Control and Care.
          </li>
          <li>R & Shiny app implementation: Elba Gomez Navas and Jean Yong (Canadian Partnership Against Cancer)
          </li>
          <li>Funding: Model development was supported by the Canadian Institutes of Health Research [operating grant VR5-172666 and foundation grant 143347 to Eduardo Franco].
          The model simulations were run using the supercomputer Beluga from Ecole de technologie superieure, managed by Calcul Quebec (www.calculquebec.ca/) and Compute Canada (www.computecanada.ca).
          The operation of this supercomputer is funded by the Canada Foundation for Innovation (CFI), Ministere de l'Economie, des Sciences et de l'Innovation du Quebec (MESI)
          and the Fonds de recherche du Quebec - Nature et technologies (FRQ-NT). This companion web application was developed with in-kind support from the Canadian Partnership Against Cancer.
          </li>
          </ul>"
         


string4 <- "<p>Full report (preprint): Tal&iacute;a Malag&oacute;n, Jean H.E. Yong, Parker Tope, Wilson H. Miller Jr., Eduardo L. Franco, for the McGill Task Force on the Impact 
of COVID-19 on Cancer Control and Care. Predicted long-term impact of COVID-19 pandemic-related care delays on cancer incidence and mortality in Canada. 
medRxiv 2021.08.26.21261149; doi: https://doi.org/10.1101/2021.08.26.21261149</p>
     <p>Contact information:</p>
     Dr Tal&iacute;a Malag&oacute;n: talia.malagon@mcgill.ca"

string2 <- "<ul>
  <li><b>Surgeries</b>: The percent change in the model is based on data on the volume of cancer surgeries in 2020-2021 
  relative to the same month in 2019, using data from the Quebec Ministry of Health and Social Services (Quebec) 
  extracted on April 30th 2021 and from the Canadian Institute of Health Information portal (other provinces and
  Canada total) extracted on May 28th 2021.</li>
  <li><b>Radiotherapies</b>:The percent change in the model is based on data on the yearly volume of radiotherapies in 
  2020 relative to 2019 from the Canadian Institute of Health Information portal (other provinces and Canada total).
  The  yearly percent change was rescaled per month so that the total over all months would equal the yearly percent change.
  While Canada-level simulations include a decline in radiotherapies, we assumed 0% change in radiotherapies in 
  province-specific simulations for NL, NS, Manitoba, Saskatchewan, Alberta, and BC as these provinces did not 
  report an overall decline in the volume of radiotherapies in 2020 relative to 2019.</li>
  <li><b>Chemotherapies</b>: We assumed chemotherapies would experience the same percent changes as radiotherapies due to a 
  lack of data.</li>
<li><b>Diagnoses</b>: We assumed diagnoses would experience the same percent changes as surgeries due to a lack of data, 
except for the province of Quebec where the percent changes are based on monthly volumes of pathology reports.</li>
<li>AB=Alberta; BC=British Columbia; MB=Manitoba; NB=New Brunswick; NL=Newfoundland and Labrador; NS=Nova Scotia;
ON=Ontario; PEI=Prince Edward Island; QC=Quebec; SK=Saskatchewan.</li>
</ul>  
"

string_footnote <- "Cumulative number of excess cancer deaths attributable to pandemic-related diagnostic and treatment delays in the province between 
2020-2030, and the number of life-years lost due to these excess cancer deaths. All cancer sites combined. The assumptions regarding the effect of delays
on cancer mortality can be specified by the user in the left-hand menu; our base case assumption is that each 4-week delay in either diagnosis or treatment
leads to a 6% increase in the cancer mortality rate (hazard ratio of 1.06) based on:
<ul>
<li><b>0% change</b>: Cancer treatment capacity returns to pre-pandemic normal
levels from June 2021 onwards.
<li><b>10% more</b>: Cancer treatment capacity is increased by 10% over pre-pandemic levels from June 2021. This allows clearing any accumulated 
backlogs in treatment prior to June 2021, but also allows to treat excess cancer cases expected to be diagnosed later due to delays in diagnosis during 2020-2021.
<li><b>10% less</b>: Cancer treatment capacity remains 10% below pre-pandemic levels between June 2021-December 2021. The assumption is that 
this decline would be due to a continued pressure of the COVID-19 pandemic on the health system, leading to an increase in the treatment
backlog over this time period and to further delays in cancer treatment.</ul> 
References: Hanna T P, King W D, Thibodeau S, Jalink M, Paulin G A, Harvey-Jones E et al. Mortality due to cancer treatment delay: systematic review
"

string_footnote2 <- "
Cumulative number of excess cancer deaths attributable to pandemic-related diagnostic and treatment delays in the province between 2020-2030, 
and the number of life-years lost due to these excess cancer deaths. All cancer sites combined. The user may specify whether to stratify results by sex or by age 
in the left-hand menu. Sex-and age-specific estimates can be obtained by hovering the mouse over the figure.
<ul>
<li><b>0% change</b>: Cancer treatment capacity returns to pre-pandemic
normal levels from June 2021 onwards.
<li><b>10% more</b>: Cancer treatment capacity is increased by 10% over pre-pandemic levels from June 2021. This allows clearing any accumulated backlogs in treatment prior to June 2021, but also allows to treat excess cancer cases expected to be diagnosed later due to delays in diagnosis during 2020-2021.
<li><b>10% less</b>: Cancer treatment capacity remains 10% below pre-pandemic levels between June 2021-December 2021. 
The assumption is that this decline would be due to a continued pressure of the COVID-19 pandemic on the health system, 
leading to an increase in the treatment backlog over this time period and to further delays in cancer treatment.</ul> 

Results include random year-to-year variability in predicted cancer outcomes by demographic subgroup (sex, age); for this reason, 
the total sum of outcomes in all subgroups is not equal to the overall prediction for the whole population in the previous tab (Sensitivity Analysis).
It is recommended to use the Sensitivity Analysis tab results for whole population predictions, and the Subgroup Analysis tab for subgroup-specific predictions.

"
sample_data <- read.xlsx("Data.xlsx", sheet =1)
sample_data2 <- read.xlsx("Data.xlsx", sheet =2)
sample_data3 <- read.xlsx("Data.xlsx", sheet =3)


sample_data <- sample_data%>% filter(Year == 2030)%>%
  rename(Excess.Death  = Excess.Death.All)%>%
  rename(Lifey.Lost  = Lifey.Lost.All)%>%
  distinct()

sample_data$Capacity.Increase <- factor(sample_data$Capacity.Increase, levels = c("10% less", "0% change", "10% more"))
sample_data2$Capacity.Increase <- factor(sample_data2$Capacity.Increase, levels = c("10% less", "0% change", "10% more"))


sample_data3 <- sample_data3 %>% mutate(Date = as.Date(Date, origin = "1899-12-30"))%>%
  select(-c("Year", "Month"))%>%
  gather(Country, Value, 3:13)

#Create percentages for sample data 3 
sample_data3$Value <- sample_data3$Value*100

#Step 2: Formatting (pseudo-html)
#Note: Remember that all the arguments inside navbarPage() need to be separated by commas.
shinyApp(
ui <- navbarPage(
                 #Title and theme
                 "Predicted Impact of COVID-19 on Cancer Outcomes (2020-2030)",
                  theme = shinytheme("flatly"),
                 
                 #First tab 
      tabPanel("About",
               tags$h4(HTML("<b>About</b>")),
               tags$div("This interactive tool is designed to show the predicted impact of Covid-19 pandemic on cancer outcomes in Canada.
         The tool displays results from a mathematical modelling study that predicts the impact of cancer treatment and diagnosis
         delays during the Covid-19 pandemic in Canada on cancer incidence and deaths:", style = "font-size:14px;", tags$a(href="https://www.medrxiv.org/content/10.1101/2021.08.26.21261149v1", 
                      "https://doi.org/10.1101/2021.08.26.21261149 ")),
               tags$div(HTML(stringp2),style = "font-size:14px;"),
               tags$h4(HTML("<b>Model methods and information</b>")),
               tags$div(HTML(stringp21),style = "font-size:14px;"),
               tags$div("To learn more about the model, please refer to the full report:",tags$a(href="https://www.medrxiv.org/content/10.1101/2021.08.26.21261149v1", 
                                                                                                 "https://doi.org/10.1101/2021.08.26.21261149 "),"and the model technical documentation:",
                        tags$a(href="https://www.medrxiv.org/content/10.1101/2021.08.26.21261149v1", 
                               "https://doi.org/10.1101/2021.08.26.21261149")),
               tags$h4(HTML("<b>Model Development</b>")),
               tags$div(HTML(string3),style = "font-size:14px;"),
               tags$h4(HTML("<b>Suggested Citation</b>")),
               tags$div(HTML(string4),style = "font-size:14px;")
               
        ),
 
      tabPanel("Model Assumptions",sidebarLayout(
        sidebarPanel(
          h1(id="big-heading1", "Parameters"),
          tags$style(HTML("#big-heading1{color: #202020;font-size: 20px;text-align: center;}")),
          
          #[Parameter 1] Province
          checkboxGroupInput("checkInputM", "Province", selected = c("Canada"),  c("QC", "Canada", "NL","PEI", "NS", "NB","ON", "MB", "SK", "AB", "BC")), 
          
          #[Parameter 2] Cancer Type
          uiOutput("AssumptOutput")
          
        ),
        mainPanel(fluidRow(
          
          h1(id="big-heading1", HTML("<br>")),
          h1(id="big-heading1AB", HTML("<br>")),
          column(12, plotlyOutput("coolplotA", height = "100%"))
        ),
        h5(id="big-heading5",h6(HTML(string2)))))),
      
       
      
      tabPanel("Sensitivity Analysis",
                sidebarLayout(
                      sidebarPanel(
                                  h1(id="big-heading1", "Parameters"),
                                  tags$style(HTML("#big-heading1{color: #202020;font-size: 20px;text-align: center;}")),
                      #[Parameter 1] Province
                      uiOutput("jurisOutput"),
                     
                      radioButtons("typeInput3", HTML("<p>Effect of treatment delays on cancer survival-- Mortality hazard ratios for every 4-week diagnostic + treatment delay, by cancer site:</p> <i>Breast Cancer</i>"),
                                   choices = c("1.03", "1.06","1.5"),
                                  selected = "1.06",inline = TRUE),
                     
                     radioButtons("typeInput4", HTML("<i>Colorectal Cancer</i>"),
                                  choices = c("1.03", "1.06","1.5"),
                                  selected = "1.06",inline = TRUE),
                     
                     radioButtons("typeInput5", HTML("<i> Lung Cancer</i>"),
                                  choices = c("1.03", "1.06","1.5"),
                                  selected = "1.06",inline = TRUE),
                     
                     radioButtons("typeInput6", HTML("<i>Oral Cancer</i>"),
                                  choices = c("1.03", "1.06","1.5"),
                                  selected = "1.06",inline = TRUE),
                     
                     radioButtons("typeInput7", HTML("<i>Other</i>"),
                                  choices = c("1.03", "1.06","1.5"),
                                  selected = "1.06",inline = TRUE)
    
      
                    ),
                    mainPanel(

                 fluidRow(
                  
                   h1(id="big-heading1", HTML("<br><br>")),
                   h1(id="big-heading1D2", HTML("<br>")),
                          column(6, plotlyOutput("coolplot")),
                          column(6, plotlyOutput("coolplot2"))
                 ),
        
                h5(id="big-heading5", HTML(string_footnote))))),
 tabPanel("Subgroup Analysis",
          sidebarLayout(
            sidebarPanel(
              h1(id="big-heading1", "Parameters"),
              tags$style(HTML("#big-heading1{color: #202020;font-size: 20px;text-align: center;}")),
              
              uiOutput("jurisOutput0"),
              
              
              uiOutput("SocioOutput")

              
            ),
            mainPanel( fluidRow(
              
              h1(id="big-heading1", HTML("<br><br>")),
              h1(id="big-heading1", HTML("<br>")),
              column(6, plotlyOutput("coolplotC")),
              column(6, plotlyOutput("coolplot2C")),
              h5(id="big-heading5", HTML(string_footnote2))
            ))))
 
 
  ),

server <- function(input, output) {
  
  #For Model Assumptions tab 
  output$AssumptOutput <- renderUI({
    selectInput("AssumptInputM","Treatment Type",
                sort(unique(sample_data3$Treatment.Type)),
                selected = "Surgeries")
    
    
  })  
  
  
  filteredM <- reactive({
    if (is.null(input$AssumptInputM)) {
      return(NULL)
    }    
    
    sample_data3 %>%
      filter(
        Country %in% input$checkInputM,
        Treatment.Type == input$AssumptInputM,
      )
  })
  
  output$coolplotA <- renderPlotly({
    if (is.null(filteredM())) {
      return()
    }
    
    colors2 <- c('#FFA500', '	#4169E1', '#696969')
    
    x3 <- plot_ly(filteredM(), x = ~Date, y = ~Value, color = ~Country) %>% add_lines()
    
    x3 <-  x3%>% layout(margin = list(b = 160), title = paste0("Percent Change in Cancer ", input$AssumptInputM, " 2019 vs 2020-21"),
                       xaxis = list(title = ""),
                       yaxis = list(title = ""),
                       annotations = 
                         list(x = 0.5, y = -0.15, text = "", 
                              showarrow = F, xref='paper', yref='paper', 
                              xanchor='right', yanchor='auto', xshift=0, yshift=0,
                              font=list(size=15, color="grey")))
    
    
    x3
    
  })
  
  #For Sensitivity Analysis Tab 
  output$jurisOutput <- renderUI({
    selectInput("jurisInput","Province",
                sort(unique(sample_data$Geography)),
                selected = "BC")
 
       
  })  

  
  filtered <- reactive({
    if (is.null(input$jurisInput)) {
      return(NULL)
    }    
    
    sample_data %>%
    
    filter(Geography == input$jurisInput,
           Cancer.Type == "Breast" & Hazard.Ratio == input$typeInput3 | Cancer.Type != "Breast",
           Cancer.Type == "Colorectal" & Hazard.Ratio == input$typeInput4 | Cancer.Type != "Colorectal",
           Cancer.Type == "Lung" & Hazard.Ratio == input$typeInput5 | Cancer.Type != "Lung",
           Cancer.Type == "Oral" & Hazard.Ratio == input$typeInput6 | Cancer.Type != "Oral",
           Cancer.Type == "Others" & Hazard.Ratio == input$typeInput7 | Cancer.Type != "Others")%>%
  
      group_by(Capacity.Increase,Geography )%>%
      mutate(Excess.Death1 = sum(Excess.Death), Lifey.Lost1 = sum(Lifey.Lost))%>%
      select(c("Excess.Death1","Lifey.Lost1","Capacity.Increase"))%>%
      distinct()#%>%
  })
  
  
  output$coolplot <- renderPlotly({
    if (is.null(filtered())) {
      return()
    }

    colors2 <- c('#FFA500', '	#4169E1', '#696969')
    
    x<- plot_ly(filtered(), x = ~Excess.Death1, y = ~Capacity.Increase, type = 'bar',
                text = ~Excess.Death1, textposition = 'auto', 
                marker = list(color = colors2,
                              line = list(color = 'rgb(8,48,107)', width = 1.5)),orientation = 'h')
    
    
    x1<-  x %>% layout(title = "Excess Cancer Deaths",
                       xaxis = list(title = ""),
                       yaxis = list(title = "Treatment capacity in Jun-Dec 2021"))
    
    x1
  })
  
  output$coolplot2 <- renderPlotly({
    if (is.null(filtered())) {
      return()
    }

    colors2 <- c('#FFA500', '	#4169E1', '#696969')
    
    x<- plot_ly(filtered(), x = ~Lifey.Lost1, y = ~Capacity.Increase, type = 'bar',
                text = ~Lifey.Lost1, textposition = 'auto', 
                marker = list(color = colors2,
                              line = list(color = 'rgb(8,48,107)', width = 1.5)),orientation = 'h')
    
    
    x2<-  x %>% layout(title = "Life-years Lost",
                       xaxis = list(title = ""),
                       yaxis = list(title = "Treatment capacity in Jun-Dec 2021"))
    
    x2
    
  })

  
  #For Subgroup analysis tab 
  output$jurisOutput0 <- renderUI({
    selectInput("jurisInput0","Province",
                sort(unique(sample_data2$Geography)),
                selected = "BC")
    
    
  }) 
  
  output$SocioOutput <- renderUI({
    selectInput("SocioInput","Demographic Characteristics",
                sort(unique(sample_data2$Var)),
                selected = "Sex")
    
  })  
  
  filteredS <- reactive({if (is.null(input$SocioInput)) {
    return(NULL)
  }    
    
    sample_data2 %>%
      filter(# >= input$typeInput,
        Geography == input$jurisInput0,
        Var == input$SocioInput,
      )
  })
  
  output$coolplotC <- renderPlotly({
    if (is.null(filteredS())) {
      return()
    }
  
    
    if (input$SocioInput == "Sex"){
      
      fig <- plot_ly(filteredS(), x = ~Excess.Death.Female, y = ~Capacity.Increase, type = 'bar', orientation = 'h',name = 'Female',
                     marker = list(color = '#FFA500',
                                   line = list(color = 'rgba(58, 71, 80, 0.6)',
                                               width = 1)))
      
      fig <- fig %>% add_trace(x = ~Excess.Death.Male, name = 'Male',
                               marker = list(color = 'rgba(58, 71, 80, 0.6)',
                                             line = list(color = 'rgba(58, 71, 80, 0.6)',
                                                         width = 1)))
      fig <- fig %>% layout(barmode = 'stack',
                            xaxis = list(title = ""),
                            yaxis = list(title ="Treatment capacity in Jun-Dec 2021"),
                            title = "Excess Cancer Deaths")
      
      fig
      
      
      
    } else{
      fig <- plot_ly(filteredS(), x = ~`Excess.Death.15-44`, y = ~Capacity.Increase, type = 'bar', orientation = 'h',name = '15-44',
                     marker = list(color = '#DCDCDC',
                                   line = list(color = 'rgba(58, 71, 80, 0.6)',
                                               width = 1)))
      
      fig <- fig %>% add_trace(x = ~`Excess.Death.45-54`, name = '45-54',
                               marker = list(color = '#BEBEBE',
                                             line = list(color = 'rgba(58, 71, 80, 0.6)',
                                                         width = 1)))
      
      fig <- fig %>% add_trace(x = ~`Excess.Death.55-64`, name = '55-64',
                               marker = list(color = '#989898',
                                             line = list(color = 'rgba(58, 71, 80, 0.6)',
                                                         width = 1)))
      
      fig <- fig %>% add_trace(x = ~`Excess.Death.65-74`, name = '65-74',
                               marker = list(color = '#696969',
                                             line = list(color = 'rgba(58, 71, 80, 0.6)',
                                                         width = 1)))
      
      fig <- fig %>% add_trace(x = ~`Excess.Death.75-84`, name = '75-84',
                               marker = list(color = '#404040',
                                             line = list(color = 'rgba(58, 71, 80, 0.6)',
                                                         width = 1)))
      
      fig <- fig %>% add_trace(x = ~`Excess.Death.85+`, name = '85+',
                               marker = list(color = '#181818',
                                             line = list(color = 'rgba(58, 71, 80, 0.6)',
                                                         width = 1)))
      
      fig <- fig %>% layout(barmode = 'stack',
                            xaxis = list(title = ""),
                            yaxis = list(title ="Treatment capacity in Jun-Dec 2021"),
                            title = "Excess Cancer Deaths")
      
      fig
    }
    
    
    
  })
  
  output$coolplot2C <- renderPlotly({
    if (is.null(filteredS())) {
      return()
    }
    

    if (input$SocioInput == "Sex"){
      
      fig <- plot_ly(filteredS(), x = ~Lifey.Lost.Female, y = ~Capacity.Increase, type = 'bar', orientation = 'h',name = 'Female',
                     marker = list(color = '#FFA500',
                                   line = list(color = 'rgba(58, 71, 80, 0.6)',
                                               width = 1)))
      
      fig <- fig %>% add_trace(x = ~Lifey.Lost.Male, name = 'Male',
                               marker = list(color = 'rgba(58, 71, 80, 0.6)',
                                             line = list(color = 'rgba(58, 71, 80, 0.6)',
                                                         width = 1)))
      fig <- fig %>% layout(barmode = 'stack',
                            xaxis = list(title = ""),
                            yaxis = list(title ="Treatment capacity in Jun-Dec 2021"),
                            title = "Life-years Lost")
    } else{
      fig <- plot_ly(filteredS(), x = ~`Lifey.Lost.15-44`, y = ~Capacity.Increase, type = 'bar', orientation = 'h',name = '15-44',
                     marker = list(color = '#DCDCDC',
                                   line = list(color = 'rgba(58, 71, 80, 0.6)',
                                               width = 1)))
      
      fig <- fig %>% add_trace(x = ~`Lifey.Lost.45-54`, name = '45-54',
                               marker = list(color = '#BEBEBE',
                                             line = list(color = 'rgba(58, 71, 80, 0.6)',
                                                         width = 1)))
      
      fig <- fig %>% add_trace(x = ~`Lifey.Lost.55-64`, name = '55-64',
                               marker = list(color = '#989898',
                                             line = list(color = 'rgba(58, 71, 80, 0.6)',
                                                         width = 1)))
      
      fig <- fig %>% add_trace(x = ~`Lifey.Lost.65-74`, name = '65-74',
                               marker = list(color = '#696969',
                                             line = list(color = 'rgba(58, 71, 80, 0.6)',
                                                         width = 1)))
      
      fig <- fig %>% add_trace(x = ~`Lifey.Lost.75-84`, name = '75-84',
                               marker = list(color = '#404040',
                                             line = list(color = 'rgba(58, 71, 80, 0.6)',
                                                         width = 1)))
      
      fig <- fig %>% add_trace(x = ~`Lifey.Lost.85+`, name = '85+',
                               marker = list(color = '#181818',
                                             line = list(color = 'rgba(58, 71, 80, 0.6)',
                                                         width = 1)))
      
      fig <- fig %>% layout(barmode = 'stack',
                            xaxis = list(title = ""),
                            yaxis = list(title ="Treatment capacity in Jun-Dec 2021"),
                            title = "Life-years Lost")
      
      fig
      
    }    
    
    
  })
  
  
})
