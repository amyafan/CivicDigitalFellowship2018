#setwd("C:/Users/oom1/Desktop/R/RShiny/")
library(shiny)
library(tidyr)
library(ggplot2)
library(plotly)
library(shinythemes)

a<- read.csv("table063.csv")

ui <- fluidPage(
  theme = shinytheme("flatly"),
	titlePanel("Table 63"),

	#input
	sidebarLayout(
		sidebarPanel(

			#select metric
			radioButtons(
				inputId= "metric", 
				label= "Select metric:",
				choices= unique(a$Metric) 
			), 
			
			#select theme
			selectInput(
			  inputId="theme",
			  label= "Select theme",
			  choices= c("Demographic", "Socioeconomic", "Healthcare", "Geographic")
			),
			
			#select stub variable 
			selectInput( 
				inputId="stub",
				label= "Select category", 
				choices = c("Age" = unique(a$Group)[1],
				           "Sex" = unique(a$Group)[2], 
				          "Race"= unique(a$Group)[3],
				         "Race and Ethnicity"= unique(a$Group)[4])
			) , 
			
			#optional panel for poverty 
			
			uiOutput(outputId = 'dynamicInput'),
			
			#select subcategory
			checkboxGroupInput(
			  inputId= "subcat", 
			  label= "Select subcategory", 
			  choices= unique(a[a$Group=="Age",]$Characteristic),
			  selected= unique(a[a$Group=="Age",]$Characteristic)[1]
			) ,
			
			
			
			#option to download data
			downloadButton(
			  outputId="download", 
			  label= "Download selected data (.csv)"
			),
			hr(),
			textOutput(outputId="finalfootnotes"),
			hr(),
			textOutput(outputId="finalfootnotes2")
		),

		#output
		mainPanel(
		  h3(textOutput(outputId= "title")),
		  plotlyOutput(outputId= "picture"),
		  textOutput(outputId= "footnote1"),
		  textOutput(outputId="footnote2"),
		  textOutput(outputId= "footnote3"),
			tableOutput(outputId="table")

		)
	)
)  

server<- function(input, output, session){
	subset<- reactive({
		req(input$metric, input$stub, input$subcat)
	  if(input$stub!=unique(a$Group)[6]) {
	    a[(a$Group == input$stub) & (a$Metric==input$metric) & (a$Characteristic %in% input$subcat), c(-4,-5)]	     
	  } else{
      a[(a$Group == input$dynamic) & (a$Metric==input$metric) & (a$Characteristic %in% input$subcat), c(-4,-5)]
	  }
	})
	
	output$title <- renderText({
	  req(input$stub, input$metric, subset()$Group, input$subcat)
	  if((footnotes2[subset()$Group[1]==unique(a$Group)])[1]!=" ") {fn<- "**"}
	  else {fn<-""}
	  
	  req(input$stub, input$metric, subset()$Group, input$subcat)
	  if(input$stub!=unique(a$Group)[6]) {
	    paste(c(input$metric, "* by ", input$stub, fn), collapse = "")   
	  } else{
	    paste(c(input$metric, "* by ", input$dynamic, fn), collapse = "")
	  }

	})

	output$table<- renderTable({
	  req(input$metric, input$stub, input$subcat)
		subset()
	})
	
	output$picture<- renderPlotly({
	  req(input$metric, input$stub, input$subcat, subset()$Group)
	  ggplotly(ggplot(subset(), aes(x= Year, y= Percent, color=Characteristic))+ geom_line()+scale_color_brewer(palette="Paired")+coord_cartesian(xlim = c(1997, 2015), ylim = c(0, max(subset()$Percent[!is.na(subset()$Percent)]))) )
	})
	
	footnotes<- c("* Based on persons responding to the questions, \"During the past 12 months was there any time when person needed medical care but did not get it because person couldn't afford it?\" and \"During the past 12 months has medical care been delayed because of worry about the cost?\"", 
	              "* Based on persons responding to the question, \"During the past 12 months was there any time when person needed prescription medicine but didn't get it because person couldn't afford it?\"", 
	              "* Based on persons responding to the question, \"During the past 12 months was there any time when person needed dental care (including checkups) but didn't get it because person couldn't afford it?\"")
	
	output$footnote1 <- renderText({
	  req(input$metric, subset())
	  footnotes[input$metric==unique(a$Metric)]  	    
	})
	
	footnotes2<- c("** Total includes all other races not shown separately, unknown health insurance status, unknown education level, and unknown disability status.", 
	               " ", 
	               "** The race groups, white, black, American Indian or Alaska Native, Asian, Native Hawaiian or Other Pacific Islander, and 2 or more races, include persons of Hispanic and non-Hispanic origin. Persons of Hispanic origin may be of any race. Starting with 1999 data, race-specific estimates are tabulated according to the 1997 Revisions to the Standards for the Classification of Federal Data on Race and Ethnicity and are not strictly comparable with estimates for earlier years. The five single-race categories plus multiple-race categories shown in the table conform to the 1997 Standards. Starting with 1999 data, race-specific estimates are for persons who reported only one racial group; the category 2 or more races includes persons who reported more than one racial group. Prior to 1999, data were tabulated according to the 1977 Standards with four racial groups, and the Asian only category included Native Hawaiian or Other Pacific Islander. Estimates for single-race categories prior to 1999 included persons who reported one race or, if they reported more than one race, identified one race as best representing their race. Starting with 2003 data, race responses of other race and unspecified multiple race were treated as missing, and then race was imputed if these were the only race responses. Almost all persons with a race response of other race were of Hispanic origin. See Appendix II, Hispanic origin; Race.", 
	               "** The race groups, white, black, American Indian or Alaska Native, Asian, Native Hawaiian or Other Pacific Islander, and 2 or more races, include persons of Hispanic and non-Hispanic origin. Persons of Hispanic origin may be of any race. Starting with 1999 data, race-specific estimates are tabulated according to the 1997 Revisions to the Standards for the Classification of Federal Data on Race and Ethnicity and are not strictly comparable with estimates for earlier years. The five single-race categories plus multiple-race categories shown in the table conform to the 1997 Standards. Starting with 1999 data, race-specific estimates are for persons who reported only one racial group; the category 2 or more races includes persons who reported more than one racial group. Prior to 1999, data were tabulated according to the 1977 Standards with four racial groups, and the Asian only category included Native Hawaiian or Other Pacific Islander. Estimates for single-race categories prior to 1999 included persons who reported one race or, if they reported more than one race, identified one race as best representing their race. Starting with 2003 data, race responses of other race and unspecified multiple race were treated as missing, and then race was imputed if these were the only race responses. Almost all persons with a race response of other race were of Hispanic origin. See Appendix II, Hispanic origin; Race.",
	               "** Estimates are for persons aged 25-64. GED is General Educational Development high school equivalency diploma. See Appendix II, Education.",
	               "** Percent of poverty level is based on family income and family size and composition using U.S. Census Bureau poverty thresholds. Missing family income data were imputed for 1997 and beyond. See Appendix II, Family income; Poverty; Table VI.",
	               "** The race groups, white, black, American Indian or Alaska Native, Asian, Native Hawaiian or Other Pacific Islander, and 2 or more races, include persons of Hispanic and non-Hispanic origin. Persons of Hispanic origin may be of any race. Starting with 1999 data, race-specific estimates are tabulated according to the 1997 Revisions to the Standards for the Classification of Federal Data on Race and Ethnicity and are not strictly comparable with estimates for earlier years. The five single-race categories plus multiple-race categories shown in the table conform to the 1997 Standards. Starting with 1999 data, race-specific estimates are for persons who reported only one racial group; the category 2 or more races includes persons who reported more than one racial group. Prior to 1999, data were tabulated according to the 1977 Standards with four racial groups, and the Asian only category included Native Hawaiian or Other Pacific Islander. Estimates for single-race categories prior to 1999 included persons who reported one race or, if they reported more than one race, identified one race as best representing their race. Starting with 2003 data, race responses of other race and unspecified multiple race were treated as missing, and then race was imputed if these were the only race responses. Almost all persons with a race response of other race were of Hispanic origin. See Appendix II, Hispanic origin; Race.",
                 "** For information on the health insurance categories, see Appendix II, Health insurance coverage.", 
                 "** For information on the health insurance categories, see Appendix II, Health insurance coverage.",
                 "** Percent of poverty level is based on family income and family size and composition using U.S. Census Bureau poverty thresholds. Missing family income data were imputed for 1997 and beyond. See Appendix II, Family income; Poverty; Table VI.",
                 "** Any basic actions difficulty or complex activity limitation is defined as having one or more of the following limitations or difficulties: movement difficulty, emotional difficulty, sensory (seeing or hearing) difficulty, cognitive difficulty, self-care (activities of daily living or instrumental activities of daily living) limitation, social limitation, or work limitation. For more information, see Appendix II, Basic actions difficulty; Complex activity limitation. Starting with 2007 data, the hearing question, a component of the basic actions difficulty measure, was revised. Consequently, data prior to 2007 are not comparable with data for 2007 and beyond. For more information on the impact of the revised hearing question, see Appendix II, Hearing trouble.",
                 " ",
                 "** MSA is metropolitan statistical area. Starting with 2006 data, MSA status is determined using 2000 census data and the 2000 standards for defining MSAs. For data prior to 2006, see Appendix II, Metropolitan statistical area (MSA) for the applicable standards."
	               )
	
	output$footnote2 <- renderText({
	  req(subset()$Group)
	  footnotes2[subset()$Group[1]==unique(a$Group)]  	    
	})

  footnotes3<-c(" ", " ", " ", " ", " ", " ", 
                 "Percent of poverty level is based on family income and family size and composition using U.S. Census Bureau poverty thresholds. Missing family income data were imputed for 1997 and beyond. See Appendix II, Family income; Poverty; Table VI.", 
                 " ", " ",
                 "For information on the health insurance categories, see Appendix II, Health insurance coverage.", 
                 " ", " ", " ")
  
  
  output$footnote3 <- renderText({
    req(subset()$Group)
    footnotes3[subset()$Group[1]==unique(a$Group)]  	    
  })
	
	output$download<- downloadHandler(
	  filename = function() {
	    paste(input$metric, " subset.csv", sep="")
	  },
	  content = function(file) {
	    write.csv(subset(), file, row.names=FALSE)
	  }
	)
	
	demomatching<- list(Demographic= c(1:4), Socioeconomic= c(5,6), Healthcare= c(8:11), Geographic= c(12,13))
	
	observeEvent(input$theme!= "Demographic" , {
	  updateSelectInput(session, "stub",
	     choices = unique(a$Group)[unlist(c(demomatching[names(demomatching)==input$theme]), use.names = F)],
	     selected =unique(a$Group)[unlist(c(demomatching[names(demomatching)==input$theme]), use.names = F)][1]
	  )
	})
	

	
	values <- reactiveValues()
	
	##START WORKING HERE: https://stackoverflow.com/questions/45060782/disable-a-selectinput-in-r-shiny-when-not-required
	
	output$dynamicInput <- renderUI({
	  
	  # This input exists if the `static`
	  # one is equal to `A` only
	  if (input$stub==unique(a$Group)[6]) {
	    selectInput(inputId = 'dynamic',
	                label = 'Select:',
	                choices = unique(a$Group)[c(6,7,10)],
	                selectize = FALSE)
	  } else {
	    return(NULL)
	  }
	  
	})
	
	## this bit fixes the issue
	observe({
	  if (input$stub==unique(a$Group)[6]) {
	    values$dyn <- input$dynamic
	  } else {
	    values$dyn <- NULL
	  }
	})
	
	observeEvent(input$stub!="Age" && !is.null(input$dynamic), {
	  if(input$stub!= unique(a$Group)[6]) {
	    updateCheckboxGroupInput(session, "subcat", 
	                             choices = unique(a[a$Group==input$stub,]$Characteristic),
	                             selected = unique(a[a$Group==input$stub,]$Characteristic)
	    )
	  } else {
	    updateCheckboxGroupInput(session, "subcat", 
	                             choices = unique(a[a$Group==input$dynamic,]$Characteristic),
	                             selected =unique(a[a$Group==input$dynamic,]$Characteristic)
	    )
	  } 
	})
	
	output$finalfootnotes<-renderText("NOTES: Standard errors and additional data years are available in the spreadsheet version of this table. Available from: http://www.cdc.gov/nchs/hus.htm. Data for additional years are available. See the Excel spreadsheet on the Health, United States website at: http://www.cdc.gov/nchs/hus.htm.")
  output$finalfootnotes2<- renderText("SOURCE: NCHS, National Health Interview Survey, family core, sample child, and sample adult questionnaires. See Appendix I, National Health Interview Survey (NHIS).")
}

shinyApp(ui=ui, server=server)

