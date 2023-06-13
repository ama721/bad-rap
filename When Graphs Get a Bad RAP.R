#### Final Chart From 'When Graphs Get a Bad R.A.P.' ####
#### Last Modified: 10.3.21 ####

#### PACKAGES + SET UP ####
# create notin operator
`%notin%` <- Negate(`%in%`)

# Check if the required packages are installed
# And if they are not installed, install them
pckgs <- c("plotly", "tidyverse","htmlwidgets")

sapply(pckgs, FUN = function(x){require(x, character.only = TRUE)})

#### IMPORT DATA ####
d <- read_csv("https://www.dropbox.com/s/qcpfxgi49v7fm7t/Bad%20RAP.csv?dl=1")

#### DATA MANIPULATION ####
# Rename and reorder Salary + Identity Categories
d <- 
d %>%
mutate(Salary = factor(Salary, levels = c(1,2,3), 
						  labels = c("$100,001 or more","$50,001 - $100,000","$0 - $50,000")),
			Group = factor(Group, levels = c(1,2,3,4), 
						  labels = c("Women of Color", "Men of Color", "White Women", "White Men"))) %>%
rename_with(., .fn = ~gsub("-",".",.))

# Create overlay
overlay <- data.frame(Sal =  c(1,2,3), Over =  c(1,1,1))

# Extract unique Group Names
c <-as.character(unique(d$Group))
# Extract the number of levels
N <- nlevels(factor(d$Group))
# Create plot_list to loop over
plot_list <- vector("list",N)

#### BEGIN PLOTTING ####
for(i in order(N:1,decreasing = F)){
	d_sub <- filter(d, Group %in% c[i])
 
## CHART LAYOUT AND FORMATTING
# Set font parameters for chart axes
axisfont<-list(family = "Montserrat", size = 12.5, color = "#A6A6A6")

# Set x and y axis layout 
xaxis <- list(title = "",
    range=c(-.65,1),
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE,
    autotick = FALSE,
    dtick=1,
    tickfont = axisfont,
    tickformat = "%",
    ticks = 'none',
    tickcolor = 'transparent',
    tickwidth = 0.5,
    ticklen = 5)

yaxis <- list(title = "",
    range=c(0,4),
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE,
    autotick = FALSE,
    tickfont = axisfont,
    dtick=1,
    ticks = 'none',
    tickcolor = 'transparent',
    tickwidth =  0.5,
    ticklen = 0)

# Set chart margins
margin <- list(autoexpand = TRUE,
	l = 120,
	r = 15,
	t = 90,
	b = 0,
	pad = 1)

# Label for identity category
title <-list(
	x = -0.05,
	y = 4, 
	xanchor = "left", 
	showarrow = F, 
	ax = 0, 
	ay = 0, 
	align = "left",
	text= paste0("<span style = 'font-weight: 600;'>",c[i],"</span>"),
	font = list(face="bold", size = 13,
	color=as.character(unique(d_sub$hex)),
	family="Montserrat"))
	
	chart_title <-list(
	x = 0.025,
	y = 0.8, 
	xanchor = "left", 
	showarrow = F, 
	ax = 0, 
	ay = 0, 
	align = "left",
	text= paste0("<span style = 'font-weight: 500'>This display shows the distribution of salaries in $US for nonprofit workers <br> with a master's degree or above, grouped by race and gender.</span>"),
	font = list(size = 20,
	color= "#3b3c3d",
	family="Montserrat"))
		
	
## PLOTTING CHART                     
p <- plot_ly(height = 400, width = 900)%>%
# add percentages
add_trace(data = d_sub,  x = ~ Percent,y = ~ order(y.lab), hoverinfo = "none", 
	type = "bar", orientation = "h", marker = list(color = ~ as.character(unique(hex)),
    line = list(color = ~ as.character(unique(hex))))) %>%
# add overlay
add_trace(data = overlay, x = ~ Over, y = ~ Sal, hoverinfo = "none", 
	type = "bar", orientation = "h", marker = list(color = '#f4f4f4',
    line = list(color = "#f4f4f4"))) %>%
# add data labels for percentages shown
add_trace(data = d_sub, type = "scatter", mode = "text", x = rep(-.225,times=3), y = 3:1,
	hoverinfo = "none",text = ~ paste(Percent*100,"%",sep=""), 
	textfont = list(family = "Montserrat", color = ~ as.character(unique(hex)), size = 12.5)) %>%
# add (identity) label
layout(title = chart_title, barmode = "stack", xaxis = xaxis, yaxis=yaxis, showlegend = FALSE, 
			autosize= FALSE, plot_bgcolor  = "rgba(0, 0, 0, 0)", paper_bgcolor = "rgba(0, 0, 0, 0)", 
			margin = margin, annotations = list(title)) 		

# add salary categories to first chart, then save it
# to plot_list
if(i == 1) {
plot_list[[i]] <- p	%>%
add_annotations(x = rep(-.5,times=3),
	y = 3:1,
	text = d_sub$Salary,
	showarrow = FALSE,
    ax = 0, 
    ay = 0, 
	xanchor = "right",
	align = "right",
	font = list(size = 12.5,color="#404040", family="Montserrat"))
}else{
# saves charts to plot_list (without additional annotations)
plot_list[[i]] <- p
}
}

# Render chart. Note: the chart will render locally in your web browser 
# (or viewer if you are using RStudio)
plot_final <- 
subplot(plot_list, nrows = 1,margin = 0, heights = c(.42), shareX = TRUE, shareY = TRUE) %>%
# do not show mode bar
config(displayModeBar = F) %>%
# change pointer
onRender("function(el, x) {Plotly.d3.select('.cursor-crosshair').style('cursor', 'pointer')}")	

print(plot_final)

#### EXPORT PLOT (if you have orca) ####
# orca(plot_final, file = "graphs get a bad rap.png", scale  = 10, width = 900, height = 500)
