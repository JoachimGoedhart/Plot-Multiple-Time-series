# Script that:
# Reads csv file with header, first column is time. Assumes that each of the other columns
# represents a single time-dependent measurement (e.g. the reponse of a single cell)
# Caluclates summary statistics per time-point (mean, sd, sem, 95%CI)
# Plots time dependent data: single time-dependent measurements, average and error (95%CI)

# Created by: 
# Joachim Goedhart, @joachimgoedhart, 2018

#Requires the packages tidyr, ggplot2, dplyr, magrittr

require(tidyr)
require(ggplot2)
require(dplyr)
require(magrittr)

######################## Set some parameters #########################
#Confidence level, typically 95% = 0.95
Conf_level <-  0.95

#Start and end point of stimulation
stim_start <- 1.8
stim_end <- 20

######################### import the data ##############################

#Read a text file (comma separated values)
df_wide1 <- read.csv("Fig3_Rho_S1P.csv", na.strings = "")
df_wide2 <- read.csv("Fig3_Rac_S1P.csv", na.strings = "")
df_wide3 <- read.csv("Fig3_Cdc42_S1P.csv", na.strings = "")

#Merge the dataframes, based on the header and giving each dataframe a unique id
df_all <- bind_rows(df_wide1, df_wide2, df_wide3, .id = "Condition")


#Tidy the data, i.e. long format with each row is variable
df_tidy <- gather(df_all, Cell, Ratio, -Time, -Condition)

#Give each sample a unique id
df_tidy <- unite(df_tidy, unique_id, c(Condition, Cell), sep = "_", remove = FALSE)


######### Calulcate summary statistics to fill dataframe 'df_summary' ########

df_tidy_mean <- df_tidy %>%
				filter(!is.na(Ratio)) %>%
				group_by(Time, Condition) %>%
				summarise(mean = mean(Ratio, na.rm = TRUE),
						sd = sd(Ratio, na.rm = TRUE),
						n = n()) %>%
				mutate(sem = sd / sqrt(n - 1),
						CI_lower = mean + qt((1-Conf_level)/2, n - 1) * sem,
						CI_upper = mean - qt((1-Conf_level)/2, n - 1) * sem)


####################################################

#### Command to prepare the plot ####
	ggplot(data= df_tidy_mean, aes(x=Time, y=mean)) +

#### plot individual measurements ####
	geom_line(data=df_tidy, aes(x=Time, y=Ratio, group=unique_id, color=Condition), alpha=0.3) +

#### plot average response over time ####
	geom_line(data= df_tidy_mean, aes(x=Time, y=mean, color=Condition), size=1, alpha=1)+


#### plot error (95%CI) of the resnse over time ####
	geom_ribbon(data= df_tidy_mean, aes(ymin=CI_lower, ymax=CI_upper, fill=Condition), color="grey70", alpha=0.4)+

#### plot s.e.m. of the average response over time ####
	# geom_ribbon(data= df_tidy_mean, aes(ymin=mean-sem, ymax=mean+sem) ,fill="green", alpha=0.5)+

#### Draw a filled, transparant rectangle to indicte when stimulation takes place
#	annotate("rect", xmin=stim_start, xmax=stim_end, ymin=1.22, ymax=1.24, alpha=0.4, fill="black")+

#### Draw a verticle line to indicte when stimulation takes place
	geom_vline(xintercept = stim_start) +

## Set the Y-axis scale, remove for autoscale

  coord_cartesian(ylim = c(0.94, 1.24)) + coord_cartesian(xlim = c(0.0, 10)) +
  
## Set theme&basic font
	theme_light(base_size = 16) +


  
### set the title
	ggtitle("Timeseries; average & 95%CI") +
    
### Style the axis (font size)
	# theme(axis.text.x = element_text(size=16, angle=0, vjust = 0), axis.text.y = element_text(size=16)) +

### Set layout of the graph 
	# theme(panel.border = element_rect(size = 1, linetype = "solid", colour = "black", fill=NA)) +

### Set label of x- and y-axis
	ylab("Ratio YFP/CFP [-]") + xlab("Time [s]") +
  
### Set aspect ratio of the graph n/n = square
	theme(aspect.ratio=4/4)
  


