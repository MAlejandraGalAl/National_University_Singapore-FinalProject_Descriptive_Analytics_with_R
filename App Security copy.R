
setwd("C:\\Users\\alega\\OneDrive\\Documentos\\ProjecttR")


#Libraries
library(shiny)
library(shinyjs)
library(shinythemes)
library(wordcloud)
library(readxl)  
library(RColorBrewer)
library(plotly)
library(dplyr)
library(ggplot2)
library(ggridges)
library(gridExtra)
library(leaflet)
library(tm)

#WORDCLOUD
data_cloud= read_excel("Industry.xlsx") 

plot_industry_wordcloud <- function(industryChoice) {
  # Filtrar los datos por la industria seleccionada
  industry_data <- subset(data_cloud, Industry == industryChoice)
  
  if (nrow(industry_data) > 0) {
    # Determinar el número de colores en función de las palabras únicas
    num_colors <- length(unique(industry_data$Carrer))
    
    # Crear una paleta de colores en tonos de azul
    blue_palette <- colorRampPalette(c("lightblue", "blue", "darkblue"))
    colors <- blue_palette(num_colors)
    
    # Configuración de los márgenes de la gráfica
    par(mai = c(0.1, 0.1, 0.1, 0.1))
    
    # Generar la nube de palabras
    wordcloud(words = industry_data$Carrer, 
              freq = industry_data$Frecuencia,
              scale = c(4, 0.5),  
              min.freq = 1,
              max.words = 200,
              random.order = FALSE,
              rot.per = 0,  # sin rotación
              colors = colors,
              family = "sans",  
              fontface = "bold")
  } else {
    print("No data found for the selected industry.")
  }
}



#GRAPHICS

ai <- read.csv('ai_FINAL.csv')

#DATA MANIPULATION 

set.seed(1)

# Data frame with GDP and GDP_Growth

# Helps to asses whether the countries with higher level of resources and growth are highly exposed to AI adoption. 
df <- data.frame(
  City = c("Dubai", "Singapore", "Berlin", "Tokyo", "San Francisco", "London", "Paris", "Sydney", "New York", "Toronto"),
  GDP_2023_Billion_USD = c(499.2, 423.6, 4467, 4371, 26206, 3250, 3190, 1580, 26206, 2342),
  GDP_Growth_2023_Percent = c(3.4, 3.0, 0.2, 1.3, 2.1, 0.5, 0.7, 1.5, 2.1, 1.2)
)
ai <- merge(ai,df, by.x = 'Location', by.y = 'City')



# Automation_Impact_Score

# This column will provide a calculated likelihood of automation. This will help assessing how susceptible different jobs and industries are to being replaced

ai <- ai %>%
  mutate(Automation_Impact_Score = case_when(
    AI_Adoption_Level == "High" & Automation_Risk == "High" ~ 10,
    AI_Adoption_Level == "High" & Automation_Risk == "Medium" ~ 8,
    AI_Adoption_Level == "High" & Automation_Risk == "Low" ~ 6,
    AI_Adoption_Level == "Medium" & Automation_Risk == "High" ~ 8,
    AI_Adoption_Level == "Medium" & Automation_Risk == "Medium" ~ 6,
    AI_Adoption_Level == "Medium" & Automation_Risk == "Low" ~ 4,
    AI_Adoption_Level == "Low" & Automation_Risk == "High" ~ 6,
    AI_Adoption_Level == "Low" & Automation_Risk == "Medium" ~ 4,
    TRUE ~ 2
  ))




# Group by industry and Summarize Average Salary

# This provides isnights into the avrege compenstation within the occupation. Might be useful fro comparison among different fields

# Summarize average salary by Industry
average_salary_by_industry <- ai %>%
  group_by(Industry, Job_Title) %>%
  summarize(Average_Salary = mean(Salary_USD, na.rm = TRUE))

ai <- ai %>%
  left_join(average_salary_by_industry, by = c("Industry", 'Job_Title'))



#Compensation Including Stock Options and Bonuses

# Assumption -> Stock_options are worth 10% of the salary and Bonus might reach up to 15% of the basic salary
# Create stock options and bonus percentages

ai <- ai %>% # Industries that are mopre likely to be compensated with stock options
  mutate(Stock_Option_Percent = case_when(
    Industry %in% c("Technology", "Finance", "Telecommunications") ~ 0.20,
    Industry %in% c("Manufacturing", "Healthcare", "Energy") ~ 0.10,
    Industry %in% c("Retail", "Education", "Entertainment") ~ 0.05,
    TRUE ~ 0.05 # Default to 5% if not specified
  ),
  Bonus_Percent = case_when(
    Industry %in% c("Technology", "Finance", "Telecommunications") ~ 0.20,
    Industry %in% c("Manufacturing", "Healthcare", "Energy") ~ 0.15,
    Industry %in% c("Retail", "Education", "Entertainment") ~ 0.10,
    TRUE ~ 0.1 # Default to 10% if not specified
  ))
ai <- ai %>%
  mutate(Stock_Options = Salary_USD * Stock_Option_Percent,
         Bonus = Salary_USD * Bonus_Percent,
         Total_Compensation = Salary_USD + Stock_Options + Bonus)



# Automation Probability
# This provides a way to quantify automation risk and compare industries or job titles.
# It can be used to evaluate which sectors may face immediate automation threats.

ai <- ai %>%
  mutate(Automation_Probability = case_when(
    AI_Adoption_Level == "High" & Automation_Risk == "High" ~ runif(1, 80, 100),     # High AI Adoption & High Risk, high probability
    AI_Adoption_Level == "High" & Automation_Risk == "Medium" ~ runif(1, 60, 80),   # High AI Adoption & Medium Risk, medium-high probability
    AI_Adoption_Level == "High" & Automation_Risk == "Low" ~ runif(1, 40, 60),      # High AI Adoption & Low Risk, medium probability
    AI_Adoption_Level == "Medium" & Automation_Risk == "High" ~ runif(1, 60, 80),   # Medium AI Adoption & High Risk, medium-high probability
    AI_Adoption_Level == "Medium" & Automation_Risk == "Medium" ~ runif(1, 40, 60), # Medium AI Adoption & Medium Risk, medium probability
    AI_Adoption_Level == "Medium" & Automation_Risk == "Low" ~ runif(1, 20, 40),    # Medium AI Adoption & Low Risk, medium-low probability
    AI_Adoption_Level == "Low" & Automation_Risk == "High" ~ runif(1, 40, 60),      # Low AI Adoption & High Risk, medium probability
    AI_Adoption_Level == "Low" & Automation_Risk == "Medium" ~ runif(1, 20, 40),    # Low AI Adoption & Medium Risk, medium-low probability
    TRUE ~ runif(1, 0.0, 20)                                                        # Default case, low probability
  ))

# AI adoption level vs Company Size
# Shows how common each AI Adoption Level is for each company size.
# For instance, it tells us if large companies tend to adopt AI at higher levels than small companies.

# Create a new column that counts the occurrences of AI Adoption Level per Company Size
ai <- ai %>%
  group_by(Company_Size, AI_Adoption_Level) %>% 
  mutate(count = n()) %>%  # Add the count of companies in each group
  group_by(Company_Size) %>%  
  mutate(AI_Adoption_Count = sum(count),  # Total companies per size
         AI_Adoption_Rate_Per_Company_Size = (count / sum(count))*100) %>%  # Adoption rate per size
  ungroup() 


# Job Security Index

#This index gives an idea of how secure a job might be based on its automation risk and the growth projection for the role.

ai <- ai %>%
  mutate(Job_Security_Index = case_when(
    Automation_Risk == "Low" & Job_Growth_Projection == "Growth" ~ "High",
    Automation_Risk == "High" & Job_Growth_Projection == "Decline" ~ "Low",
    TRUE ~ "Medium"
  ))


# add most common degrees
common_degrees <- c(
  "Product Manager" = "Business Administration, Marketing, Engineering",
  "Sales Manager" = "Business Administration, Marketing, Economics",
  "Cybersecurity Analyst" = "Computer Science, Information Technology, Cybersecurity",
  "HR Manager" = "Human Resource Management, Business Administration, Psychology",
  "Data Scientist" = "Computer Science, Statistics, Mathematics",
  "UX Designer" = "Graphic Design, Human-Computer Interaction, Psychology",
  "Marketing Specialist" = "Marketing, Business Administration, Communications",
  "Operations Manager" = "Business Administration, Operations Management, Industrial Engineering",
  "Software Engineer" = "Computer Science, Software Engineering, Information Technology",
  "AI Researcher" = "Computer Science, Artificial Intelligence, Mathematics"
)

# Common degrees in certain industries
ai <- ai %>% 
  mutate(Common_Degrees = common_degrees[Job_Title])



# JOB GROWTH PROJECTION
get_base_growth_rate <- function(job_growth) {
  if (job_growth == "Growth") {
    return(c(4, 5))  # 4-5%
  } else if (job_growth == "Stable") {
    return(c(2, 3))  # 2-3%
  } else if (job_growth == "Decline") {
    return(c(-2, 1))  # -2-1%
  }
}

# Calculate only the necessary columns with percentages
ai <- ai %>%
  rowwise() %>%
  mutate(
    # Base Growth Rate
    Base_Growth_Rate_Low = get_base_growth_rate(Job_Growth_Projection)[1],
    Base_Growth_Rate_High = get_base_growth_rate(Job_Growth_Projection)[2],
    
    # GDP Growth Factor
    GDP_Growth_Factor = GDP_Growth_2023_Percent.x / 10,
    
    # Automation Impact Factor (directly using Automation_Probability as percentage)
    Automation_Impact_Factor_Low = Automation_Probability * 1.01 / 100,  # 1% impact
    Automation_Impact_Factor_High = Automation_Probability * 1.02 / 100,  # 2% impact
    
    # Final Annual Growth Rate (low, high, and mean) in percentage
    Annual_Growth_Rate_Low = Base_Growth_Rate_Low + GDP_Growth_Factor - Automation_Impact_Factor_Low,
    Annual_Growth_Rate_High = Base_Growth_Rate_High + GDP_Growth_Factor - Automation_Impact_Factor_High,
    Annual_Growth_Rate_Mean = (Annual_Growth_Rate_Low + Annual_Growth_Rate_High) / 2,
    
    # Projected Total Compensation in 5 Years (low, high, and mean)
    Projected_Total_Compensation_Low_5Y = Total_Compensation * (1 + Annual_Growth_Rate_Low / 100)^5,  # Convert percentage to decimal for calculation
    Projected_Total_Compensation_High_5Y = Total_Compensation * (1 + Annual_Growth_Rate_High / 100)^5,
    Projected_Total_Compensation_Mean_5Y = (Projected_Total_Compensation_Low_5Y + Projected_Total_Compensation_High_5Y) / 2
  ) %>%
  select(-Base_Growth_Rate_Low, -Base_Growth_Rate_High, -GDP_Growth_Factor, -Automation_Impact_Factor_Low, -Automation_Impact_Factor_High)  # Remove intermediate calculations


# Add the new column "Type of Automation" based on the industry
ai <- ai %>%
  mutate(Type_of_Automation = case_when(
    Industry %in% c("Manufacturing", "Transportation", "Construction") ~ "Physical",
    Industry %in% c("Finance", "Education", "Healthcare") ~ "Cognitive",
    Industry %in% c("Information Technology", "Telecommunications") ~ "Hybrid",
    TRUE ~ "Unknown"  # For industries not clearly fitting into a category or unspecified
  ))








#DATA PROCESING 
unique(ai$Location)
ai$Location <- as.factor(ai$Location)
unique(ai$Job_Title) #Alright
ai$Job_Title <- as.factor(ai$Job_Title)
unique(ai$Industry) # Alright
ai$Industry <- as.factor(ai$Industry)
unique(ai$Company_Size) # Alright
ai$Company_Size <- as.factor(ai$Company_Size)
unique(ai$AI_Adoption_Level) #Alright
ai$AI_Adoption_Level <- as.factor(ai$AI_Adoption_Level)
unique(ai$Automation_Risk) #Alright
ai$Automation_Risk <- as.factor(ai$Automation_Risk)
unique(ai$Required_Skills) #Alright
ai$Required_Skills <- as.factor(ai$Required_Skills)
unique(ai$Remote_Friendly) #Alright
ai$Remote_Friendly <- as.factor(ai$Remote_Friendly)
unique(ai$Job_Growth_Projection) #Alright
ai$Job_Growth_Projection <-as.factor(ai$Job_Growth_Projection)
unique(ai$Job_Security_Index) #Alright
ai$Job_Security_Index <- as.factor(ai$Job_Security_Index)
unique(ai$Common_Degrees) #Alright
ai$Common_Degrees <- as.factor(ai$Common_Degrees)
unique(ai$Type_of_Automation) #Alright
ai$Type_of_Automation <-as.factor(ai$Type_of_Automation)




#FUNCTIONS 


# Function to plot a bar chart with a slight gradient for Salary
plot_salary_by_industry <- function(location) {
  
  # Step 1: Filter the data by location
  filtered_data <- ai %>% filter(Location == location)
  
  # Step 2: Ensure the second variable (Salary_USD) is numeric (if it is stored as a factor or character)
  if (!is.numeric(filtered_data$Salary_USD)) {
    filtered_data$Salary_USD <- as.numeric(as.character(filtered_data$Salary_USD))
  }
  
  # Step 3: Group by the first variable (Industry) and calculate the average of the second variable (Salary_USD)
  grouped_data <- filtered_data %>%
    group_by(Industry) %>%
    summarise(average_value = mean(Salary_USD, na.rm = TRUE)) %>%
    arrange(desc(average_value))  # Sort the data in descending order by the average value
  
  # Step 4: Plot the bar chart with a gradient fill based on Salary
  p <- ggplot(grouped_data, aes(x = reorder(Industry, -average_value), y = average_value, fill = average_value)) +
    geom_bar(stat = "identity", color = "black") +
    geom_text(aes(label = round(average_value, 2)), vjust = -0.7, size = 2) +
    labs(title = paste("Average Salary by Industry in", location),
         x = "Industry", y = "Average Salary (USD)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_gradient(low = "peru", high = "chocolate4", guide = FALSE)  # Subtle gradient
  
  return(p)
}


# Function with only data and location as inputs, with hardcoded first_var and second_var
plot_industry_automation_impact_score <- function(location) {
  
  # Hardcoding the first and second variables
  first_var <- "Industry"
  second_var <- "Automation_Impact_Score"
  
  # Step 1: Filter the data by location
  filtered_data <- ai %>% filter(Location == location)
  
  # Step 2: Ensure the second variable (Automation_Risk) is numeric (if stored as a factor or character)
  if (!is.numeric(filtered_data[[second_var]])) {
    filtered_data[[second_var]] <- as.numeric(as.character(filtered_data[[second_var]]))
  }
  
  # Step 3: Group by the first variable (Industry) and calculate the average of the second variable (Automation_Risk)
  grouped_data <- filtered_data %>%
    group_by(Industry) %>%
    summarise(average_value = mean(Automation_Impact_Score, na.rm = TRUE)) %>%
    arrange(desc(average_value))  # Sort the data in descending order by the average value
  
  # Step 4: Plot the bar chart with average values
  p <- ggplot(grouped_data, aes(x = Industry, y = average_value)) +
    geom_bar(stat = "identity", fill = "skyblue", color = "black") +
    geom_text(aes(label = round(average_value, 2)), vjust = -0.9, size = 2.5) +
    labs(title = paste("Average Automation Risk by Industry in", location),
         x = "Industry", y = "Average Automation Risk") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Show the plot
  return(p)
}


# Function to compare job growth projection across industries with interactive plot
plot_job_growth_by_industry <- function(location) {
  
  # Step 1: Filter the data by location
  filtered_data <- ai %>% filter(Location == location)
  
  # Step 2: Group by industry and job growth projection and count occurrences
  grouped_data <- filtered_data %>%
    group_by(Industry, Job_Growth_Projection) %>%
    summarise(count = n()) %>%
    ungroup()
  #Set Colors
  growth_colors <- c("Growth" = "#66FF66", "Decline" = "#FF6666", "Stable" = "#FFCC66")
  
  # Step 3: Plot the bar chart
  p <- ggplot(grouped_data, aes(x = Industry, y = count, fill = Job_Growth_Projection)) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    labs(title = paste("Job Growth Projection by Industry in", location),
         x = "Industry", y = "Count of Job Growth Projections") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = growth_colors)  # Use a color palette for different job growth projections
  
  return(p)
}


# Function to plot Automation Probability for all industries, with hardcoded inputs
plot_automation_probability_by_industry <- function(location) {
  
  # Hardcoded variables
  industry_column <- "Industry"
  automation_column <- "Automation_Probability"
  
  # Step 1: Filter the data by location
  filtered_data <- ai %>% filter(Location == location)
  
  # Step 2: Group by industry and calculate average automation probability for each industry
  grouped_data <- filtered_data %>%
    group_by(.data[[industry_column]]) %>%
    summarise(average_automation_prob = mean(.data[[automation_column]], na.rm = TRUE)) %>%
    arrange(desc(average_automation_prob))  # Sort industries by automation probability
  
  # Step 3: Create a bar plot to show average automation probability by industry
  p <- ggplot(grouped_data, aes(x = reorder(Industry, -average_automation_prob), y = average_automation_prob, fill = average_automation_prob)) +
    geom_bar(stat = "identity", color = "black") +  # Remove fixed fill color and make it dynamic
    geom_text(aes(label = round(average_automation_prob, 2)), vjust = -0.7, size = 2) +  # Show average values
    labs(title = paste("Average Automation Probability by Industry in", location),
         x = "Industry", y = "Average Automation Probability (%)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_gradient(low = "green3", high = "red", name = "Automation Probability (%)")  # Apply gradient from green to red
  
  # Show the plot
  return(p)
  
}


# Function to plot AI Adoption Rate vs Company Size for a selected location
plot_ai_adoption_by_company_size <- function(location) {
  
  # Filter data based on the selected location
  filtered_data <- ai %>% filter(Location == location)
  
  # Ensure AI_Adoption_Rate_Per_Company_Size is numeric
  filtered_data$AI_Adoption_Rate_Per_Company_Size <- as.numeric(filtered_data$AI_Adoption_Rate_Per_Company_Size)
  
  # Create the plot with Company Size on x-axis and AI Adoption Rate on y-axis
  ggplot(filtered_data, aes(x = Company_Size, y = AI_Adoption_Rate_Per_Company_Size, fill = Company_Size)) +
    geom_bar(stat = "identity") +
    labs(title = paste("AI Adoption Rate vs Company Size in", location),
         x = "Company Size", y = "AI Adoption Rate (%)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_brewer(palette = "Set2")  # You can customize the color palette
}


# Function to plot skills vs automation risk as percentage for a selected location
plot_skills_vs_risk_percent <- function(location) {
  
  # Filter data based on the selected location
  filtered_data <- ai %>% filter(Location == location)
  
  # Calculate percentage of jobs for each skill and automation risk
  percent_data <- filtered_data %>%
    group_by(Required_Skills, Automation_Risk) %>%
    summarise(Job_Count = n()) %>%
    mutate(Percent = Job_Count / sum(Job_Count) * 100)
  
  # Create the plot with Skills (Required_Skills) on x-axis and Percentage on y-axis
  ggplot(percent_data, aes(x = Required_Skills, y = Percent, fill = Automation_Risk)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("Skills vs Automation Risk (Percentage) for", location),
         x = "Skills", y = "Percentage of Jobs") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_brewer(palette = "Set1") # You can customize the color palette
}

plot_skills_vs_risk_percent('Singapore')

# Function to plot proportion of remote-friendly jobs by industry (location as the only input)
plot_remote_friendly_by_industry <- function(location) {
  
  # Step 1: Filter data by location
  filtered_data <- ai %>% filter(Location == location)
  
  # Step 2: Create stacked bar plot (with industry and remote friendliness hardcoded)
  p <- ggplot(filtered_data, aes(x = Industry, fill = Remote_Friendly)) +
    geom_bar(position = "fill", color = "black") +
    labs(title = paste("Proportion of Remote-Friendly Jobs by Industry in", location),
         x = "Industry", y = "Proportion of Jobs") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_brewer(palette = "Set2")
  
  # Show the plot
  return(p)
}



# Function to plot job security index by industry
plot_job_security_by_industry <- function(location) {
  
  # Step 1: Filter data by location
  filtered_data <- ai %>% filter(Location == location)
  
  # Step 2: Create stacked bar plot
  p <- ggplot(filtered_data, aes(x = Industry, fill = Job_Security_Index)) +
    geom_bar(position = "fill", color = "black") +
    labs(title = paste("Job Security Index by Industry in", location),
         x = "Industry", y = "Proportion of Jobs by Security Level") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_brewer(palette = "Set3")
  
  # Show the plot
  return(p)
}



plot_interactive_automation_probability_by_industry <- function(location) {
  
  # Step 1: Filter data by the given location
  filtered_data <- ai %>% filter(Location == location)
  
  # Step 2: Create a ggplot object
  p <- ggplot(filtered_data, aes(x = Automation_Probability, color = Industry, fill = Industry)) +
    geom_density(alpha = 0.5) +
    labs(title = paste("Automation Probability by Industry in", location),
         x = "Automation Probability (%)", y = "Density") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Step 3: Convert ggplot to plotly
  interactive_plot <- ggplotly(p)
  
  #Step 4: Set all traces (industries) to be hidden by default
  for (i in seq_along(interactive_plot$x$data)) {
    interactive_plot$x$data[[i]]$visible <- "legendonly"  # Set all industries to be hidden by default
  }
  return(interactive_plot)
}



# Function to create an interactive box plot for automation probability by industry
plot_interactive_automation_boxplot <- function(location) {
  
  # Step 1: Filter data by the given location
  filtered_data <- ai %>% filter(Location == location)
  
  # Step 2: Create the ggplot box plot
  p <- ggplot(filtered_data, aes(x = Industry, y = Automation_Probability, color = Industry)) +
    geom_boxplot() +
    labs(title = paste("Automation Probability by Industry in", location),
         x = "Industry", y = "Automation Probability (%)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Step 3: Convert ggplot to an interactive plotly plot and return it
  interactive_plot <- ggplotly(p)
  
  return(interactive_plot)
}



# Function to plot ridgeline plot of automation probability by industry
plot_ridgeline_automation <- function(location) {
  filtered_data <- ai %>% filter(Location == location)
  
  ggplot(filtered_data, aes(x = Automation_Probability, y = Industry, fill = Industry)) +
    geom_density_ridges(alpha = 0.8, scale = 2) +
    labs(title = paste("Automation Probability Distribution by Industry in", location),
         x = "Automation Probability", y = "Industry") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set3")
}



# Function to visualize the percentage change in total compensation per industry
plot_compensation_change_by_industry <- function(location) {
  
  # Filter the data by location
  data_location <- ai %>% filter(Location == location)
  
  # Ensure necessary columns are numeric
  data_location$Total_Compensation <- as.numeric(data_location$Total_Compensation)
  data_location$Projected_Total_Compensation_Low_5Y <- as.numeric(data_location$Projected_Total_Compensation_Low_5Y)
  data_location$Projected_Total_Compensation_High_5Y <- as.numeric(data_location$Projected_Total_Compensation_High_5Y)
  data_location$Projected_Total_Compensation_Mean_5Y <- as.numeric(data_location$Projected_Total_Compensation_Mean_5Y)
  
  # Calculate the percentage change in total compensation per industry
  compensation_change <- data_location %>%
    group_by(Industry) %>%
    summarise(Current_Compensation = mean(Total_Compensation, na.rm = TRUE),
              Projected_Compensation = mean(Projected_Total_Compensation_Mean_5Y, na.rm = TRUE)) %>%
    mutate(Percent_Change = ((Projected_Compensation - Current_Compensation) / Current_Compensation) * 100)
  
  # Create the plot with Industry on x-axis and Percentage Change on y-axis
  plot <- ggplot(compensation_change, aes(x = reorder(Industry, Percent_Change), y = Percent_Change, fill = Percent_Change)) +
    geom_bar(stat = "identity") +
    scale_fill_gradient(low = "red", high = "green3") +
    labs(title = paste("Percentage Change in Total Compensation per Industry in", location),
         x = "Industry", y = "Percentage Change in Compensation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_flip() # Flip the axes for better readability
  
  
  return(plot)
}


# Function to create an interactive Automation Risk vs Industry plot
plot_automation_risk_by_industry <- function(location) {
  
  # Filter the data based on the selected location
  filtered_data <- ai %>%
    filter(Location == location) %>%
    filter(!is.na(Automation_Risk))  # Remove NA values in Automation Risk
  
  # Create the base ggplot with Industry on x-axis and count of Automation Risk categories on y-axis
  p <- ggplot(filtered_data, aes(x = Industry, fill = Automation_Risk, text = paste("Industry:", Industry, "<br>Automation Risk:", Automation_Risk))) +
    geom_bar(position = "dodge", color = "black") +  # Add black borders for distinction
    labs(title = paste("Automation Risk vs Industry in", location),
         x = "Industry", y = "Count of Jobs by Automation Risk") +
    theme_minimal() +  # Apply a minimal theme
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "black"), 
          axis.title.x = element_text(size = 12), 
          axis.title.y = element_text(size = 12)) +
    scale_fill_manual(values = c("High" = "#FF6666", "Medium" = "#FFCC66", "Low" = "#66FF66")) +  # Custom colors for Automation Risk
    coord_flip()  # Flip the axes for better readability
  
  # Convert the ggplot object to an interactive plotly plot
  interactive_plot <- ggplotly(p, tooltip = "text") %>% 
    layout(title = list(text = paste("Automation Risk vs Industry in", location),
                        font = list(size = 18)),
           hoverlabel = list(font = list(size = 12)),
           xaxis = list(tickfont = list(size = 10)),
           yaxis = list(tickfont = list(size = 10)),
           legend = list(title = list(text = "Automation Risk Levels")))
  
  # Display the interactive plot
  return(interactive_plot)
}


plot_skills_wordcloud <- function(location) {
  filtered_data <- ai[ai$Location == location, ]
  skill_text <- paste(filtered_data$Required_Skills, collapse = " ")
  skill_corpus <- Corpus(VectorSource(skill_text))
  skill_corpus <- tm_map(skill_corpus, content_transformer(tolower))
  skill_corpus <- tm_map(skill_corpus, removePunctuation)
  skill_corpus <- tm_map(skill_corpus, removeWords, stopwords("english"))
  par(mai = c(0.1, 0.1, 0.1, 0.1))
  wordcloud(skill_corpus, max.words = 100, random.order = FALSE)
}












#UI
ui <- navbarPage(
  theme = shinytheme("flatly"),
  title = "App",
  tabPanel("Welcome",
           fluidPage(
             useShinyjs(),
             titlePanel(" "),
             uiOutput("welcomeContent1"),  # WELCOME PART 1
             br(),
             leafletOutput("cityMap"),
             titlePanel(" "),
             uiOutput("welcomeContent2"),  # WELCOME PART 2
             br(),
             #wordcloud
             
             fluidRow(
               column(3, selectInput("industrySelected", "Select an industry:", choices = unique(data_cloud$Industry))),
               column(9, plotOutput("wordcloud"))
             )
             
           )),
  tabPanel("Industry Impact", includeHTML("s2.html"),
           tabsetPanel(
             tabPanel("Automation impact", 
                      fluidRow(
                        column(3, selectInput("location1", "Select a location:", choices = unique(ai$Location))),
                        column(9, plotOutput("plot1"))
                      )
             ),
             tabPanel("Automation risk", 
                      fluidRow(
                        column(3, selectInput("location2", "Select a location:", choices = unique(ai$Location))),
                        column(9, plotlyOutput("plot2"))
                      )
             ),
             tabPanel("Salary", 
                      fluidRow(
                        column(3, selectInput("location3", "Select a location:", choices = unique(ai$Location))),
                        column(9, plotOutput("plot3"))
                      )
             ),
             tabPanel("Job security", 
                      fluidRow(
                        column(3, selectInput("location4", "Select a location:", choices = unique(ai$Location))),
                        column(9, plotOutput("plot4"))
                      )
             ),
             tabPanel("Compensation change", 
                      fluidRow(
                        column(3, selectInput("location5", "Select a location:", choices = unique(ai$Location))),
                        column(9, plotOutput("plot5"))
                      )
             )
             
             
           )
           ),
  tabPanel("Automation Risk and Probability", includeHTML("s3.html"),
           tabsetPanel(
             tabPanel("Automation probability", 
                      fluidRow(
                        column(3, selectInput("location6", "Select a location:", choices = unique(ai$Location))),
                        column(9, plotOutput("plot6"))
                      )
             ),
             tabPanel("Interactive automation probability", 
                      fluidRow(
                        column(3, selectInput("location7", "Select a location:", choices = unique(ai$Location))),
                        column(9, plotlyOutput("plot7"))
                      )
             ),
             tabPanel("Interactive automation", 
                      fluidRow(
                        column(3, selectInput("location8", "Select a location:", choices = unique(ai$Location))),
                        column(9, plotlyOutput("plot8"))
                      )
             ),
             tabPanel("Ridgeline automation", 
                      fluidRow(
                        column(3, selectInput("location9", "Select a location:", choices = unique(ai$Location))),
                        column(9, plotOutput("plot9"))
                      )
             )
             
           )
           ),
  tabPanel("Job Market Trends", includeHTML("s4.html"),
           tabsetPanel(
             tabPanel("Job growth", 
                      fluidRow(
                        column(3, selectInput("location10", "Select a location:", choices = unique(ai$Location))),
                        column(9, plotOutput("plot10"))
                      )
             ),
             tabPanel("Skills vs Risk", 
                      fluidRow(
                        column(3, selectInput("location11", "Select a location:", choices = unique(ai$Location))),
                        column(9, plotlyOutput("plot11"))
                      )
             )
             
           )
           ),
  tabPanel("Workplace Dynamics", includeHTML("s5.html"),
           tabsetPanel(
             tabPanel("AI adoption by company size", 
                      fluidRow(
                        column(3, selectInput("location12", "Select a location:", choices = unique(ai$Location))),
                        column(9, plotOutput("plot12"))
                      )
             ),
             tabPanel("Remote friendly", 
                      fluidRow(
                        column(3, selectInput("location13", "Select a location:", choices = unique(ai$Location))),
                        column(9, plotOutput("plot13"))
                      )
             ),
             tabPanel("Skills", 
                      fluidRow(
                        column(3, selectInput("location14", "Select a location:", choices = unique(ai$Location))),
                        column(9, plotOutput("plot14"))
                      )
             )
             
           )
           ),
  tabPanel("Advice and resources", includeHTML("s6.html")),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css")
  )
)





#SERVER
server <- function(input, output) {
  #WELCOME PART 1
  output$welcomeContent1 <- renderUI({
    HTML(paste(readLines("welcome1.html"), collapse = "\n"))
  })
  
  #WELCOME PART 2
  output$welcomeContent2 <- renderUI({
    HTML(paste(readLines("welcome2.html"), collapse = "\n"))
  })
  
  
  #MAP
  output$cityMap <- renderLeaflet({
    iconos <- list(
      berlin = makeIcon(iconUrl = "berlin.jpeg", iconWidth = 40, iconHeight = 40),
      newYork = makeIcon(iconUrl = "newYork.jpeg", iconWidth = 40, iconHeight = 40),
      dubai = makeIcon(iconUrl = "dubai.jpg", iconWidth = 40, iconHeight = 40),
      singapore = makeIcon(iconUrl = "singapore.avif", iconWidth = 40, iconHeight = 40),
      paris = makeIcon(iconUrl = "paris.jpg", iconWidth = 40, iconHeight = 40),
      sanF = makeIcon(iconUrl ="sanfran.jpg", iconWidth = 40, iconHeight = 40),
      sydney = makeIcon(iconUrl = "sydney.webp", iconWidth = 40, iconHeight = 40),
      tokyo = makeIcon(iconUrl = "tokyo.webp", iconWidth = 40, iconHeight = 40),
      toronto = makeIcon(iconUrl = "toronto.avif", iconWidth = 40, iconHeight = 40),
      london = makeIcon(iconUrl = "london.jpg", iconWidth = 40, iconHeight = 40)
    )
    
    # Create the map
    leaflet() %>%
      addTiles() %>%
      addMarkers(lng=13.4050, lat=52.5200, icon=iconos$berlin, popup="Berlin") %>%
      addMarkers(lng=-74.0060, lat=40.7128, icon=iconos$newYork, popup="New York") %>%
      addMarkers(lng=55.2708, lat=25.2048, icon=iconos$dubai, popup="Dubai") %>%
      addMarkers(lng=103.8198, lat=1.3521, icon=iconos$singapore, popup="Singapore") %>%
      addMarkers(lng=2.3522, lat=48.8566, icon=iconos$paris, popup="Paris") %>%
      addMarkers(lng=-122.4194, lat=37.7749, icon=iconos$sanF, popup="San Francisco") %>%
      addMarkers(lng=151.2093, lat=-33.8688, icon=iconos$sydney, popup="Sydney") %>%
      addMarkers(lng=139.6917, lat=35.6895, icon=iconos$tokyo, popup="Tokyo") %>%
      addMarkers(lng=-79.3832, lat=43.6532, icon=iconos$toronto, popup="Toronto") %>%
      addMarkers(lng=-0.1276, lat=51.5074, icon=iconos$london, popup="London")
  })
  
  
  #WORDCLOUD
  output$wordcloud <- renderPlot({
    plot_industry_wordcloud(input$industrySelected)
  })
  
  #GRAPHICS
  output$plot1 <- renderPlot({
    plot_industry_automation_impact_score(input$location1)
  })
  output$plot2 <- renderPlotly({
    plot_automation_risk_by_industry(input$location2)
  })
  output$plot3 <- renderPlot({
    plot_salary_by_industry(input$location3)
  })
  output$plot4 <- renderPlot({
    plot_job_security_by_industry(input$location4)
  })
  output$plot5 <- renderPlot({
    plot_compensation_change_by_industry(input$location5)
  })
  #second panel
  output$plot6 <- renderPlot({
    plot_automation_probability_by_industry(input$location6)
  })
  output$plot7 <- renderPlotly({
    plot_interactive_automation_probability_by_industry(input$location7)
  })
  output$plot8 <- renderPlotly({
    plot_interactive_automation_boxplot(input$location8)
  })
  output$plot9 <- renderPlot({
    plot_ridgeline_automation(input$location9)
  })
  
  #Third panel
  output$plot10 <- renderPlot({
    plot_job_growth_by_industry(input$location10)
  })
  output$plot11 <- renderPlotly({
    plot_skills_vs_risk_percent(input$location11)
  })
  
  #fourth panel 
  output$plot12 <- renderPlot({
    plot_ai_adoption_by_company_size(input$location12)
  })
  output$plot13 <- renderPlot({
    plot_remote_friendly_by_industry(input$location13)
  })
  output$plot14 <- renderPlot({
    plot_skills_wordcloud(input$location14)
  })
  
}


#RUN THE APP
shinyApp(ui = ui, server = server)

shiny::addResourcePath("www", "C:\\Users\\alega\\OneDrive\\Documentos\\ProjecttR")