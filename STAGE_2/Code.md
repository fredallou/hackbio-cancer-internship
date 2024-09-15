**\> #Below is the R programming codes for analysis**

\>

\> ---

title: "Untitled"

**authors: "Claudia Agyeere", "Gideon Danso", "Frederick Allou", "Akeemat Ayinla", "Duong Gia Khanh"**

**date: "2024-09-14"**

\---

\`\`\`{r setup, include=FALSE}

\# Load necessary libraries

library(writexl)

library(ggplot2)

library(readxl)

library(DT)

library(plotly)

library(dplyr)

library(wordcloud)

library(RColorBrewer)

library(orca)

library(tidyr)

library(reshape2)

library(forcats)

library(htmlwidgets)

library(htmltools)

library(prettydoc)

library(ggridges)

library(hrbrthemes)

library(extrafont)

library(stringr)

library(lattice)

library(babynames)

library(reticulate)

\# Set working directory

setwd("C:/Users/musah yussif/Downloads/HackBio/stage 2/AMR\_APP")

\# Load the data

amr\_data <- read\_excel("AMR\_PRODUCTS\_1.xlsx")

\`\`\`

\`\`\`{r setup-chunk, include=FALSE, echo=FALSE}

\#knitr::opts\_chunk$set(dev = "png",

                     # dpi = 300,

                     # echo = FALSE,

                     # cache = TRUE)

\`\`\`

\`\`\`{r 1, echo=FALSE}

\# Loading the dataset

url <- "https\://raw\.githubusercontent.com/HackBio-Internship/public\_datasets/main/R/WHO\_AMR\_PRODUCTS\_DATA.tsv"

amr\_data <- read.table(url, header = TRUE, sep = "\t")

\# View the first few rows

head(amr\_data)

\#saving file

write\_xlsx(amr\_data, "AMR\_PRODUCTS.xlsx")

\#summary

summary(amr\_data)

\#checking for missing data

colSums(is.na(amr\_data))

\# Summary of the data

summary(amr\_data)

\# Checking for missing data

colSums(is.na(amr\_data))

\# Converting 'y' to 'Yes' in the 'Mycobacterium.tuberculosis' and 'Clostridioides.difficile' columns

\#amr\_data$Mycobacterium.tuberculosis <- ifelse(amr\_data$Mycobacterium.tuberculosis == "y", "Yes", amr\_data$Mycobacterium.tuberculosis)

\#amr\_data$Clostridioides.difficile <- ifelse(amr\_data$Clostridioides.difficile == "y", "Yes", amr\_data$Clostridioides.difficile)

\`\`\`

\`\`\`{r 2, echo=FALSE, fig.width=10, fig.cap="Figure 1. Showing the Distribution of Product Type"}

\# Creating a bar graph for Product Type

ggplot(amr\_data, aes(x = Product.type)) +

  geom\_bar(fill = "steelblue", color = "black", width = 0.7) +

  theme\_minimal(base\_size = 15) +

  labs(

    title = "Distribution of Product Type",

    x = "Product Type",

    y = "Count"

  ) +

  coord\_flip() +

  theme(

    plot.title = element\_text(hjust = 0.3, face = "bold", color = "darkblue"),

    axis.title.x = element\_text(face = "bold", color = "darkred"),

    axis.title.y = element\_text(face = "bold", color = "darkred"),

    axis.text.x = element\_text(size = 10),

    axis.text.y = element\_text(size = 10),

    panel.grid = element\_blank(),

    axis.line = element\_line(color = "black")

  )

\`\`\`

\`\`\`{r 1i, echo=FALSE, fig.width=10, fig.cap="Figure 2. Showing the Distribution of R & D Phase of Products"}

\# assigning data to amr\_data

data <- amr\_data %>%

  count(R.D.phase) %>%   

  summarize(n = sum(n)) %>%                                 

  ungroup() %>%

  arrange(n) %>%                                            

  mutate(R.D.phase = factor(R.D.phase, levels = c("Phase I", "Phase II", "Phase III", "Preregistration"))) 

\# Defining colors for each R.D.phase

colors <- c("Phase I" = "blue", 

            "Phase II" = "green", 

            "Phase III" = "red", 

            "Preregistration" = "purple")

\# Creating the plot with annotations for each bar

ggplot(data, aes(x = R.D.phase, y = n, color = R.D.phase)) +

  geom\_segment(aes(xend = R.D.phase, yend = 0)) +

  geom\_point(size = 4) +

  geom\_text(aes(label = n), vjust = -0.5, size = 5) +  # Add text annotations

  scale\_color\_manual(values = colors) + # Apply custom colors

  coord\_flip() +

  theme\_bw() +

  xlab("R.D. Phase") +  # Label x-axis

  ylab("Count") +# Label y-axis

  ggtitle("Distribution of R\&D Phases")

 

\`\`\`

\`\`\`{r comparing the r.d phase to the product type, echo=FALSE, fig.height=12, fig.width=12, fig.cap="Figure 3. Showing the Distribution of Products and their R & D Phases"}

\# Preparing data

filtered\_data <- amr\_data %>%

  count(Product.name, R.D.phase) %>%

  arrange(Product.name, R.D.phase)

\# Plot

p <- ggplot(filtered\_data, aes(x = reorder(Product.name, n), y = n)) +

  geom\_bar(stat = "identity", fill = "lightblue") +

  geom\_text(aes(label = n), vjust = -0.5, hjust = 1.2, size = 2, color = "black") +  

  facet\_wrap(\~ R.D.phase, scales = "free\_y") +

  theme\_minimal(base\_family = "sans") +  

  coord\_flip() +

  theme(

    legend.position = "none",

    axis.text = element\_text(size = 8),

    axis.title = element\_text(size = 10),

    axis.line = element\_line(color = "black"),

    panel.grid.major.y = element\_blank(),

    panel.grid.minor.y = element\_blank()

  ) +

  xlab("Product Name") +

  ylab("Count") +

  ggtitle("Product Distribution Across R\&D Phases")

print(p)

\`\`\`

\`\`\`{r Administration, echo=FALSE, fig.height=7, fig.width=10, fig.cap="Figure 6. Showing the Route of Administration"}

\# Creating dataset

route\_counts <- data.frame(

  Route.of.administration = factor(

    c("Colonoscopy", "Enema", "Inhalation", "IV", "IV & oral", "IV, Oral", "Oral", "Oral, Not absorbed"),

    levels = c("Colonoscopy", "Enema", "Inhalation", "IV", "IV & oral", "IV, Oral", "Oral", "Oral, Not absorbed")

  ),

  Count = c(1, 1, 65, 326, 13, 65, 195, 3)

)

\# Ploting with counts displayed

ggplot(route\_counts, aes(x = Route.of.administration, y = Count, color = Route.of.administration)) +

  geom\_line(aes(group = 1), linewidth = 1) +  \`

  geom\_point(size = 3) +

  geom\_text(aes(label = Count), vjust = -0.8, size = 4) +  

  theme\_minimal() +

  theme(

    legend.position = "right", 

    axis.text.x = element\_text(angle = 45, hjust = 1, size = 10), 

    axis.title = element\_text(size = 12),

    axis.line = element\_line(color = "black"),

    panel.grid = element\_blank()

  ) +

  xlab("Route of Administration") +

  ylab("Count") +

  ggtitle("Distribution of Routes of Administration")

\`\`\`

\`\`\`{r Administration, echo=FALSE, fig.height=7, fig.width=10, fig.cap="Figure 6. Showing the Route of Administration"}

\# Creating dataset

route\_counts <- data.frame(

  Route.of.administration = factor(

    c("Colonoscopy", "Enema", "Inhalation", "IV", "IV & oral", "IV, Oral", "Oral", "Oral, Not absorbed"),

    levels = c("Colonoscopy", "Enema", "Inhalation", "IV", "IV & oral", "IV, Oral", "Oral", "Oral, Not absorbed")

  ),

  Count = c(1, 1, 65, 326, 13, 65, 195, 3)

)

\# Plotting with counts displayed

ggplot(route\_counts, aes(x = Route.of.administration, y = Count, color = Route.of.administration)) +

  geom\_line(aes(group = 1), linewidth = 1) +  \`

  geom\_point(size = 3) +

  geom\_text(aes(label = Count), vjust = -0.8, size = 4) +  

  theme\_minimal() +

  theme(

    legend.position = "right", 

    axis.text.x = element\_text(angle = 45, hjust = 1, size = 10), 

    axis.title = element\_text(size = 12),

    axis.line = element\_line(color = "black"),

    panel.grid = element\_blank()

  ) +

  xlab("Route of Administration") +

  ylab("Count") +

  ggtitle("Distribution of Routes of Administration")

\`\`\`

\`\`\`{r 7, echo=FALSE, fig.height=5, fig.width=10, fig.cap="Figure 6. Showing the Distribution Pathogen Activity against Active against Priority pathogens" }

\# Filtering data for only 'Yes' in Active.against.priority.pathogens.

filtered\_data <- amr\_data %>%

  filter(Active.against.priority.pathogens. == "Yes")  # Filter for 'Yes' only

\# Creating the horizontal bar plot

ggplot(filtered\_data, aes(x = Pathogen.activity, fill = Pathogen.activity)) +

  geom\_bar() +  

  coord\_flip() +  

  geom\_text(stat = 'count', aes(label = after\_stat(count)), hjust = -0.2, size = 4, fontface = "bold") +  

  theme\_minimal() +  

  labs(

    x = "Pathogen Activity",  

    y = "Count",  

    title = "Pathogen Activity for 'Yes' Active Priority Pathogens"  

  ) +

  theme(

    axis.text.x = element\_text(size = 12, face = "bold"),  # Bold x-axis labels

    axis.text.y = element\_text(size = 12, face = "bold"),  # Bold y-axis labels

    axis.title.x = element\_text(size = 14, face = "bold"),  # Bold x-axis title

    axis.title.y = element\_text(size = 14, face = "bold"),  # Bold y-axis title

    plot.title = element\_text(size = 12, face = "bold", hjust = 0.5),  

    legend.position = "none",  

    axis.line = element\_line(color = "black", linewidth = 0.8)  

  ) +

  scale\_fill\_manual(values = c("skyblue", "orange", "green", "red"))  

\`\`\`

\`\`\`{r 8, echo=FALSE, fig.height=6, fig.width=10, fig.cap="Figure 7. Showing the Distribution of Pathogen name against Active Priority pathogen" }

\# Filtering data for only 'Yes' in Active.against.priority.pathogens.

filtered\_data <- amr\_data %>%

  filter(Active.against.priority.pathogens. == "Yes") %>%

  count(Pathogen.name)  # Count occurrences of each pathogen for 'Yes'

\# Reordering the data based on count

filtered\_data <- filtered\_data %>%

  arrange(n) %>%

  mutate(Pathogen.name = factor(Pathogen.name, levels = Pathogen.name))

\# Plotting with counts displayed

p <- ggplot(filtered\_data, aes(x = Pathogen.name, y = n)) +

  geom\_segment(

    aes(x = Pathogen.name, xend = Pathogen.name, y = 0, yend = n), 

    color = "black", 

    linewidth = 0.7

  ) +

  geom\_point(

    color = "red", 

    size = 2

  ) +

  geom\_text(aes(label = n), vjust = -0.5, size = 3.5) +  

  coord\_flip() +

  theme(

    axis.line = element\_line(color = "black", linewidth = 0.5),

    text = element\_text(family = "sans"),  

    panel.grid = element\_blank(),  

    plot.title = element\_text(size = 12, hjust = 0.5)  

  ) +

  xlab("") +

  ylab("Number of 'Yes' for Active Against Priority Pathogens") +

  ggtitle("Number of 'Yes' for Active Pathogens by Pathogen Name")

\# Display the plot

p

\`\`\`

\`\`\`{r 9, echo=FALSE, fig.height=9, fig.width=10, fig.cap="Figure 8. Showing the Distribution of Product Name by Active Pathogens"}

\# Filtering data for only 'Yes' in Active.against.priority.pathogens

filtered\_data <- amr\_data %>%

  filter(Active.against.priority.pathogens. == "Yes") %>%

  count(Product.name)  # Count occurrences of each product for 'Yes'

\# Reordering the data based on count

filtered\_data <- filtered\_data %>%

  arrange(n) %>%

  mutate(Product.name = factor(Product.name, levels = Product.name))

\# Plotting with counts displayed below the points

p <- ggplot(filtered\_data, aes(x = Product.name, y = n)) +

  geom\_segment(

    aes(x = Product.name, xend = Product.name, y = 0, yend = n), 

    color = "black", 

    linewidth = 0.7

  ) +

  geom\_point(

    color = "purple", 

    size = 2

  ) +

  coord\_flip() +

  theme(

    axis.line = element\_line(color = "black", linewidth = 0.5),

    text = element\_text(family = "sans"),  

    panel.grid = element\_blank(),  

    plot.title = element\_text(size = 12, hjust = 0.5),

    plot.margin = margin(10, 10, 30, 10),  

    axis.text.y = element\_text(size = 7, face = "bold", color = "black") 

  ) +

  xlab("") +

  ylab("Number of 'Yes' for Active Against Priority Pathogens") +

  ggtitle("Number of 'Yes' for Active Pathogens by Product Name")

\# Display the plot

p

\`\`\`

\`\`\`{r 10, echo=FALSE, fig.height=6, fig.width=10, fig.cap="Figure 9. Showing the Distribution of Alternative name by Active priority pathogen"}

\# Filtering data for only 'Yes' in Active.against.priority.pathogens

filtered\_data <- amr\_data %>%

  filter(Active.against.priority.pathogens. == "Yes") %>%

  count(Alternative.name)  # Count occurrences of each alternative for 'Yes'

\# Reordering the data based on count

filtered\_data <- filtered\_data %>%

  arrange(n) %>%

  mutate(Alternative.name = factor(Alternative.name, levels = Alternative.name))

\# Plot

p <- ggplot(filtered\_data, aes(x = Alternative.name, y = n)) +

  geom\_segment(

    aes(x = Alternative.name, xend = Alternative.name, y = 0, yend = n), 

    color = "black", 

    linewidth = 0.7

  ) +

  geom\_point(

    color = "green", 

    size = 2

  ) +

  coord\_flip() +

  theme(

    axis.line = element\_line(color = "black", linewidth = 0.5),

    text = element\_text(family = "sans"),  

    panel.grid = element\_blank(),  

    plot.title = element\_text(size = 12, hjust = 0.5),

    plot.margin = margin(10, 10, 30, 10),  

    axis.text.y = element\_text(size = 7, face = "bold", color = "black")  

  ) +

  xlab("") +

  ylab("Number of 'Yes' for Active Against Priority Pathogens") +

  ggtitle("Number of 'Yes' for Active Pathogens by Alternative Name")

\# Display the plot

p

\`\`\`

\`\`\`{r grouped barplot, echo=FALSE, fig.height=11.5, fig.width=11, fig.cap="Figure 10. Showing the Distribution of Product Name by Active Pathogens"}

\# Converting 'NA' values to 'Unknown' and filter out 'Unknown' and 'N/A'

amr\_data$Active.against.priority.pathogens. <- ifelse(is.na(amr\_data$Active.against.priority.pathogens.) | amr\_data$Active.against.priority.pathogens. == "N/A", "Unknown", amr\_data$Active.against.priority.pathogens.)

\# Filtering out 'Unknown'

amr\_data\_filtered <- amr\_data\[amr\_data$Active.against.priority.pathogens. != "Unknown", ]

\# Creating a horizontal stacked bar plot 

ggplot(amr\_data\_filtered, aes(x = reorder(Product.name, Product.name, length), fill = Active.against.priority.pathogens.)) +

  geom\_bar(position = "stack") +

  theme\_minimal() +

  labs(title = "Stacked Bar Chart of Product Names Against Active Priority Pathogens",

       x = "Product Name",

       y = "Count",

       fill = "Active Against Priority Pathogens") +

  theme(axis.text.x = element\_text(size = 8), 

        axis.text.y = element\_text(size = 8, margin = margin(r = 10)), 

        axis.line = element\_line(color = "black", linewidth = 0.5),

        panel.grid = element\_blank(),

        legend.position = "bottom",

        plot.margin = margin(10, 10, 10, 10),

        plot.title = element\_text(size = 15, hjust = 0.5)

        ) + 

  coord\_flip() 

\`\`\`
