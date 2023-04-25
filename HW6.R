library(tidyverse)
library(tidyr)
library(ggplot2)



tree_data = read_csv("TS3_Raw_tree_data.csv")

#Question 1: Sample sizes by state
#This code splits the city and state in two sepereate columns 
# Compare to in class I found that using the separate function is simpler  
# https://www.rdocumentation.org/packages/tidyr/versions/1.3.0/topics/separate 
tree_data = tree_data %>%
  separate(City, c("city", "state"), sep = ",\\s*", remove = FALSE) %>%
  select(city, state, everything())

# I counted the number of trees to get the records in each state and created a graph 
tree_data %>%
  group_by(state) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = state, y = count)) +
  geom_bar(stat = "identity") +
  labs(title = "Records in each state")

#Question 2: Cities in NC/SC
# Filter the tree_data to only North and South Carolina
filter_to_nc_sc_data = tree_data %>%
  filter(state %in% c("NC", "SC"))

# Listing all the unique cities did they collect data from in North and South Carolina
#"Charleston" "Charlotte" 
unique(filter_to_nc_sc_data$city)

#Question 3: Genera and species
#I extracted genus from scientific name and put it in a new column 
#str_extract was briefly mentioned in the parsing exerciser but I still didn't know how to set it up so this site helped
# https://www.gastonsanchez.com/r4strings/cleaning.html 
filter_to_nc_sc_data = filter_to_nc_sc_data %>%
  mutate(genus = str_extract(ScientificName, "^[A-Z][a-z]+"))

#find average crown diameter for each genus. 
#I created a new table for genus and ignored any missing values 
#then I arranged it by descending order 

#Quercus          13.62316  meter 
largest_crown = filter_to_nc_sc_data %>%
  group_by(genus) %>%
  summarize(avg_crown_dia = mean(`AvgCdia (m)`, na.rm = TRUE)) %>% 
  arrange(desc(avg_crown_dia)) %>% 
  head(3)

#Extra credit 

#Tree Age
#part one # I will first find the average age of each genus 
genus_avg_age = filter_to_nc_sc_data %>%
  group_by(genus) %>%
  summarize(avg_age = mean(Age)) 
#yes 

#Now I will find the average crown diameter and I will also add the average age of the genus to this
avg_age_crown = filter_to_nc_sc_data %>%
  group_by(genus) %>%
  summarize(avg_age = mean(Age), avg_crn = mean(`AvgCdia (m)`, na.rm = TRUE))

#I will now create a scatter plot to see the correlation and see which genus stands out with a large diameter for its age
#i searched up how i can label the dots in the graph 
ggplot(avg_age_crown, aes(x = avg_age, y = avg_crn, label = genus)) +
  geom_point() +
  xlab("Average Age") +
  ylab("Average Crown Diameter (m)") +
  geom_text(hjust = -0.2, vjust = -0.2, size = 3)

#I was still confused with which produces a large crown quickly with small age so I divided the average crown diamter by average age
avg_age_crown = filter_to_nc_sc_data %>%
  group_by(genus) %>%
  summarize(avg_age = mean(Age), avg_crn = mean(`AvgCdia (m)`, na.rm = TRUE), ratio = mean(`AvgCdia (m)`) / mean(Age))

#Now i will graph it 
ggplot(avg_age_crown, aes(x = genus, y = ratio)) +
  geom_point() +
  xlab("Genus") +
  ylab("Average Crown Diameter / Average Age")

#I choose Ulmus cause it had the highest diameter/age and also in the graph before I also assumed it would me Ulmus 

#EC part two 
# extract genus and species from scientific name to count the species 
#I used these sources to help me write this code: https://stackoverflow.com/questions/66475350/use-regular-expression-to-find-species-names-and-author-names
#I just had to modify the regex this to account for x in between the genus and species
#and the \\s.* accounts for plants may have additional information after the species
regex = "^[A-Z][a-z]+\\s([a-z]+[\\.\\s]?[a-z]*(['’][[:alpha:]]+)?([[:space:]]?x[[:space:]])?([[:space:]]var\\.[[:space:]][[:alpha:]]+)?)(\\s.*)?$"
# group by genus and species, count the number of records
count_species <- filter_to_nc_sc_data %>%
  group_by(genus, species = str_extract(ScientificName, regex)) %>%
  summarize(n = n())

#Now i need to add the total number of times each genus shows up
count_species <- filter_to_nc_sc_data %>%
  group_by(genus, species = str_extract(ScientificName, regex)) %>%
  summarize(n = n()) %>%
  group_by(genus) %>%
  summarize(total = n())




