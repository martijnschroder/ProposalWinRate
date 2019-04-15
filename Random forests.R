library(party)
print(head(readingSkills))
output.forest <- randomForest(nativeSpeaker ~ age + shoeSize + score, 
                              data = readingSkills)

print(output.forest) 

print(importance(output.forest,type = 2)) 

# select data set without NA values
proposals_clean <- proposals %>% na.omit() %>% select(stage, amount)
