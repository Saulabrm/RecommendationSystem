
#Load libraries
library(stringr)
library(data.table)
library(dplyr)
library(plyr)

######      Building the dictionary ######

#Anger
Apathy <-  "Annoyed ~ Apathetic ~ Bored ~ Certain ~ Cold ~ Crabby ~ Cranky ~ Critical ~ Cross ~ Detached ~ Displeased ~ Frustrated ~ Impatient ~ Indifferent ~ Irritated ~ Peeved ~ Rankled"
Anger <- "Affronted ~ Aggravated ~ Angry ~ Antagonized ~ Arrogant ~ Bristling ~ Exasperated ~ Incensed ~ Indignant ~ Inflamed ~ Mad ~ Offended ~ Resentful ~ Riled up ~ Sarcastic"
Hatred <- "Aggressive ~ Appalled ~ Belligerent ~ Bitter ~ Contemptuous ~ Disgusted ~ Furious ~ Hateful ~ Hostile ~ Irate ~ Livid ~ Menacing ~ Outraged ~ Ranting ~ Raving ~ Seething ~ Spiteful ~ Vengeful ~ Vicious ~ Vindictive ~ Violent"

#Fear
Anxiety <- "Alert ~ Apprehensive ~ Cautious ~ Concerned ~ Confused ~ Curious ~ Disconcerted ~ Disoriented ~ Disquieted ~ Doubtful ~ Edgy ~ Fidgety ~ Hesitant ~ Indecisive ~ Insecure ~ Instinctive ~ Intuitive ~ Leery ~ Pensive ~ Shy ~ Timid ~ Uneasy ~ Watchful"
Fear <- "Afraid ~ Alarmed ~ Anxious ~ Aversive ~ Distrustful ~ Fearful ~ Jumpy ~ Nervous ~ Perturbed ~ Rattled ~ Shaky ~ Startled ~ Suspicious ~ Unnerved ~ Unsettled ~ Wary ~ Worried"
Panic <- "Filled with Dread ~ Horrified ~ Panicked ~ Paralyzed ~ Petrified ~ Phobic ~ Shocked ~ Terrorized"

#Happines
Happiness <-"Amused ~ Calm ~ Encouraged ~ Friendly ~ Hopeful ~ Inspired ~ Jovial ~ Open ~ Peaceful ~ Smiling Upbeat"
Contentment <- "Cheerful ~ Contented ~ Delighted ~ Excited ~ Fulfilled ~ Glad ~ Gleeful ~ Gratified ~ Happy ~ Healthy Self-esteem ~ Joyful ~ Lively ~ Merry ~ Optimistic ~ Playful ~ Pleased ~ Proud ~ Rejuvenated ~ Satisfied"
Joy <- "Awe-filled ~ Blissful ~ Ecstatic ~ Egocentric ~ Elated ~ Enthralled ~ Euphoric ~ Exhilarated ~ Giddy ~ Jubilant ~ Manic ~ Overconfident ~ Overjoyed ~ Radiant ~ Rapturous ~ Self-aggrandized ~ Thrilled"

#Sadness
Sadness <- "Contemplative ~ Disappointed ~ Disconnected ~ Distracted ~ Grounded ~ Listless ~ Low ~ Regretful ~ Steady ~ Wistful"
Grief <- "Dejected ~ Discouraged ~ Dispirited ~ Down ~ Downtrodden ~ Drained ~ Forlorn ~ Gloomy ~ Grieving ~ Heavy-hearted ~ Melancholy ~ Mournful ~ Sad ~ Sorrowful ~ Weepy ~ World-weary"
Depression <- "Anguished ~ Bereaved ~ Bleak ~ Depressed ~ Despairing ~ Despondent ~ Grief-stricken ~ Heartbroken ~ Hopeless ~ Inconsolable ~ Morose"

#Shame and Guilt
SoftShameAndGuilt <- "Abashed ~ Awkward ~ Discomfited ~ Flushed ~ Flustered ~ Hesitant ~ Humble ~ Reticent ~ Self-conscious ~ Speechless ~ Withdrawn"
MoodStateShameandGuilt <- "Ashamed ~ Chagrined ~ Contrite ~ Culpable ~ Embarrassed ~ Guilty ~ Humbled ~ Intimidated ~ Penitent ~ Regretful ~ Remorseful ~ Reproachful ~ Rueful ~ Sheepish"
IntenseShameandGuilt <- "Belittled ~ Degraded ~ Demeaned ~ Disgraced ~ Guilt-ridden ~ Guilt-stricken ~ Humiliated ~ Mortified ~ Ostracized ~ Self-condemning ~ Self-flagellating ~ Shamefaced ~ Stigmatized"

#Category of Emotions
Category <- c("Anger", "Fear", "Happiness", "Sadness", "Shame and Guilt")

#Lists of Words for each Emotion
emotionsList <- list("Apathy"= Apathy, "Anger" = Anger, "Hatred" = Hatred, "Anxiety" = Anxiety,
                     "Fear" = Fear, "Panic" = Panic, "Happiness" = Happiness, "Contentment" = Contentment,
                     "Joy" = Joy, "Sadness" = Sadness, "Grief" = Grief, "Depression" = Depression,
                     "Soft Shame and Guilt" = SoftShameAndGuilt,"Mood State Shame and Guilt" = MoodStateShameandGuilt,
                     "Intense Shame and Guilt" = IntenseShameandGuilt)


# List dataframes of dictionaries for emotions
df <- lapply(emotionsList, function(x){ 
 as.data.frame(unlist(strsplit(x,"~"))) 
  })

#Add Emotions and Category to each dataframe in the list
for(i in 1:length(df)){
  if(i == 1 | i == 2 | i == 3){
    df[[i]] <- cbind(df[[i]],"Emotion" = names(df[i]), "Category" = Category[1])  
  } else if(i == 4 | i == 5 | i == 6){
    df[[i]] <- cbind(df[[i]],"Emotion" = names(df[i]), "Category" = Category[2])  
  } else if(i == 7 | i == 8 | i == 9){
    df[[i]] <- cbind(df[[i]],"Emotion" = names(df[i]), "Category" = Category[3])  
  } else if(i == 10 | i == 11 | i == 12){
    df[[i]] <- cbind(df[[i]],"Emotion" = names(df[i]), "Category" = Category[4])    
  } else{
    df[[i]] <- cbind(df[[i]],"Emotion" = names(df[i]), "Category" = Category[5])  
  }
}
#Create Dictionary - Join dataframes
dictionary <- ldply(df,data.frame)[,2:4]
names(dictionary) <- c("Words","Emotion","Category")

write.csv(dictionary,"dictionary.csv")
