path <- "C:/Users/sheinina/Desktop/israelex22.xlsx"

suppressMessages(library(tidyverse))
suppressMessages(library(readxl))
israelex22 <- read_excel(path = path)

exvoters <- israelex22[nrow(israelex22),]

israelex22 <- israelex22 %>% 
  filter(`Vote Rate` < 1) %>% 
  mutate(Voters = labor + Gimel + Arab + Yamina + Yisrael_Beytenu + Likkud + MRZ + Blue_White + SHAS,
         Right = Yamina + Gimel + SHAS + Likkud,
         Left = Blue_White + MRZ + labor,
         Leaning = ifelse(Right > Left, 'Right', 'Left'))


israelex22 %>% 
  group_by(Leaning) %>% 
  summarise(Rate = weighted.mean(`Vote Rate`, P.Voters)) #%>% clipr::write_clip()
# The numbers are lower than the ofiicial 69% voting rate since we do not take double enveloped votes


cor(israelex22$`Vote Rate`, (israelex22$G + israelex22$Yamina + israelex22$JP + israelex22$Likkud + israelex22$SHAS)/israelex22$Voters)
plot((israelex22$G + israelex22$Yamina + israelex22$JP + israelex22$Likkud + israelex22$SHAS)/israelex22$Voters, israelex22$`Vote Rate`)