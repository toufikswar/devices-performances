library(tidyverse)


#df <- read_delim(file = "./DeX_Baseline_Compact/data-raw/sample_data.csv", delim = "\t")

df <- read_delim(file = "./DeX_Baseline_Compact/data-raw/DEX_Device_data_Blacstone_April2020.csv", delim = "\t")

df %>%
  group_by(`OS version and architecture`) %>%
  summarize(`Number of devices` = n(),
   dex_score = mean(`Digital Experience Score (Digital Experience Score)`)) %>%
  mutate(`OS version and architecture` = str_replace_all(`OS version and architecture`,"Windows", "W")) %>%
  mutate(`OS version and architecture` = str_replace_all(`OS version and architecture`,"Enterprise", "E")) %>%
  mutate(`OS version and architecture` = str_replace_all(`OS version and architecture`,"Professional", "Pro")) %>%
  mutate(`OS version and architecture` = str_replace_all(`OS version and architecture`,"(64 bits)", "64")) %>%
  mutate(`OS version and architecture` = str_replace_all(`OS version and architecture`,"(32 bits)", "32")) %>%
  arrange(desc(`Number of devices`)) %>%
  ungroup() %>%
  ggplot(mapping = aes(x = reorder(`OS version and architecture`, -`Number of devices`))) +
  geom_bar(mapping = aes(y = `Number of devices`), stat = "identity", fill = "#1046A8", colour = "black") +
  geom_line(mapping = aes(y = dex_score * 300), colour = "#3194F7", group = 1) +
  geom_point(mapping = aes(y = dex_score * 300), colour = "#F9BA37", group = 1) +
  geom_text(mapping = aes(y = dex_score * 300, label = round(dex_score,1)), colour = "#FF7461", group = 1, size = 5, vjust = -1) +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ . /300, name = "DeX")) +
  theme_minimal() +
  labs(x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size=12))

  
  # NO LENOVO
df %>%
  filter(`Device manufacturer` != "Lenovo") %>%
  group_by(`Device model`) %>%
  summarize(number_of_devices = n(),
            DXS_Device = mean(`DXS - Device (DXS - Device)`, na.rm = T),
            DXS_Boot = mean(`Boot speed (DXS - Device)`, na.rm = T),
            DXS_Logon = mean(`Logon duration (DXS - Device)`, na.rm = T),
            DXS_BSOD = mean(`BSODs (DXS - Device)`, na.rm = T),
            DXS_Reset = mean(`Hard resets (DXS - Device)`, na.rm = T),
            DXS_Free_Space = mean(`System free space (DXS - Device)`, na.rm = T),
            DXS_CPU = mean(`CPU usage (DXS - Device)`, na.rm = T),
            DXS_Memory = mean(`Memory usage (DXS - Device)`, na.rm = T)
            ) %>%
  ungroup() %>%
  filter(number_of_devices >=25) %>%
  arrange(desc(number_of_devices)) %>%
  na.omit() %>%
  gather(key  = "DXS", value = "score", -`Device model`,-number_of_devices) %>%
  mutate(`DXS` = str_replace_all(`DXS`,"DXS_", "")) %>%
  ggplot(mapping = aes(x = reorder(`Device model`, -number_of_devices), y=score, fill = DXS)) +
  geom_bar(stat = "identity", position = "stack", alpha=0.8, color="black") +
  geom_text(aes(label=round(score,1)),position=position_stack(vjust = 0.5),color="white", fontface = "bold", size=5, show.legend = F) +
  geom_label(aes(y =-1, label=paste(number_of_devices, " devices")), show.legend = F, fill = "white")+
  scale_fill_manual(values = c("#111F65", "#3194F7", "#1046A8", "#F9BA37", "#BA74D2", "#08B3AB", "#FF7461", "#B3BBC8")) +
  theme_void()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 12))

  geom_line(mapping = aes(y = number_of_devices*5), group =1, linetype = 2) +
  geom_point(mapping = aes(y = number_of_devices*5), color = "orange", group =1) +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ . /5, name = "# of devices")) +
  scale_fill_manual(values = c("#111F65", "#3194F7", "#1046A8", "#F9BA37", "#BA74D2", "#08B3AB", "#FF7461", "#B3BBC8")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))

c("#111F65", "#3194F7", "#1046A8", "#F9BA37", "#BA74D2", "#08B3AB", "#FF7461", "#B3BBC8")


#ONLY LENOVO
df %>%
  filter(`Device manufacturer` == "Lenovo") %>%
  group_by(`Device product version`) %>%
  summarize(count_n = n(),
            DXS_Device = mean(`DXS - Device (DXS - Device)`, na.rm = T),
            DXS_Boot = mean(`Boot speed (DXS - Device)`, na.rm = T),
            DXS_Logon = mean(`Logon duration (DXS - Device)`, na.rm = T),
            DXS_BSOD = mean(`BSODs (DXS - Device)`, na.rm = T),
            DXS_Reset = mean(`Hard resets (DXS - Device)`, na.rm = T),
            DXS_Free_Space = mean(`System free space (DXS - Device)`, na.rm = T),
            DXS_CPU = mean(`CPU usage (DXS - Device)`, na.rm = T),
            DXS_Memory = mean(`Memory usage (DXS - Device)`, na.rm = T)
  ) %>%
  arrange(desc(count_n))



df %>%
  group_by(`Number of hard resets`) %>%
  summarize(`Number of devices` = n(),
            `Average RAM` = round(mean(`Total RAM [bytes]`/(1024^3)),0),
            `Average number of crashes` = round(mean(`Number of application crashes`),0),
            `Average number of freezes` = round(mean(`Number of application not responding events`),0)) %>%
  formattable(align = c("c","c","c","c","c"),
              list(`Number of devices` = color_bar(customGreen),
                   `Average number of crashes` = color_bar(customRed),
                   `Average number of freezes` = color_bar(customOrange)))




if (lapply(df$Entity, function(x) grepl(x,"-"))){
  print("test")
}


if (any(lapply(df$Entity, function(x) grepl("-", x)))){
  print("OK")
}

(lapply(df$Entity, function(x) grep("-", x)))

df <- df %>%
  mutate(Entity = replace(Entity, which(grepl("^-$", Entity)),"Unknown"))

df %>%
filter(grepl("^r$", Entity))

if (nrow(filter(df, grepl("^-$",Entity))) == 0){
  print("No dash found")
}

