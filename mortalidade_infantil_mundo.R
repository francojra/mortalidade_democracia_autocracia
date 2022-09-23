
# Mortalidade infantil --------------------------------------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco --------------------------------------------------------------------------------------------------------------
# Data: 23/09/22 --------------------------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/child-mortality ----------------------------------------------------------------------------------------

# Sobre os dados ---------------------------------------------------------------------------------------------------------------------------

### Nesse registro, nós estamos dando uma visão geral sobre a mortalidade de
### bebês e crianças. Na demografia, mortalidade de crianças se refere a mortalidade
### de crianças com menos de 5 anos de idade, enquanto a morte de bebês se refere
### a morte de crianças com menos de 1 ano de idade.

### A mortalidade infantil hoje é a mais baixa de todos os tempos. Em menos de 
### três décadas a mortalidade infantil caiu para metade - De 12,5 milhões em 1990
### para 5,2 milhões em 2019. Isso é uma grande conquista que não deve ser subestimada.

### Obviamente, a morte de cada criança é uma enorme tragédia, e em muitos países
### muitas crianças morrem por causas que nós sabemos como prevenir e tratar.
### Hoje, a maior taxa de mortalidade infantil está na África sub-saariana, onde
### ainda existem países com taxa de mortalidade infantil maior que 10%. Isso significa
### que uma criança a cada 10 que nascem morre antes de alcançar os 5 anos de idade.

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(cols4all)
library(hrbrthemes)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

mi <- read.csv("child-mortality-igme.csv")
view(mi)
names(mi)

# Manipular dados --------------------------------------------------------------------------------------------------------------------------

mi <- mi %>%
  select(-Code) %>%
  rename(mort_inf = Mortality.rate..under.5..per.1.000.live.births.) %>%
  view()

mi1 <- mi %>%
  filter(Entity %in% c("China", "North Korea", "Cuba",
                       "United States", "Japan", "Germany")) %>%
  group_by(Entity) %>%
  summarise(media = mean(mort_inf),
            n = n(), sd = sd(mort_inf),
            se = sd/sqrt(n)) %>%
  view()

mi2 <- mi %>%
  filter(Entity %in% c("China", "North Korea", "Cuba",
                       "United States", "Japan", "Germany"),
         (between(Year, 1990, 2020))) %>%
  view()

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

c4a("safe", 6)

ggplot(mi1, aes(x = fct_reorder(Entity, media), 
                y = media, fill = Entity)) +
  geom_col(width = 0.9) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                size = 1, width = 0.2) +
  scale_fill_manual(values = c("#88CCEE", "#CC6677",
                               "#DDCC77", "#117733",
                               "#332288", "#AA4499")) +
  scale_y_continuous(expand = expansion(mult = c(0,0))) +
  labs(x = "Países", y = "Número médio de mortes (milhões)") +
  theme_ipsum(axis_title_size = 16,
              axis_text_size = 14) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))

ggplot(mi2, aes(x = Year, y = mort_inf,
                group = Entity, color = Entity)) +
  geom_line(size = 1.3) +
  scale_color_manual(values = c("#88CCEE", "#CC6677",
                               "#DDCC77", "#117733",
                               "#332288", "#AA4499")) +
  labs(x = "Tempo (anos)", y = "Número de mortes (milhões)") +
  theme_ipsum(axis_title_size = 16,
              axis_text_size = 14) +
  theme(axis.text = element_text(color = "black"))

