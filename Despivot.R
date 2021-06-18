#Bibliotecas
library(dplyr)
library(googlesheets4)
library(googledrive)
require(tidyr)

segredo <-'C:/Users/Gabriel_Gomes/OneDrive/Área de Trabalho/Credenciais/Credenciais_G&G.json'

drive_auth(path = segredo)
gs4_auth(path = segredo)

#Dados

Orcamento_IClinic <- read_sheet("https://docs.google.com/spreadsheets/d/1Ds0a7jn8NkP8Y-G_KtHij-72KTZWvHeE28agt0D690M/edit#gid=2002813053")

Orcamento_IClinic_trat <- as.data.frame(Orcamento_IClinic) %>%
  filter(!is.na(Orcamento_IClinic["Centro de Custo"]))

names(Orcamento_IClinic_trat)

View(tidyr::pivot_longer(Orcamento_IClinic_trat, cols = 8:44,names_to = "Competência", values_to = "Teste"))
