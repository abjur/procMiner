a1 <- 'data-raw/dje_pdf/tjsc_dje_2016-03-15/tjsc_dje_1_2016-03-15.pdf'
a2 <- 'data-raw/dje_pdf/tjsp_dje_2016-03-16/tjsp_dje_11_2016-03-16.pdf'
a3 <- 'data-raw/dje_pdf/tjal_dje_2016-03-17/tjal_dje_3_2016-03-17.pdf'

a2 %>% pdf2text(1,1,F,T) %>% tira_header_pagina('TJSP') %>% encontra_processos()

header_alagoas = 'deAlagoas -LeiFederalnº11.419/06,art.4ºDisponibilização:quinta-feira,17demarçode2016DiárioOficialPoderJudiciário-CadernoJurisdicional-PrimeiroGrauMaceió,AnoVII-Edição'
header_sp =      'deSãoPaulo-LeiFederalnº11.419/06,art.4ºDisponibilização:quarta-feira,16demarçode2016DiáriodaJustiçaEletrônico-CadernoJudicial-2ªInstânciaSãoPaulo,AnoIX-Edição20772'

pgs = a2 %>% pdf2text(r = T, keep_file = T)

system.time(pgs %>% encontra_processos('TJSP'))
system.time(pgs %>%  encontra_processos('TJSP'))

lista_sp = list.files('data-raw/dje_pdf/tjsp_dje_2016-03-16/', full.names = T)
lista_sc = list.files('data-raw/dje_pdf/tjsc_dje_2016-03-15/', full.names = T)
lista_al = list.files('data-raw/dje_pdf/tjal_dje_2016-03-17/', full.names = T)

sp = lista_sp %>% extrai_processos(raw = T)
al = lista_al %>% extrai_processos(tr = '02', raw = T, tj = 'TJAL')
sc = lista_sc %>% extrai_processos(tr = '24', raw = T, tj = 'TJSC')