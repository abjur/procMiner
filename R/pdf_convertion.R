#' @export
formatos <- function(orgao = '8', tr = '26'){
  paste0(
    #formato CNJ - 2008
    '[0-9]{7}\\-[0-9]{2}\\.[0-9]{4}\\.',orgao,'\\.',tr,'\\.[0-9]{4}|',
    #nro saj
    '[0-9]{3}\\.[0-9]{2}\\.[0-9]{6}\\-[0-9]{1}|',
    #nro que aparece no tjal 1
    '[0-9]{5}\\-[0-9]{1}\\.[0-9]{4}\\.[0-9]{3}'
  )
}

#' @export
pdf2text <- function(a, first_pg = NA, last_pg = NA, r = F, keep_file = F, new_file = 'repo.txt'){
  
  if(!file.exists(a)){return('')}
  
  if(file.size(a) > 5000 & stringi::stri_detect(a,fixed = '.pdf')){
    sprintf('pdftotext %s %s%s%s%s',
            a,
            ifelse(r,'-raw ',' '),
            ifelse(!is.na(first_pg),paste('-f',first_pg,''),' '),
            ifelse(!is.na(last_pg),paste('-l',last_pg,''),' '),
            new_file) %>% 
      system()
    texto = readr::read_file(new_file)
    ifelse(keep_file,NA,file.remove(new_file))
  } else {
    texto = ''
  }
  return(texto)
}

#' @export
encontra_processos <- function(texto, orgao = '8', tr = '26', tj = 'TJSP'){
  texto %>%
    tira_header_pagina(tj) %>% 
    stringi::stri_extract_all(regex = formatos(orgao,tr)) %>%
    dplyr::first() %>%
    dplyr::data_frame() %>%
    setNames('n_processo') %>% 
    dplyr::distinct(n_processo)
}

#' @export
tira_header_pagina <- function(texto, tj = 'TJSP'){
  texto %<>% 
    stringi::stri_replace_all(regex = '\n', replacement = '')
  if(tj == 'TJSC'){
    texto %<>% 
      stringi::stri_replace_all(regex = '\f[0-9].*?de2016|[\f\r]', replacement = '')
  } else if (tj %in% c('TJSP','TJAL')){
    texto %<>% 
      stringi::stri_replace_all(regex = '[\f ]', replacement = '') %>% 
      stringi::stri_replace_all(regex = 'PublicaçãoOficialdoTribunal.*?Edição[0-9]{3,6}', replacement = '')
  } 
  return(texto)
}

#' @export
extrai_processos_arq <- function(a,orgao = '8', tr = '26', r = F, tj= 'TJSP', save_file = T){
  d = a %>%
    pdf2text(raw = r) %>%
    encontra_processos(orgao, tr, tj)
  
  if(save_file){saveRDS(d,file = stringi::stri_replace(a, fixed = '.pdf', replacement = '.rds'))}
  
  return(d)
}

#' @export
extrai_processos <- function(lista_arquivos, orgao = '8', tr = '26', raw = F, tj = 'TJSP'){
  dplyr::data_frame(arq = lista_arquivos) %>%
    dplyr::group_by(arq) %>%
    dplyr::do(extrai_processos_arq(.$arq, orgao,tr,raw)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(n_processo)
}

#' @export
tipo_n_procesos <- function(d){
  d %>% 
    dplyr::select(n_processo) %>% 
    dplyr::first() %>% 
    stringi::stri_length() %>% 
    table()
}

#' @export
revstr <- function(x) sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")

#' Encontra a merda do digito verificador de um numero cnj
#'
#' @export
calcula_digito <- function(num, monta = FALSE) {
  NNNNNNN <- substr(num,  1L,  7L)
  AAAA <-    substr(num,  8L, 11L)
  JTR <-     substr(num, 12L, 14L)
  OOOO <-    substr(num, 15L, 18L)
  n1 <- sprintf('%02d', as.numeric(NNNNNNN) %% 97)
  n2 <- sprintf('%02d', as.numeric(sprintf('%s%s%s', n1, AAAA, JTR)) %% 97)
  n3 <- sprintf('%02d', 98 - ((as.numeric(sprintf('%s%s', n2, OOOO)) * 100) %% 97))
  dig <- n3
  if(monta) {
    return(sprintf('%s%s%s', substr(num, 1, 7), dig, substr(num, 8, 18)))
  }
  return(dig)
}

#' 
#' @export 
saj2cnj <- function(nro_processo, orgao, tr){
  nro_processo %>% 
    stringr::str_split_fixed('[\\-\\.]',4) %>%
    data.frame() %>%
    dplyr::transmute(
      n_processo = sprintf('0%s20%s%s%s0%s',X3,X2,orgao,tr,X1),
      dig = calcula_digito(n_processo),
      n_processo = sprintf('0%s-%s.20%s.%s.%s.0%s',X3,dig,X2,orgao,tr,X1)) %>% 
    dplyr::select(nro_processo)
}