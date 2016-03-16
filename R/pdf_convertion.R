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

pdf2text <- function(a, keep_file = F, new_file = "repo.txt", raw = F, first_pg = NA, last_pg = NA){
  bash = ifelse(raw,'pdftotext -raw','pdftotext')
  bash = ifelse(is.na(first_pg),bash,paste(bash,'-f',first_pg))
  bash = ifelse(is.na(last_pg),bash,paste(bash,'-l',last_pg))
  bash = paste(bash,a,new_file)
  
  if(!file.exists(a)){return('')}
  
  if(file.size(a) > 5000){
    system(bash)
    text = readr::read_file(new_file)
    ifelse(keep_file,NA,file.remove(new_file))
  } else {
    text = ''
  }
  return(text)
}

encontra_processos <- function(text, orgao = '8', tr = '26'){
  text %>%
    stringi::stri_replace_all(regex = '[\n]', replacement = '')%>%
    stringi::stri_replace_all(regex = '\f[0-9].*de2016|[\f\r]', replacement = '') %>%
    stringi::stri_extract_all(regex = formatos(orgao,tr)) %>%
    dplyr::first() %>%
    dplyr::data_frame() %>%
    setNames('n_processo')
}

filtra_anos <- function(processos, anos = seq(2009,2015)){
  processos %>%
    plyr::adply(1,substr, start = 12, stop = 15) %>%
    dplyr::filter(V1 %in% anos) %>%
    dplyr::select(n_processo)
}

extrai_processos_arq <- function(a,orgao = '8', tr = '26'){
  print(a)
  a %>%
    pdf2text %>%
    encontra_processos(orgao, tr)
}

extrai_processos <- function(lista_arquivos, orgao = '8', tr = '26'){
  dplyr::data_frame(arq = lista_arquivos) %>%
    dplyr::group_by(arq) %>%
    dplyr::do(extrai_processos_arq(.$arq, orgao,tr)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(n_processo)
}
