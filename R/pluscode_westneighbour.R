#' @title Pluscode West Neighbour
#'
#' @description This package retrieves the westerly neighbour of a valid pluscode, with a precision of 2, 4, 8, or 10 excluding the plus sign (which can be included)
#'
#' @param pluscode A valid pluscod 2, 4, 8, or 10 characters in length excluding the plus sign (which can be included)
#'
#' @return NULL
#'
#' @examples pluscode_westneighbour("9C4MGC2M+H4")
#'
#' @export

pluscode_westneighbour <- function(pluscode) {
  code<-c("2","3","4","5","6","7","8","9","C","F","G","H","J","M","P","Q","R","V","W","X")
  pluscode<-toupper(gsub(pattern = "\\+",replacement = "",pluscode))
  pluscode_length<-nchar(pluscode)
  if(pluscode_length %in% c(2,4,8,10)!=TRUE) {
    stop(paste0("The pluscode is not a valid length, please enter value with length of 2/4/6/8/10, or 9/11 (with + character)"))
  }
  for(i in strsplit(pluscode,"")[[1]]){
    if(any(grepl(i,code))!=TRUE){
      stop(paste0("The character ",i," is not a valid pluscode character"))
    }
  }
  d10<-strsplit(pluscode,"")[[1]][10]
  d9<-strsplit(pluscode,"")[[1]][9]
  d8<-strsplit(pluscode,"")[[1]][8]
  d7<-strsplit(pluscode,"")[[1]][7]
  d6<-strsplit(pluscode,"")[[1]][6]
  d5<-strsplit(pluscode,"")[[1]][5]
  d4<-strsplit(pluscode,"")[[1]][4]
  d3<-strsplit(pluscode,"")[[1]][3]
  d2<-strsplit(pluscode,"")[[1]][2]
  d1<-strsplit(pluscode,"")[[1]][1]
  if(d1%in% seq(3,9)!=TRUE){
    stop(paste0("The character ",d1," is not a valid pluscode character for the first character"))
  }
  n10<-if(is.na(d10)==TRUE) {
    NA
  } else if (grep(d10,code)==1) {
    code[20]
  } else {
    code[grep(d10,code)-1]
  }
  n8<-if(is.na(d8)==TRUE) {
    NA
  } else if(any(is.na(d10)==TRUE | grep(d10,code)==1,na.rm = T) & grep(d7,code)==1){
    code[20]
  } else if (any(is.na(d10)==TRUE | grep(d10,code)==1,na.rm = T)) {
    code[grep(d8,code)-1]
  } else {
    code[grep(d8,code)]
  }
  n6<-if(is.na(d6)==TRUE) {
    NA
  } else if(any(is.na(d10)==TRUE | grep(d10,code)==1,na.rm = T) & any(is.na(d8)==TRUE | grep(d8,code)==1,na.rm = T) & grep(d6,code)==1){
    code[20]
  } else if (any(is.na(d10)==TRUE | grep(d10,code)==1,na.rm = T) & any(is.na(d8)==TRUE | grep(d8,code)==1,na.rm = T)) {
    code[grep(d6,code)-1]
  } else {
    code[grep(d6,code)]
  }
  n4<-if(is.na(d4)==TRUE) {
    NA
  } else if(any(is.na(d10)==TRUE | grep(d10,code)==1,na.rm = T) & any(is.na(d8)==TRUE | grep(d8,code)==1,na.rm = T) & any(is.na(d6)==TRUE | grep(d6,code)==1,na.rm = T) & grep(d4,code)==1){
    code[20]
  } else if (any(is.na(d10)==TRUE | grep(d10,code)==1,na.rm = T) & any(is.na(d8)==TRUE | grep(d7,code)==1,na.rm = T) & any(is.na(d6)==TRUE | grep(d6,code)==1,na.rm = T)) {
    code[grep(d4,code)-1]
  } else {
    code[grep(d4,code)]
  }
  n2<-if(is.na(d2)==TRUE) {
    NA
  } else if(any(is.na(d10)==TRUE | grep(d10,code)==1,na.rm = T) & any(is.na(d8)==TRUE | grep(d8,code)==1,na.rm = T) & any(is.na(d6)==TRUE | grep(d6,code)==1,na.rm = T) & any(is.na(d4)==TRUE | grep(d4,code)==1,na.rm = T) & grep(d2,code)==1){
    code[20]
  } else if(any(is.na(d10)==TRUE | grep(d10,code)==1,na.rm = T) & any(is.na(d8)==TRUE | grep(d8,code)==1,na.rm = T) & any(is.na(d6)==TRUE | grep(d6,code)==1,na.rm = T) & any(is.na(d4)==TRUE | grep(d4,code)==1,na.rm = T)) {
    as.numeric(d2)-1
  } else {
    d2
  }
  #if(n1>9 | n1<1){
  #  stop("Neighbour pluscode is out of bounds")
  #}
  pluscode_neighbour<-if(pluscode_length == 10) {
    paste0(d1,n2,d3,n4,d5,n6,d7,n8,"+",d9,n10)
  } else if(pluscode_length == 8) {
    paste0(d1,n2,d3,n4,d5,n6,d7,n8,"+")
  } else if(pluscode_length == 4) {
    paste0(d1,n2,d3,n4)
  } else if(pluscode_length == 2) {
    paste0(d1,n2)
  }
  return(pluscode_neighbour)
}
