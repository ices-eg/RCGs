FAZ_SEGMENTACAO_COMPRIMENTO_DCF2<-function(dados, coluna = "Loa"){
  # Nuno Prista, IPMA, Portugal
  # 2015
  # FAZ_SEGMENTACAO_COMPRIMENTO_DCF adaptada para baltico
  # instrucoes: 'dados' e uma data.frame; 'coluna' e uma string com o nome da coluna que contem os comprimentos a categorizar (por omissao denominada "Loa")
  
  # definido por Appendix III da Decisao da Comissao 2010/93/EU de 18 Dezembro 2009
  # 0-< 8 m 
  # 8-< 10 m 
  # 10-< 12 m
  # 12-< 18 m
  # 18-< 24 m
  # 24-< 40 m
  # 40 m and larger
  
  # 11-03-2015: optimizado com funcao "cut"
  # 16-04-2015: alteracao do nome da funcao e da coluna final para DCF (era PNAB2)
  # 16-04-2015: explicitado o include.lowest=T por forma a tornar mais claro a categorizacao efectuada na funcao
  
  dados[,coluna]<-as.numeric(as.character(dados[,coluna]))
  dados$SEG_DCF<-cut(dados[,coluna], breaks=c(0,8,10,12,18,24,40,200), labels=c("[0-8[","[8-10[","[10-12[","[12-18[","[18-24[","[24-40[","[40+["), include.lowest=T, right=F)
  dados
}