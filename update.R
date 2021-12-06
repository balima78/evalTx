
install.packages('rsconnect')

#removeAccount("bioestatisticas") # bruno-lima

rsconnect::setAccountInfo(name='balima',
                          token='6722923A15EB1ECE4CD912A8867A3374',
                          secret='ijITxv6kuSsQMyaoLQ00uz5LtErEgTCd2Ue2HGnv')

library(rsconnect)
rsconnect::deployApp("C:/Users/balim/Documents/4.APPs/evalTx", appName = "scoreTx")

