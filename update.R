
install.packages('rsconnect')

#removeAccount("bioestatisticas") # bruno-lima

rsconnect::setAccountInfo(name='balima',
                          token='6722923A15EB1ECE4CD912A8867A3374',
                          secret='ijITxv6kuSsQMyaoLQ00uz5LtErEgTCd2Ue2HGnv')

library(rsconnect)
rsconnect::deployApp("D:/1.CHN/SPT2021/evalTx", appName = "scoreTx")

