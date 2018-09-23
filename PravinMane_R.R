library(rjson);

#Bulk calculation
StartOfDayPositions = read.csv(file = "Input_StartOfDay_Positions.txt", header = TRUE, sep = ",");
TxnData <- fromJSON(paste(readLines("1537277231233_Input_Transactions.json"), collapse=""));
TxnData <- as.data.frame(fromJSON(paste(readLines("1537277231233_Input_Transactions.json"), collapse="")));
# TxnData = read.csv(file = "TxnData.csv");
AggTxnData = aggregate(formula = TransactionQuantity ~ Instrument + TransactionType, data = TxnData[,c("Instrument", "TransactionType", "TransactionQuantity")], FUN = sum);

EndOfDayPositions = StartOfDayPositions;
EndOfDayPositions$Delta = 0;
for(i in 1:dim(AggTxnData)[1])
{
  if(AggTxnData$TransactionType[i] == "B")
  {
    # Calculation for Account TYpe = I
    RowInd = which((EndOfDayPositions$Instrument == as.character(AggTxnData$Instrument[i]))*(EndOfDayPositions$AccountType == "I") ==1);
    EndOfDayPositions$Quantity[RowInd] = EndOfDayPositions$Quantity[RowInd] - AggTxnData$TransactionQuantity[i];
    EndOfDayPositions$Delta[RowInd] = EndOfDayPositions$Delta[RowInd] - AggTxnData$TransactionQuantity[i];
    rm(RowInd)
    # Calculation for Account TYpe = E
    RowInd = which((EndOfDayPositions$Instrument == as.character(AggTxnData$Instrument[i]))*(EndOfDayPositions$AccountType == "E") ==1);
    EndOfDayPositions$Quantity[RowInd] = EndOfDayPositions$Quantity[RowInd] + AggTxnData$TransactionQuantity[i];
    EndOfDayPositions$Delta[RowInd] = EndOfDayPositions$Delta[RowInd] + AggTxnData$TransactionQuantity[i];
    rm(RowInd)
  }
  if(AggTxnData$TransactionType[i] == "S")
  {
    # Calculation for Account TYpe = I
    RowInd = which((EndOfDayPositions$Instrument == as.character(AggTxnData$Instrument[i]))*(EndOfDayPositions$AccountType == "I") ==1);
    EndOfDayPositions$Quantity[RowInd] = EndOfDayPositions$Quantity[RowInd] + AggTxnData$TransactionQuantity[i];
    EndOfDayPositions$Delta[RowInd] = EndOfDayPositions$Delta[RowInd] + AggTxnData$TransactionQuantity[i];
    rm(RowInd)
    # Calculation for Account TYpe = E
    RowInd = which((EndOfDayPositions$Instrument == as.character(AggTxnData$Instrument[i]))*(EndOfDayPositions$AccountType == "E") ==1);
    EndOfDayPositions$Quantity[RowInd] = EndOfDayPositions$Quantity[RowInd] - AggTxnData$TransactionQuantity[i];
    EndOfDayPositions$Delta[RowInd] = EndOfDayPositions$Delta[RowInd] - AggTxnData$TransactionQuantity[i];
    rm(RowInd)
  }
}

# instruments with largest net transaction volumes for the day
print(paste("instruments with largest net transaction volumes for the day are\n",paste(unique(EndOfDayPositions$Instrument[which(abs(EndOfDayPositions$Delta) == max(abs(EndOfDayPositions$Delta)))]), collapse = ', ')))
# instruments with lowest net transaction volumes for the day
print(paste("instruments with lowest net transaction volumes for the day are\n",paste(unique(EndOfDayPositions$Instrument[which(abs(EndOfDayPositions$Delta) == min(abs(EndOfDayPositions$Delta)))]), collapse = ', ')));
      
      

# per transaction calculation
StartOfDayPositions = read.csv(file = "Input_StartOfDay_Positions.txt", header = TRUE, sep = ",");
TxnData <- fromJSON(paste(readLines("1537277231233_Input_Transactions.json"), collapse=""));
TxnData <- as.data.frame(fromJSON(paste(readLines("1537277231233_Input_Transactions.json"), collapse="")));
# TxnData = read.csv(file = "TxnData.csv");

EndOfDayPositions = StartOfDayPositions;
EndOfDayPositions$Delta = 0;
for(i in 1:dim(TxnData)[1])
{
  if(TxnData$TransactionType[i] == "B")
  {
    # Calculation for Account TYpe = I
    RowInd = which((EndOfDayPositions$Instrument == as.character(TxnData$Instrument[i]))*(EndOfDayPositions$AccountType == "I") ==1);
    EndOfDayPositions$Quantity[RowInd] = EndOfDayPositions$Quantity[RowInd] - TxnData$TransactionQuantity[i];
    EndOfDayPositions$Delta[RowInd] = EndOfDayPositions$Delta[RowInd] - TxnData$TransactionQuantity[i];
    rm(RowInd)
    # Calculation for Account TYpe = E
    RowInd = which((EndOfDayPositions$Instrument == as.character(TxnData$Instrument[i]))*(EndOfDayPositions$AccountType == "E") ==1);
    EndOfDayPositions$Quantity[RowInd] = EndOfDayPositions$Quantity[RowInd] + TxnData$TransactionQuantity[i];
    EndOfDayPositions$Delta[RowInd] = EndOfDayPositions$Delta[RowInd] + TxnData$TransactionQuantity[i];
    rm(RowInd)
  }
  if(TxnData$TransactionType[i] == "S")
  {
    # Calculation for Account TYpe = I
    RowInd = which((EndOfDayPositions$Instrument == as.character(TxnData$Instrument[i]))*(EndOfDayPositions$AccountType == "I") ==1);
    EndOfDayPositions$Quantity[RowInd] = EndOfDayPositions$Quantity[RowInd] + TxnData$TransactionQuantity[i];
    EndOfDayPositions$Delta[RowInd] = EndOfDayPositions$Delta[RowInd] + TxnData$TransactionQuantity[i];
    rm(RowInd)
    # Calculation for Account TYpe = E
    RowInd = which((EndOfDayPositions$Instrument == as.character(TxnData$Instrument[i]))*(EndOfDayPositions$AccountType == "E") ==1);
    EndOfDayPositions$Quantity[RowInd] = EndOfDayPositions$Quantity[RowInd] - TxnData$TransactionQuantity[i];
    EndOfDayPositions$Delta[RowInd] = EndOfDayPositions$Delta[RowInd] - TxnData$TransactionQuantity[i];
    rm(RowInd)
  }
}

# instruments with largest net transaction volumes for the day
print(paste("instruments with largest net transaction volumes for the day are\n",paste(unique(EndOfDayPositions$Instrument[which(abs(EndOfDayPositions$Delta) == max(abs(EndOfDayPositions$Delta)))]), collapse = ', ')));
# instruments with lowest net transaction volumes for the day
print(paste("instruments with lowest net transaction volumes for the day are\n",paste(unique(EndOfDayPositions$Instrument[which(abs(EndOfDayPositions$Delta) == min(abs(EndOfDayPositions$Delta)))]), collapse = ', ')));

