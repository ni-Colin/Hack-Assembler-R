#load stringr package for text processing
library(stringr)
library(binaryLogic)
library(Hmisc)

#Function to Translate an Instruction
TranslateIns<-function(ins){
  if(substring(ins,1,1)=="@"){
    TranslateA(ins)
  }else{
    TranslateC(ins)
  }
}

#Function to Translate an A-Instruction
#Takes the value without the leading @ and returns a binary string representation padded to 16 bits
TranslateA<-function(s){
  str_pad(paste0(as.binary(substring(s,2)),collapse=""),16,pad="0")
}

#Function to translate a C-instruction
TranslateC<-function(c){
  #parse jump symbol
  jumpsym=unlist(strsplit(c,";"))[2]
  if(is.na(jumpsym)){jumpsym="null"}
  
  #parse computation and destination symbols
  compdest=unlist(strsplit(unlist(strsplit(c,";"))[1],"="))
  if(length(compdest)==1){
      compsym=compdest[1];destsym="null"
    }else
      {destsym=compdest[1];compsym=compdest[2]}
  
  #append leading 111 and substitute for comp dest jump
  paste0("111",TranslateComp(compsym),TranslateDest(destsym),TranslateJump(jumpsym))
  
}

#jump instruction substitution matrix
jumpMatrix=matrix(c('null','000','JGT','001','JEQ','010','JGE','011','JLT','100','JNE','101','JLE','110','JMP','111'),nrow=8, ncol=2 ,byrow=TRUE)

#Function to translate the jump instruction
TranslateJump<-function(j){
  jumpMatrix[(match(j,jumpMatrix[,1])),2]
}

#destination instruction substitution matrix
destMatrix=matrix(c('null','000','M','001','D','010','MD','011','A','100','AM','101','AD','110','AMD','111'),nrow=8, ncol=2 ,byrow=TRUE)

#function to translate the destination instruction
TranslateDest<-function(d){
  destMatrix[(match(d,destMatrix[,1])),2]
}

#computation instruction substitution matrix
compMatrix=matrix(c('0','0101010','1','0111111','-1','0111010','D','0001100','A','0110000','!D','0001101','!A','0110001','-D','0001111','-A','0110011','D+1','0011111','A+1','0110111','D-1','0001110','A-1','0110010','D+A','0000010','D-A','0010011','A-D','0000111','D&A','0000000','D|A','0010101','M','1110000','!M','1110001','-M','1110011','M+1','1110111','M-1','1110010','D+M','1000010','D-M','1010011','M-D','1000111','D&M','1000000','D|M','1010101'),nrow=28, ncol=2 ,byrow=TRUE)

#Function to translate the compinstruction
TranslateComp<-function(c){
  compMatrix[(match(c,compMatrix[,1])),2]
}


#load in the asm file as a vector
SourceVector=unlist(read.table("Rect.asm", header=FALSE, sep='\n', stringsAsFactors=FALSE),recursive = TRUE, use.names = FALSE)

#Delete lines that begin with a comment
#First create a vector that is false where lines begin // and true otherwise
#Then keep only the element of SourceVector when the corresponding values are true
SourceVector=SourceVector[substring(SourceVector[],1,2)!="//"]

#remove tab characters
SourceVector=str_replace_all(SourceVector[],"\t","")

#remove comments
SourceVector=str_replace(SourceVector[],"//..*","")

#remove spaces
SourceVector=str_replace_all(SourceVector[]," ","")

#-----Symbol handling goes here-----

#matrix of predefined symbols
predefinedSymbols=matrix(c('SP','0','LCL','1','ARG','2','THIS','3','THAT','4','R0','0','R1','1','R2','2','R3','3','R4','4','R5','5','R6','6','R7','7','R8','8','R9','9','R10','10','R11','11','R12','12','R13','13','R14','14','R15','15','SCREEN','16384','KBD','24576'),nrow=23, ncol=2 ,byrow=TRUE)

#function to determine if a line is a label
IsLabel<-function(c){
	substring(c,1,1)=="(" & substring(c,nchar(c),nchar(c))==")"
}

#function to extract symbol from label (remove brackets)
SymFromLabel<-function(c){
	substring(c,2,nchar(c)-1)
}

#create a matrix to store labels
labelSymbols=matrix(nrow=0,ncol=2)

#Pass through code (SourceVector) to determine labels and variables

#Label handling first
i2<-1
while (i2<=length(SourceVector)){
	#Prevents overrunning due to shortening of SourceVector when labels removed
	if(i2>length(SourceVector)){break}
	#If element of SourceVector is a label
	if(IsLabel(SourceVector[i2])){
		#add that label to the labelSymbols matrix (note that r indexes vectors from 1 whereas instructions are indexed from 0)
		labelSymbols=rbind(labelSymbols,matrix(c(SymFromLabel(SourceVector[i2]),i2-1),nrow=1,ncol=2,byrow=TRUE))
		#remove the label from theá SourceVector matrix
		SourceVector=SourceVector[-i2]
		i2<-i2-1
	}
  i2<-i2+1
}

#Create a Symbol Table by combining the predefined symbols with the labels
SymbolTable=rbind(predefinedSymbols,labelSymbols)

#Handle variables

#Function to check if an instruction contains a symbol
IsSymbol<-function(c){
	substring(c,1,1)=="@" & all.is.numeric(substring(c,2,nchar(c)))==FALSE
}

#Function to check if the symbol already exists in the Symbol table
InSymbolTable<-function(c){
	match((substring(c,2,nchar(c))),SymbolTable,nomatch=0)>0
}

#Function to replace @xxx with number from symbol table in SourceVector
ReplaceSymbolWithNum<-function(SourceIndex){
	symString=SourceVector[SourceIndex]
	SourceVector[SourceIndex]<<-(paste("@",SymbolTable[match(substring(symString,2,nchar(symString)),SymbolTable),2],sep=""))
}

#If already in symbol table, just replace
#If not already in symbol table, add to symbol table and then perform replacement

#Memory addresses up to 16 are reserved
VariableCounter=16
for (i in 1:length(SourceVector)){
	if(IsSymbol(SourceVector[i])){
		if(InSymbolTable(SourceVector[i])){
			ReplaceSymbolWithNum(i)
		}
		else{
			#add symbol to symbol table
			SymbolTable<-rbind(SymbolTable,cbind(substring(SourceVector[i],2,nchar(SourceVector[i])),VariableCounter))
			VariableCounter=VariableCounter+1
			#then replace in SourceVector
			ReplaceSymbolWithNum(i)
		}
	}
}

#Translate Symbol vector into binary instructions
BinVector=unlist(lapply(SourceVector,TranslateIns))

write(BinVector,"output.hack")