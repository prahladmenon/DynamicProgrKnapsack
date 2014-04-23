################################################################
# R script using only the base package
################################################################
# dynamic programming designed for the knapsack problem
#
# knapsack problem ::
# decision variables: include item x (1) or not (0)
# linear constraints: total set of items implies a cost lower than the capacity xw<=C
# objective function: maximise total value v = xv=V

# with thanks to the coursera course on discrete optimisation
# of professor Pascal Van Hentenryck

# manual specification of a tiny dataset
# in agreement with course notes

# maximum weight
maxw <- 31181
# the combinations of value - weight for each of 20 items
tdta <- scan(sep="")
19 31181
1945 4990
321 1142
2945 7390
4136 10372
1107 3114
1022 2744
1101 3102
2890 7280
962 2624
1060 3020
805 2310
689 2078
1513 3926
3878 9656
13504 32708
1865 4830
667 2034
1833 4766
16553 40006

dta <- matrix(tdta,nrow=20,byrow=T)

# if not included manually... import it as follows:
# my data import
# dtaName <- "data/ks_19_0"
# dta <- read.table(dtaName,sep=" ")
# nri <- dta[1,1]
# maxw <- dta[1,2]
# dta <- dta[-1,]
dimnames(dta)[[1]] <- paste0("i",1:nrow(dta))
dimnames(dta)[[2]] <- c("v","w")

# reorder the items according to their weight to get near the maximum as soon as possible
dta <- dta[rev(order(dta[,2])),]

# remove combinations that are invalid from the start
# only consider items with a weight that is less than the capacity
dta <- dta[dta[,'w']<=maxw,]

# construct a matrix with a row for every possible capacity, columns are assigned to items that are included or not
CapByItem <- data.frame(0:maxw,0,0)
names(CapByItem) <- c("cap","i0",dimnames(dta)[[1]][1])
# assign the value of the first item (last column) if its weight is less then or equal to the capacity
CapByItem[,ncol(CapByItem)] <- dta[1,'v']*(dta[1,'w'] <= CapByItem[,'cap'])
# for the remaining items (rows in the dta set of value - weight combinations)
for(item in 2:nrow(dta)){
	# select the next column with values linked to the next item
	tmp <- CapByItem[,ncol(CapByItem)]
	# create a temporary dataframe determine the best of two situations, items already included, new item, or both
		# sofar : copy of part of the last column
		# correct : copy of part of the last column if the next item's weight can be added, 0 otherwise
	Tmp <- data.frame(sofar=tmp[(dta[item,'w']+1):length(tmp)],correct=tmp[1:(length(tmp)-dta[item,'w'])])
	# add a column of values for the next item to the resulting column 'correct'
	Tmp$newValue <- Tmp$correct+dta[item,'v']
	# copy the last column
	CapByItem <- cbind(CapByItem,CapByItem[,ncol(CapByItem)]); names(CapByItem)[ncol(CapByItem)] <- dimnames(dta)[[1]][item]
	# overwrite the last column with the maximum of each row of the temporary matrix
	CapByItem[(dta[item,'w']+1):length(tmp),dimnames(dta)[[1]][item]] <- apply(Tmp,1,max)	
}
# collect the items that are included using backtracking
collect <- numeric()
# set up temporary matrix to avoid loosing the final result so far
Tmp <- CapByItem
# for all columns with values for items
while(ncol(Tmp)>2){
	# take the last column of the shrinking matrix
	it <- ncol(Tmp)
	# if the last value (highest capacity) of the two last columns differ then the item is included
	if(Tmp[nrow(Tmp),it]!=Tmp[nrow(Tmp),it-1]){
		# add the item to the list of collected items
		collect <- c( names(Tmp)[it],collect)
		# reduce the matrix to include only the rows for which the next item can be included
		Tmp <- Tmp[cumsum(Tmp[,it]>Tmp[nrow(Tmp),it]-dta[it-2,'v'])<1,1:(it-1)]
	}
	else{
		# if the last rows of the last two columns are equal, remove the last column
		Tmp <- Tmp[,-ncol(Tmp)]
	}
}
rm(Tmp)

# the items that are included are :
collect
# the items imply the following value and weight combinations
dta[collect,]
# the final total value is :
sum(dta[collect,'v'])
# the final weight for this set of items is :
sum(dta[collect,'w'])
# the capacity still remaining is :
maxw - sum(dta[collect,'w'])

###############
# solution ####
# maximum 12248
# including "i15" "i4"  "i9"  "i14" "i7" 
# with total weigth 30996 so that 185 remaining
       # v    w
# i15 3878 9656
# i4  2945 7390
# i9  2890 7280
# i14 1513 3926
# i7  1022 2744
