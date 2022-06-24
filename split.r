library(tibble)
evaluateObjectiveFunction <- function(tbl, objectiveFunction) {

}
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) abs(x - round(x)) < tol

split <- function(tbl, objectiveFunction) {
    values <- vector()
    doublesIndex <- vector()
    variablesNames <- vector()
    for (ind in 1:(ncol(tbl) - nrow(tbl))) {
        ind2 <- which(tbl[, ind] == 1)
        valSol <- round(tbl[ind2, ncol(tbl)], 3)
        values <- append(values, valSol)
        variablesNames <- append(variablesNames, rownames(tbl)[ind2])
        print(variablesNames)
        if (!is.wholenumber(valSol)) {
            doublesIndex <- append(doublesIndex, ind)
        }
    }
    if (length(doublesIndex) == 0) {
        return(values)
    } else {
        left <- data.frame(tbl)
        right <- data.frame(tbl)
        leftConstraints <- round(values[doublesIndex[1]])
        rightConstraints <- leftConstraints + 1
        columnIndex <- which(colnames(left) == variablesNames[1])

        left <- add_column(left, integer(nrow(left)), .before = ncol(left))
        newRowLeft <- integer(ncol(left))
        newRowLeft[columnIndex[1]] <- 1
        newRowLeft[ncol(left) - 1] <- 1
        left <- rbind(left[1:(ncol(left) - 4), ], newRowLeft, left[-(1:(ncol(left) - 4)), ])

        right <- add_column(right, integer(nrow(right)), .before = ncol(right))
        newRowRight <- integer(ncol(right))
        newRowRight[columnIndex[1]] <- 1
        newRowRight[ncol(right) - 1] <- -1
        right <- rbind(right[1:(ncol(right) - 4), ], newRowRight, right[-(1:(ncol(right) - 4)), ])
        if (simplex(right) > simplex(left)) {
            split(right)
        } else {
            split(left)
        }
    }
}

displayResult <- function() {
    tbl <- read.delim(file.choose(), header = FALSE, sep = " ") # store in a table the input file
    tbl <- createTable(tbl)
    tbl <- cbind(tbl[, 1:(ncol(tbl) - 2)], tbl[, (ncol(tbl))]) # erase the inequality column
    objectiveFunction <- tbl[nrow(tbl), 1:ncol(tbl) - 1]
    tbl <- simplex(tbl)
    split(tbl, objectiveFunction)
}
displayResult()