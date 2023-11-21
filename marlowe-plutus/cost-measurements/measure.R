require(data.table)
require(magrittr)

require(ggplot2)

raw <- fread("marlowe-semantics-rpacb.tsv", integer64="numeric")
results <-
  melt(
    raw[, .(`TxId`, `Steps`=`Reference CPU`, `Memory`=`Reference Memory`)]
  , id.vars=c("TxId")
  , measure.variables=c("CPU", "Memory")
  , variable.name="Cost"
  , value.name="Value"
  ) %>% suppressWarnings
results[, `Case`:="Production"]

for (f in dir(pattern="marlowe-semantics-.*.tsv")) {
  raw <- fread(f, integer64="numeric")
  caseResults <-
    melt(
      raw[, .(`TxId`, `Steps`=`Measured CPU`, `Memory`=`Measured Memory`)]
    , id.vars=c("TxId")
    , measure.variables=c("CPU", "Memory")
    , variable.name="Cost"
    , value.name="Value"
    ) %>% suppressWarnings
    caseResults[, `Case`:=substr(f, 19, 23)]
  results <- rbind(results, caseResults)
}

results[, `:=`(
  `Precondition`=grepl("r",`Case`,fixed=TRUE)
  , `Positive Balance`=grepl("p",`Case`,fixed=TRUE)
  , `Duplicate Accounts`=grepl("a",`Case`,fixed=TRUE)
  , `Duplicate Choices`=grepl("c",`Case`,fixed=TRUE)
  , `Duplicate Bindings`=grepl("b",`Case`,fixed=TRUE)
  )
]
results[`Case` == "Production", `:=`(
    `Precondition`=NA
  , `Positive Balance`=NA
  , `Duplicate Accounts`=NA
  , `Duplicate Choices`=NA
  , `Duplicate Bindings`=NA
  )
]

write.table(results, file="full.tsv", sep="\t", row.names=FALSE)

results %>% summary

summaryResults <-
  results[, .(
      Minimum=min(`Value`)
    , Mean=mean(`Value`)
    , Median=median(`Value`)
    , Maximum=max(`Value`)
    ), by=.(
      `Cost`
    , `Case`
    , `Precondition`
    , `Positive Balance`
    , `Duplicate Accounts`
    , `Duplicate Choices`
    , `Duplicate Bindings`
    )
  ]

write.table(summaryResults, file="summary.tsv", sep="\t", row.names=FALSE)

summaryResults %>% head

relativeResults <- summaryResults[`Case`!="Production"]
relativeResults[
  `Cost`=="Steps"
, `:=`(
    `Minimum`=`Minimum`/summaryResults[`Cost`=="Steps"&`Case`=="Production",`Minimum`]
   ,`Mean`=`Mean`/summaryResults[`Cost`=="Steps"&`Case`=="Production",`Mean`]
   ,`Median`=`Median`/summaryResults[`Cost`=="Steps"&`Case`=="Production",`Median`]
   ,`Maximum`=`Maximum`/summaryResults[`Cost`=="Steps"&`Case`=="Production",`Maximum`]
   )
]
relativeResults[
  `Cost`=="Memory"
, `:=`(
    `Minimum`=`Minimum`/summaryResults[`Cost`=="Memory"&`Case`=="Production",`Minimum`]
   ,`Mean`=`Mean`/summaryResults[`Cost`=="Memory"&`Case`=="Production",`Mean`]
   ,`Median`=`Median`/summaryResults[`Cost`=="Memory"&`Case`=="Production",`Median`]
   ,`Maximum`=`Maximum`/summaryResults[`Cost`=="Memory"&`Case`=="Production",`Maximum`]
   )
]

write.table(summaryResults, file="relative.tsv", sep="\t", row.names=FALSE)

summary(relativeResults)

relativeResults %>% head

rp <- function(n, x) paste(n, "=", round(100*x,2), "%", sep="")

mkNode <- function(case, x0, x1, x2, x3) {
  rp <- function(n, x) paste(n, sprintf("%.1f%%", 100*x), sep="=")
  paste(case, " [label=\"", case, "|{", paste(rp("min", x0), rp("avg", x1), rp("med", x2), rp("max", x3), sep="|"), "}\"]", sep="")
}

accept <- function(from, to) {
  from < to && length(unique(tstrsplit(paste(from,to,sep=""),split=""))) == nchar(from) + 1
}

differs <- function(a, b) do.call(setdiff, strsplit(c(a, b), split = ""))

prelude <-
  data.table(
    Line=c(
      "digraph memory {"
    , "fontname=\"monospace\""
    , "node [shape=record,fontname=\"monospace\"]"
    , "edge [fontname=\"monospace\"]"
    , "ranksep=1"
    , "measurements [label=\"Marlowe\\nsemantics\\nvalidator\\nmemory|{min=minimum|avg=mean|med=median|max=maximum}\"]"
    , "production [label=\"Production|{min=100.0%|avg=100.0%|med=100.0%|max=100.0%}\"]"
    , "cabal [label=\"Cabal\\nflags|{r = +check-preconditions|R = -check-preconditions|p = +check-positive-balances|P = -check-positive-balances|a = +check-duplicate-accounts|A = -check-duplicate-accounts|c = +check-duplicate-choices|C = -check-duplicate-choices|b = +check-duplicate-bindings|B = -check-duplicate-bindings}\"]"
    , "production -> rpacb [label=\"Plutus\\n1.15.0.0\\ncompiler\"]"
    )
  )

postlude <-
  data.table(
    Line=c(
      "}"
    )
  )

writeDot <- function(cost) {
  nodes <-
    relativeResults[
    , .(
        `Cost`
      , `Line`=mapply(mkNode, `Case`, `Minimum`, `Mean`, `Median`, `Maximum`)
      )
    ]
  cases <- relativeResults[, unique(`Case`)]
  links <-
  CJ(Cost=relativeResults[, unique(`Cost`)], From=cases, To=cases)[
      mapply(accept, From, To)
    , .(
        `Cost`
      , `Line`=paste(From, " -> ", To, " [label=", mapply(differs, To, From), "]", sep="")
      )
    ]
  write.table(rbind(
      prelude
    , nodes[`Cost`==cost, .(`Line`)]
    , links[`Cost`==cost, .(`Line`)]
    , postlude
  ), file=paste(cost, "dot", sep="."), quote=FALSE, row.names=FALSE, col.names=FALSE)
}

writeDot("Memory")

writeDot("Steps")


