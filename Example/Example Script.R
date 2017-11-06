### Creating Example 
set.seed(999)
example <- data.frame('var1' = rexp(1000, 3) + 2,
                      'var2' = rpois(1000, 10),
                      'var3' = rpois(1000, 1) + 1)


example

write.csv(example, 'Example/data_example.csv', row.names = FALSE)

## Creating Var Def

var.def <- data.frame('Variable' = c('var1', 'var2', 'var3'),
                      'a' = c(1, NA, 2),
                      'b' = c(5, 30, 6),
                      'Type' = c('Amounts', 'Counts', 'Ordered Categories'),
                      'To.Reverse' = c(0, 1, 0))

write.csv(var.def, 'Example/vardef_example.csv', row.names = FALSE)

