# Calc sum flattening across nested calc()

Nested additions inside calc() are flattened into a single addition. Two nested calc() sums are merged and all px terms are combined: `10 + 20 + 30 + 40 = 100px`.
