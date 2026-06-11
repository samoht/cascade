# Nested @media must not be merged

Nested `@media` rules should be minified but NOT flattened into a single rule.
Merging can change cascade behavior in edge cases.
