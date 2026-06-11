# Duplicate nested rules should dedup and flatten

Two identical `.bar` rules nested inside `.foo` should collapse to one, then
flatten to `.foo .bar{...}` since there are no other rules in the parent.
