# Potential Applications


## Guarantee Correctness
Make illegal states unrepresentable by adding constraints and generating a new type.
```
input  = { { <q:[<>], a:[<>]> | 1len val.q <= 1len val.a } | 1len val.a <= 1len val.q }
output = <q:<0:[<α:<>, β:<>>]>, a:<>>
```

## Efficiency
Use known properties to automatically prune unused states from your types.
```
input  = { [<>] + [<>] | match(Left 1len val <= 0len val) }
output = <>
```

## Remove Partiality
Make libraries more user-friendly, less error prone by encoding constraints into any type system.
