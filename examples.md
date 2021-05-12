# Potential Applications
Below are some examples of potential applications of this technique.

**Note:** `output` is automatically generated from the latest version of the tool.

## Guarantee Correctness
Make illegal states unrepresentable by specifying constraints to generate a new type.
```
# e.g. Given a list of questions and a list of answers, ensure they have the same length.
input  = {{ <q:[<>], a:[<>]> | len val.q <= len val.a } | len val.a <= len val.q }
output = <q:<0:[<α:<>, β:<>>]>, a:<>>
```

## Guarantee Performance
Ensure certain algorithmic assumptions hold when implementing efficient data structures.
```
# e.g. Okasaki functional requires front to be shorter than the back for amortized O(1) operations.
input  = { <front:[<>], back:[<>]> | len val.front <= len val.back }
output = <front:<0:[<α:<>, β:<>>]>, back:[<>]>
```

## Improve Performance
Potentially could use known properties to automatically prune unused states from your types.
```
# e.g. Automatically drop type union tags and empty list pointers when unnecessary.
input  = { [<>] + [<>] | match(Left len val <= 0) }
output = <>
```

## Improve Interfaces
Design library interfaces that can be less error prone by encoding constraints into the type system.
```
# e.g. Encode that a random choice function requires a non-empty list of choices.
input  = { <choices:[<>]> | match(<choices ~ match(T :: T)>) }
output = <choices:<hd:<>, tl:[<>]>>
```
