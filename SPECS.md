## Acceptor
It's a function that takes in (a `rulelist`, and a `tokenlist`) and returns a `Some(d, s)` where `d` is a rule that was used to accept, and `s` is the rest of the tokenlist if valid, else it's just `None`.

## Matcher
It's a curried function that accepts (`acceptor` and `fragment`). The matcher stacks accepts on top of each other.
