## Takeaways

- Laziness lets an infinite stream be defined once and computed only as far as needed.
- Most stream operations follow a similar pattern: `cons(current value, recursive call)`.
- Folds as data can be transformed, paused, and combined, as in `wc`.
- Files and sockets must close no matter how a stream ends.
