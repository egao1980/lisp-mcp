In the current directory (it is empty), create a Common Lisp project.

- `ring-buffer.asd` defines system `ring-buffer` (sources in `src/`) and
  `ring-buffer/tests` (Rove, sources in `tests/`), connected through `test-op`.
- Package `ring-buffer` exports:
  - `(make-ring capacity)`: a ring of fixed capacity, a positive integer.
  - `(ring-push ring item)`: adds ITEM; when the ring is full, the oldest item
    is overwritten.
  - `(ring-pop ring)`: removes and returns the oldest item. On an empty ring it
    signals `ring-empty`, a subclass of `error`, with a `use-value` restart
    whose value `ring-pop` then returns.
  - `(ring-count ring)`, `(ring-full-p ring)`, and `(ring->list ring)`, which
    returns the items oldest first without removing them.
- Tests cover push and pop order, overwriting when full, and `ring-empty`
  together with the `use-value` restart.

Dependencies come from cl-repo; do not use Quicklisp. Run `cl-repo test` in this
directory to check your work, and finish when it passes.
