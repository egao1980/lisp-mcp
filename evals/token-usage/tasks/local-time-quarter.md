This directory is the source of the `local-time` library, version 1.0.6. Add support for
calendar quarters.

1. Export and implement `(timestamp-quarter timestamp &key (timezone *default-timezone*))`.
   It returns 1, 2, 3 or 4 for the quarter of TIMESTAMP's month in TIMEZONE.
2. Support a `:quarter` element in `format-timestring` formats. It works alone, or as
   `(:quarter n)` padded like the other numeric elements. For example,
   `'(:year "-Q" :quarter)` formats 2026-02-10 as `"2026-Q1"`.
3. Add tests for both to the library's test suite (fiasco, in `test/`).

Run `cl-repo test` in this directory to check your work, and finish when it passes.
