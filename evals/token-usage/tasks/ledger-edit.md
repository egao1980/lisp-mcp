This directory holds the `ledger` system (see `ledger.asd`, `src/`, `tests/`). Make these
changes and keep every existing test passing.

1. Rename the function `acct-bal` to `account-balance` everywhere: the package export,
   the definition, every call, and the tests. Do not keep the old name.
2. `transfer` between accounts with different currencies must signal a new condition,
   `currency-mismatch`, before it moves any money.
   - It is a subclass of `ledger-error` and is exported.
   - Its readers `currency-mismatch-from` and `currency-mismatch-to` return the two
     accounts.
   - Establish a restart named `convert` that takes a rate (a real number). Invoked, it
     withdraws CENTS from FROM and deposits `(round (* cents rate))` into TO.
3. Add and export `(statement account)`: a list of strings, oldest entry first. Each
   string is `"YYYY-MM-DD <amount> <memo>"` with the date in UTC and the amount printed
   by `format-amount`, e.g. `"2025-11-03 100.00 EUR deposit"`.
4. Add tests for items 2 and 3.

Run `cl-repo test` in this directory to check your work, and finish when it passes.
