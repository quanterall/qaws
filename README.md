# `qaws` monorepo

This repository is a one-stop shop for the `qaws` base library designed to help with basic usage of
`amazonka`, as well as the sub-libraries for dealing with different `amazonka-*` libraries like
`amazonka-sqs`, etc.

## Libraries

### `qaws`

Provides helpers for creating/loading your AWS environment as well as helpers for executing `AWS`
actions with that environment, either passed as an argument or looked for in your
`MonadReader`-compatible transformer stack.

### `qaws-sqs`

Provides helper functions and types for dealing with sending and receiving messages, purging queues
and otherwise managing them.