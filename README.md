# `qaws` monorepo

This repository is a one-stop shop for the `qaws` base library designed to help with basic usage of
`amazonka`, as well as the sub-libraries for dealing with different `amazonka-*` libraries like
`amazonka-sqs`, etc.

## Using it in a project

Add the following to your `stack.yaml` in the `extra-deps` section:

```yaml
extra-deps:
- github: quanterall/qaws
  commit: 864a9380fcd32d567e67083216d1ea6c50078423
  subdirs:
  - qaws
  - qaws-sqs
```

Note that the commit ID should likely be whatever is latest in this repository.

When you've done this, you can now depend on `qaws` & `qaws-sqs` in your `package.yaml`.

## Libraries

### `qaws`

Provides helpers for creating/loading your AWS environment as well as helpers for executing `AWS`
actions with that environment, either passed as an argument or looked for in your
`MonadReader`-compatible transformer stack.

### `qaws-sqs`

Provides helper functions and types for dealing with sending and receiving messages, purging queues
and otherwise managing them.