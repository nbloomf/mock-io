# mock-io

Typeclasses for IO effects.

Abstracts the (well, a few) base library IO functions behind typeclasses, one per function.

Define your own type class having exactly the IO-effectful capabilities you want and write code against that; then define your own IO-like type and use it as a drop-in replacement for the real IO.

Uses:
* Mocking IO for testing purposes.
* Fine control over allowed IO effects.
