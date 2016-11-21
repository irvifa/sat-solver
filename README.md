# computational-logic-assignment

## Prerequisites

Install `ghc` and python `kivy` module

### Build

```bash
make build
```

### Clean

```bash
make clean
```

## src

### Build

```bash
make build
```

### Clean

```bash
make clean
```

### Run

```bash
make run PUZZLE=${PATH_TO_PUZZLE} MINISATPATH=${MINISAT_PATH}
```

For example `make run PUZZLE=example/puzzle9x9 MINISATPATH="./../minisat/core/minisat"`

### Generate Next Solution

If the previous solution available, we could generate more solution

```bash
make next MINISATPATH=${MINISAT_PATH}
```
