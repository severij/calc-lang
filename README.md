# calc-lang

## Installation

### Cloning the repo

```
git clone https://github.com/severij/calc-lang.git
cd calc-lang
```

### Build using The Haskell Tool Stack

#### Installing Stack

If you're using Linux, it's very likely that Haskell Tool Stack is packaged for
your Linux distribution. However, if it's not found, you can install it using
`curl` or `wget`:

```sh
curl -sSL https://get.haskellstack.org/ | sh
```
or
```sh
wget -qO- https://get.haskellstack.org/ | sh
```
Check out the [documentation](https://docs.haskellstack.org/en/stable/README/)
for more info on the usage of Stack.

#### Building and executing

Inside the `calc-lang` directory, run
```sh
stack build
stack exec calc-lang-exe
```

