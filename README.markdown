# Shopping-Lisp

Lisp reimplementation of the backend of the android shopping list app.

For fun, a lisp equivalent to [GroundApps/ShoppingList_Backend](https://github.com/GroundApps/ShoppingList_Backend) which serves as the backend for [this app](https://github.com/GroundApps/ShoppingList).

## Usage
With the default configuration, the server can be run with
```lisp
(ql:quickload :shopping-lisp)
(shopping-lisp:run)
```

In production, you'll want to set `ENV=production` so the [`nest`](https://gitlab.com/knttl/nest) framework uses production settings.  At the moment, `nest` is still under heavy development, so YMMV, but it works well enough for now.  I recommend a reverse proxy, since most lisp web servers don't receive the same sort of security scrutiny one would want with a public-facing server.

## Installation

You will need two libraries that aren't in quicklisp to run this, because I've written them and don't feel like they're mature enough to publish for general use, yet.

* https://gitlab.com/knttl/nest
* https://github.com/fisxoj/validate

I think I'm going to work on a Docker image at some point, for now it's just a standard lisp package.

## Configuration
To configure the backend, all you need to do is pick a password for the server.  The development password is just `password`, and you can see the hash of that stored in `config/development.sexp`.  When you run the app in production, you'll want to set the value of `:auth-hash` in `config/production.sexp`, which is what will be used when the server is run with the environment variable `ENV` set to `production`.

To generated that hash
```lisp
(ql:quickload :cl-pass)
(cl-pass:hash "your_cool_password")
```
Nota bene: the configuration files are read as lisp expressions and so the hash should be in double quotes like any lisp string.

## Author

* Matt Novenstern (fisxoj@gmail.com)

## Copyright

Copyright (c) 2016 Matt Novenstern (fisxoj@gmail.com)
