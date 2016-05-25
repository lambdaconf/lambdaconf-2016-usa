# The Easy-Peasy-Lemon-Squeezy, Statically-Typed, Purely Functional Programming Workshop for All!

This is the repository for the workshop *The Easy-Peasy-Lemon-Squeezy, Statically-Typed, Purely Functional Programming Workshop for All!*, at LambdaConf 2016 in Boulder, Colorado.

Here, you will find [slides](presentation.md), [source code](src/), and instructions on preparing your computer for the workshop!

# Preparation

First, you should download the contents of this repository to your computer. Then proceed to the following steps.

## 1. Install `npm`

**npm** is a package manager that can be used to install many Javascript and PureScript tools. You can download the package manager here:

 * [NodeJS](https://nodejs.org/download/)

## 2. Install PureScript

You should install the 0.8.5 release of the PureScript compiler and command-line tools.

```
npm install -g purescript
```

## 3. Install Pulp

Pulp is a really amazing build tool for PureScript that makes it easy to build projects.

```
npm install -g pulp
```

## 4. Install PureScript Dependencies

This installs the PureScript dependencies specified in `bower.json`.

```bash
bower install
```

## 5. Compile with PureScript

This runs the build process, which will generate a `game.js` in the directory.

```bash
pulp build
pulp browserify > game.js
```

## 8. Open HTML to Run Game

On Mac OS X:

```
open game.html
```

On other platforms, browse the folder and manually open the file `game.html`.
