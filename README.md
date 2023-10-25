# Assignment 1: Recursion, Datatypes and Higher Order Functions (350 points)

## Overview

The overall objective of this assignment is to get some
experience using the *core* features of functional programming,
namely, Recursion, Datatypes and Higher-Order functions.

The assignment is in the following files that you will modify

- [List.hs](/src/CSE230/List.hs)
- [Shapes.hs](/src/CSE230/Shapes.hs)
- [Doc.hs](/src/CSE230/Doc.hs)
- [Directory.hs](/src/CSE230/Directory.hs)

Finally, `Test.hs` has some sample tests to be used
to check your assignments before submitting.

- [test/Test.hs](/test/Test.hs)

You should **only modify** the parts of the files which say:

```haskell
error "fill this in"
```

with suitable Haskell implementations.

**You are free to write and use any helper functions.**

## Instructions

### Assignment Testing and Evaluation

Most of the points, will be awarded automatically, by
**evaluating your functions against a given test suite**.

[Tests.hs](/tests/Test.hs) contains a very small suite
of tests which gives you a flavor of of these tests.
When you run

```shell
$ make test
```

Your last lines should have

```
All N tests passed (...)
OVERALL SCORE = ... / ...
```

**or**

```
K out of N tests failed
OVERALL SCORE = ... / ...
```

**If your output does not have one of the above your code will receive a zero**

If for some problem, you cannot get the code to compile,
leave it as is with the `error "fill me in"` with your
partial solution enclosed below as a comment.

The other lines will give you a readout for each test.
You are encouraged to try to understand the testing code,
but you will not be graded on this.

### Submission Instructions

To submit your code, just do:

```bash
$ make turnin
```

If you are working in a group, make sure to update the file `COLLABORATORS.md`
with your group members, but **each person must submit on their own**.

## Problem 1: Lists (57 points)

In this problem, you can use only the following two library functions **on lists**: `(++)` and `length`.
(Feel free to use any library function that does not operate on lists.)

### Cloning (5 points)

Fill in the implementation of `clone` such that `clone n x` returns
a list of `n` copies of `x`. When you are done, you should see the
following behavior:

```haskell
>>> clone 5 'a'
"aaaaa"

>>> clone 3 "cat"
["cat","cat","cat"]
```

### Padding (14 points)

Fill in the implementation of `pad` such that `pad dir n x ys`
"pads" the list `ys` with as many copies of `x` as needed to make
it exactly of size `n`. The `dir` parameter specifies whether the
copies of `x` should be added at the beginning (`DirL`) or at the
end (`DirR`).

When you are done, you should see the following behavior:

```haskell
>>> pad DirL 10 0 [1,2,3,4,5]
[0,0,0,0,0,1,2,3,4,5]

>>> pad DirR 10 0 [1,2,3,4,5]
[1,2,3,4,5,0,0,0,0,0]

>>> pad DirL 3 0 [1,2,3,4,5]
[1,2,3,4,5]

>>> pad DirR 3 0 [1,2,3,4,5]
[1,2,3,4,5]
```

### Sub-Sequence (10 points)

Fill in the definition of `isSubsequence` such that
`isSubSequence s1 s2` returns True if `s1` can be
obtained by *deleting* some elements of `s2`.

When you are done, you should see the following behavior:

```haskell
>>> isSubSequence "cat" "dog"
False

>>> isSubSequence "cat" "craptasticdog"
True
```

### Maximum (8 points)

Fill in the implementation of `maximum` so that `maximum d xs`
returns the largest of `d:xs`.

When you are done, you should see the following behavior:

```haskell
>>> maximum 99 []
99

>>> maximum 99 [90, 100, 200, 52]
200
```

### Intersperse (8 points)

Fill in the definition of `intersp` such that
`intersp s [x1,x2,...,xn]` returns the list
`[s, x1, s, x2, s, ..., xn, s]`.

When you are done, you should see the following behavior:

```haskell
>>> intersp '|' "chewbacca"
"|c|h|e|w|b|a|c|c|a|"

>>> intersp "yo!" ["which", "way", "is", "the", "park"]
["yo!","which","yo!","way","yo!","is","yo!","the","yo!","park","yo!"]
```

### `iter` (12 points)

Fill in the definition of `iter` such that `iter n f x`
returns the result of calling `f` on `x` exactly `n` times, e.g.

- `iter 3 f x` returns `f (f (f x))`, and
- `iter 5 f x` returns `f (f (f (f (f x))))`.

When you are done you should get the following behavior:

```haskell
>>> iter 10 (\x -> 2 * x) 1
1024
```

## Problem 2: Shapes (105 points)

From this problem on, you are allowed to use any library functions you want.
You can also use functions you have implemented in previous problems.
However, please do not change the `import` statements at the top of the file:
we have already imported all the functions you need.

### Rainbow (15 points)

Fill in the implementation of `rainbow` so that when you
are done, running

```haskell
>>> mkRainbow
```

creates a file `img/rainbow.png` that looks identical to

![Rainbow](/out/rainbow.png)

**HINT:** Read the documentation for `overlay` and `circle` from
the [`Graphics.Htdp` library](http://hackage.haskell.org/package/htdp-image-1.1.0.0/docs/Graphics-Htdp.html).

### ChessBoard using `iter` (20 points)

Fill in the implementation of `chessBoard2` so that when you
are done, running

```haskell
>>> mkChess2
```

creates a file `img/chess2.png` that looks identical to

![Chess Board](/out/chess2.png)

**HINT:** Make sure you understand the API in `chessBoard1`.

### Triangle using recursion (15 points)

Fill in the implementation of `triRec` so that when you
are done, running

```haskell
>>> mkTriangle1
```

creates a file `img/triangle1.png` that looks identical to

![Chess Board](/out/triangle1.png)

**HINT:** You may find the `Graphics.Htdp` functions `above` and `beside` useful.

### Triangle using `iter` (25 points)

Fill in the implementation of `sierpinskiTriangle2` so that when you
are done, running

```haskell
>>> mkTriangle2
```

creates a file `img/triangle2.png` that looks identical to

![Chess Board](/out/triangle2.png)

**HINT:** Make sure you understand the relation between `iter` and recursion.

### Carpet with `iter` (30 points)

Fill in the implementation of `sierpinskiCarpet` so that when you
are done, running

```haskell
>>> mkCarpet
```

creates a file `img/carpet.png` that looks identical to

![Chess Board](/out/carpet.png)

## Problem 3: Documents (84 points)

For this problem you will write a simple document layout engine, that allows the pretty printing of nested documents.

Thus, a document is a list of lines, each of which is a string. For example, the document

```
a
aa
aaa
aaaa
```

is represented as

```haskell
>>> D ["a", "aa", "aaa", "aaaa"]
```

We have also provided implementations of methods for computing

* the `height` of a document (the number of lines)
* the `width` of a document (the maximum number of characters in a line)

### Vertical Concatenation aligned at Left (20 points)

Fill in the implementation of `vcatL` such that `vcatL d1 d2`
*vertically concatenates* the documents `d1` and `d2` aligning
their *left* sides.

When you are done, you should see the following behavior:

```haskell
>>> (doc "cat") `vcatL` (doc "horse") `vcatL` (doc "mongoose")
cat
horse
mongoose
```

### Vertical Concatenation aligned at Right (22 points)

Fill in the implementation of `vcatR` such that `vcatR d1 d2`
*vertically concatenates* the documents `d1` and `d2` aligning
their *right* sides.

When you are done, you should see the following behavior:

```haskell
>>> (doc "cat") `vcatR` (doc "horse") `vcatR` (doc "mongoose")
     cat
   horse
mongoose
```



### Horizontal Concatenation aligned at Top (22 points)

Fill in the implementation of `hcatT` such that `hcatT d1 d2`
*horizontally concatenates* the documents `d1` and `d2` aligning
their *top* sides.


Suppose you have the following `Doc` values:

```haskell
>>> aDoc
a
aaa
aaaaa
```

```haskell
>>> bDoc
b
bbb
```

```haskell
>>> lineDoc
<----- HERE
```

When you are done with `hcatT`, you should see the following behavior:

```haskell
>>> hcatT aDoc lineDoc
a    <----- HERE
aaa
aaaaa
```

and

```haskell
>>> hcatT aDoc bDoc
a    b
aaa  bbb
aaaaa
```


### Horizontal Concatenation aligned at Bottom (20 points)

Fill in the implementation of `hcatB` such that `hcatB d1 d2`
*horizontally concatenates* the documents `d1` and `d2` aligning
their *bottom* sides.


When you are done you should see the following behavior:

```haskell
>>> hcatB aDoc lineDoc
a
aaa
aaaaa<----- HERE
```

and

```haskell
>>> hcatB aDoc bDoc
a
aaa  b
aaaaabbb
```

## Problem 4: `htree` (104 points)

Finally, we will use `Doc` to build a command-line tool
called `htree` which does two tasks.

*1. Showing the Sub-Directories*

At the shell, executing

```
$ htree -ls src
```

produces the following tree-representation of the `src`
directory (of this repo).

```
src
├── CSE230
│   ├── Directory.hs
│   ├── Doc.hs
│   ├── Graphics.hs
│   ├── List.hs
│   └── Shapes.hs
├── Htdp
│   ├── Combinator.hs
│   ├── Data
│   │   └── Image.hs
│   ├── README.md
│   └── Shape.hs
└── Main.hs
```

*2. Finding Files Matching a Pattern*

At the shell, executing

```
$ htree -find src .hs
```

prints out the list of all the files (recursively) inside `src`
that match the substring `.hs`:

```
src/CSE230/Directory.hs
src/CSE230/Doc.hs
src/CSE230/Graphics.hs
src/CSE230/List.hs
src/CSE230/Shapes.hs
src/Htdp/Combinator.hs
src/Htdp/Data/Image.hs
src/Htdp/Shape.hs
src/Main.hs
```

### Directories

We represent a *directory* via a datatype

```haskell
data Dir a
    = Fil a             -- ^ A single file named `a`
    | Sub a [Dir a]     -- ^ A sub-directory name `a` with contents `[Dir a]`
```

For example, the files in the `src` directory can be represented as:

```haskell
srcDir :: Dir FilePath
srcDir = Sub "src"
         [ Sub "CSE230" [ Fil "Directory.hs"
                        , Fil "Doc.hs"
                        , Fil "Graphics.hs"
                        , Fil "List.hs"
                        , Fil "Shapes.hs"
                        ]
         , Sub "Htdp" [ Fil "Combinator.hs"
                      , Sub "Data" [ Fil "Image.hs" ]
                      , Fil "README.md"
                      , Fil "Shape.hs"
                      ]
         , Fil "Main.hs"
         ]
```

**HINT:** Take a look at the functions that `Directory.hs` imports from `System.FilePath` and `System.Directory`;
they will help you with some tasks in this problem.

### `dirDoc` (36 points)

Fill in the implementation of `dirDoc` that converts a `Dir FilePath`,
i.e. a directory where each name is a `FilePath` (i.e. `String`) into
a `Doc` value (from the previous problem). Your code should only use
the *exported* functions of the `Doc` module, i.e.

* the `doc` constructor, and
* the combinators `hcatT`, `hcatB`, `vcatL` and `vcatR`.

**HINT:** You may find `dash`, `stile` , `angle` and `bar` useful.

When you are done, you should see the following behavior:

```haskell
>>> dirDoc srcDir
src
├── CSE230
│   ├── Directory.hs
│   ├── Doc.hs
│   ├── Graphics.hs
│   ├── List.hs
│   └── Shapes.hs
├── Htdp
│   ├── Combinator.hs
│   ├── Data
│   │   └── Image.hs
│   ├── README.md
│   └── Shape.hs
├── Htdp.hs
└── Main.hs
```

```haskell
>>> dirDoc example
.
├── COLLABORATORS.md
├── LICENSE
├── Makefile
├── README.md
├── cse230-tree.cabal
├── out
│   ├── carpet.png
│   ├── chess1.png
│   ├── chess2.png
│   ├── rainbow.png
│   ├── triangle1.png
│   └── triangle2.png
├── src
│   ├── CSE230
│   │   ├── Directory.hs
│   │   ├── Doc.hs
│   │   ├── Graphics.hs
│   │   ├── List.hs
│   │   └── Shapes.hs
│   ├── Htdp
│   │   ├── Combinator.hs
│   │   ├── Data
│   │   │   └── Image.hs
│   │   ├── README.md
│   │   └── Shape.hs
│   ├── Htdp.hs
│   └── Main.hs
└── stack.yaml
```

### `allFiles` (10 points)

Understand and use `foldDir` to fill in the implementation
of `allFiles dir` which returns a list of *all the files*
in the directory `dir`.

When you are done, you should see the following behavior:

```haskell
>>> allFiles example
["COLLABORATORS.md","LICENSE","Makefile","README.md","cse230-tree.cabal","carpet.png","chess1.png","chess2.png","rainbow.png","triangle1.png","triangle2.png","Directory.hs","Doc.hs","Graphics.hs","List.hs","Shapes.hs","Combinator.hs","Image.hs","README.md","Shape.hs","Htdp.hs","Main.hs""stack.yaml"]
```


### `allDirs` (10 points)

Understand and use `foldDir` to fill in the implementation of
`allDirs dir` which returns a list of *all the sub-directories*
in the directory `dir`.

When you are done, you should see the following behavior:

```haskell
>>> allDirs example
[".","out","src","CSE230","Htdp","Data"]
```


### `findFiles` (16 points)

Understand and use `foldDir` to fill in the implementation of
`findFiles sub dir` which returns a list of *all the files*
in the directory `dir` such that `sub` is a *subsequence*
of the files' names.

When you are done, you should see the following behavior:

```haskell
>>> findFiles ".hs" example
["./src/CSE230/Directory.hs","./src/CSE230/Doc.hs","./src/CSE230/Graphics.hs","./src/CSE230/List.hs","./src/CSE230/Shapes.hs","./src/CSE230/Directory.hs","./src/Htdp/Combinator.hs","./src/htdp/Data/Image.hs","./src/Htdp/README.md","./src/htdp/Shape.hs","./src/Htdp.hs","./src/Main.hs"]

```

### `build` (32 points)

Finally, complete the implementation of the function
`build path` that recursively traverses the filesystem
starting at `path` to build the `Dir FilePath` object
describing the filesystem's contents at `path`. (You
can ignore complexities like symbolic links etc.)

When you are done, you should see the following
behavior (assuming you have not added extra files
in your `src/` directory!)

```haskell
>>> build "src"
Sub "src" [Sub "CSE230" [Fil "Directory.hs", Fil "Doc.hs", Fil "Graphics.hs", Fil "List.hs", Fil "Shapes.hs"], Sub "Htdp" [Fil "Combinator.hs", Sub "Data" [Fil "Image.hs"], Fil "README.md", Fil "Shape.hs"], Fil "Htdp.hs", Fil "Main.hs"]
```

Notice that the directories and files are listed in lexicographic order!

Finally, at this point, `stack install` should build and
install a standalone executable `htree` that you can then
run as described above:

```
$ htree -ls   src
```

and

```
$ htree -find src .hs
```
