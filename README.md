# Anagrammer

A quick tool to find line-anagrams (excluding those where all of the words are the same) in some ascii file, just to have a play with profiling and whatnot. It takes one command line argument - the text file to read - and errors if this is not provided.

Examples of anagrams (all matches are grouped):

```
'Tis so, indeed.
Is't so, indeed.

A thing
Hang it!

No further, sir.
For his return.
```

Examples of invalid matches (only re-arrangement of words rather than of letters):

```
Hello there sir.
Sir, hello there

He played bagpipes.
Bagpipes he played.
```

"shakespeare.txt" contains an example source text, and "result.txt" the output from running the program on this source text.

# Timings

Currently, a Basic and MutableHash implementation exists. Times taken (average of 10 runs on a 4Ghz Intel i7 iMac, 8GB 1600Mhz DDR3) on the "shakespeare.txt" source text are:

| Implementation | Created By | Time taken |
|----------------|------------|------------|
| Basic          | jsdw       | 0.908s     |
| MutableHash    | jsdw       | 1.410s     |

# Installation

Use the `stack` tool to install, so the full steps will be something like:

```
git clone https://github.com/jsdw/hs-anagrammer
cd hs-anagrammer
stack install
```