# Lilypool
## NOTE: this branch has the old Lilyval architecture. It is being kept here
for posterity, in case I horribly destroy something in master.
## Introduction
An implementation of a subset of lilypond music notation for use in the autofinger project (name still under review).

Lilypool is a convenient way to enter musical passages into a database, for the
express purpose of accumulating data for fingering suggestions. Since the
primary purpose of the project is to format music into a matrix suitable for
passing to any machine learning algorithm, only very few musical attributes are
supported. These are:

* Pitch class
* Octave
* Duration
* Tuplets
* Finger
* String
* Harmonic
* Tempo
* Left Hand Pizziccato
* Time and key signatures
* Single staff polyphony

Lilypool is designed to compile both to lilypond for pdf rendering and human
viewing, as well as a matrix csv file for passing to a learning algorithm.
Note that several large classes of musical objects: slurs, dynamics and articulations, are not supported. 
Of course, they can both have bearing on choosing fingerings, but they are secondary concerns, whereas 
the supported attributes are primary concerns. Time and key signatures are of course non-essential to the 
fingering process as well, but since one of our primary goals is to compile to pdf, they _greatly_ enhance
human readability. 

## Syntax
A single note in Lilypool is written as follows (quantities enclosed in square brackets are optional):
```latex
PitchClass [-Octave][o for Harmonic][Duration][. for Dotted][+ for Left Hand Pizz][` for Grace Note]
```
Evidently, the only required information for a note is its pitch. If the octave or duration 
is omitted, Lilypool infers the same attribute as the preceeding note. If the harmonic, left hand pizz., 
or grace note indications are omitted, Lilypool naturally assumes that they are not present. 

Fingerings are written very simply: `FingerNumber[StringNumber]`. Again, should the `StringNumber` be omitted, Lilypool
assumes the previous holds throughout. 

### Special Indications
Lilypool supports three special indications. These are `#time`, which refers to time signature,
`#tempo`, which refers to tempo, and `#key`, which refers to key signature. `#time` and `#key` purely affect readability
of the generated pdf, whereas `#tempo` actually has te potential to affect fingering decisions. To write any of these in
a Lilypool source file, they must appear on their own line. `#time` is written:
```
#time Int/Int
```
`#tempo` is written
```
#tempo Duration=Int
```
`#key` is written
```
#key Note(M or m)
```
These annotations are carried through the music until they are overwritten by a new instance. 
### Full measures
In Lilypool, measures are a sequence of note values, separated by non-newline whitespace.
Unlike in Lilypond, measures _must_ be delimited by newlines. This is to increase readability of source files. 
Measures are statically checked at compile time that their notes do in fact add up to the indicated time signature. 
Should a free measure be desired, one can set the time signature to `#time 0/1` and write every note as a grace note.

Fingerings can be given on the line immediately following their corresponding measure, or omitted completely. Fingerings
for an entire measure are, like notes, a sequence of fingerings separate by whitespace. Should
one wish to provide a partial fingering for a measure, an underscore must be placed under the note whose fingering is to be
determined.

### Chords
Chords can have 2, 3, or 4 notes. People have written 5-note chords for the violin, but in practice, they are executed as
a grace-note chord followed by another chord. The syntax for them is a `[`, followed by 2, 3, or 4 `PitchClass[-Octave][Harmonic]` indications, followed by a `]`, followed by 
```[Duration][. for Dotted][+ for Left Hand Pizz][` for Grace Note]```. Fingerings for chords are also placed in `[]`. 
There can be no nested brackets.

### Tuplets
TODO

### Polyphony
TODO

### Comments
`%` begins a single-line comment. Lilypool has no multi-line comments.

## A small example
Here is a C Major scale
```
% This is a C major scale
#time 4/4
#key cM
#tempo 4=96
c-44 b-3 c-4 d
24    1   2   3

e f  g a
4  13 2 3

b c-5 d e
f g a b
c-6 d e f
g a b c-7
c b_6 a g
f e d c 
b-5 a g f
e d c b-4
a g f e
d c b-3 d-4
c1
```
