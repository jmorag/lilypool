# Lilypool
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
* Articulation (staccato, tenuto, accent, slurs)
* Time and key signatures
* Single staff polyphony

Lilypool is designed to compile both to lilypond for pdf rendering and human
viewing, as well as a matrix csv file for passing to a learning algorithm.
