Prolog
======

[![Build Status](https://www.travis-ci.org/edadma/prolog.svg?branch=master)](https://www.travis-ci.org/edadma/prolog)
[![Build status](https://ci.appveyor.com/api/projects/status/h5b23n2vd0k4oh9q/branch/master?svg=true)](https://ci.appveyor.com/project/edadma/prolog/branch/master)
[![Coverage Status](https://coveralls.io/repos/github/edadma/prolog/badge.svg?branch=master)](https://coveralls.io/github/edadma/prolog?branch=master)
[![License](https://img.shields.io/badge/license-ISC-blue.svg)](https://github.com/edadma/prolog/blob/master/LICENSE)
[![Version](https://img.shields.io/badge/latest_release-v0.1-orange.svg)](https://github.com/edadma/prolog/releases/tag/v0.1)

*Prolog* is a processor for Prolog language written in [Scala](http://scala-lang.org).  It is a work in progress.  ISO compliance plus support for a string type is the goal.


Examples
--------

### Family Tree

Here's an example which defines a family tree along with various types of family relationships.

```
female( anne ).
female( rosie ).
female( emma ).
female( olivia ).
female( mia ).

male( randy ).
male( don ).
male( liam ).
male( logan ).
male( aiden ).

parent( don, randy ).
parent( don, anne ).
parent( rosie, randy ).
parent( rosie, anne ).
parent( liam, don ).
parent( olivia, don ).
parent( liam, mia ).
parent( olivia, mia ).
parent( emma, rosie ).
parent( logan, rosie ).
parent( emma, aiden ).
parent( logan, aiden ).

relation( X, Y ) :- ancestor( A, X ), ancestor( A, Y ), X \= Y.
ancestor( X, Y ) :- parent( X, Y ) ; parent( X, P ), ancestor( P, Y ).

mother( X, Y ) :- female( X ), parent( X, Y ).
father( X, Y ) :- male( X ), parent( X, Y ).
daughter( X, Y ) :- female( X ), parent( Y, X ).
son( X, Y ) :- male( X ), parent( Y, X ).
siblings( X, Y ) :- parent( P, X ), parent( P, Y ), X \= Y.
full_siblings( A, B ) :-
  parent( F, A ), parent( F, B ),
  parent( M, A ), parent( M, B ),
  A \= B, F \= M.
sister( X, Y ) :- female( X ), siblings( Y, X ).
brother( X, Y ) :- male( X ), siblings( Y, X ).
uncle( U, N ) :- male( U ), siblings( U, P ), parent( P, N ).
aunt( A, N ) :- female( A ), siblings( A, P ), parent( P, N ).
grandparent( X, Y ) :- parent( X, P ), parent( P, Y ).
grandmother( X, Y ) :- female( X ), grandparent( X, Y ).
grandfather( X, Y ) :- male( X ), grandparent( X, Y ).
```

This example is contained in the repo as `examples/family_tree`.  Run the REPL (repeat-evaluate-print-loop) and type

    ;l examples/family_tree
    
to load the example.  To get a list of all the persons in the family tree who are fathers, type

    father( X, _ )*
    
#### Output

X = don

X = liam

X = logan
 
### Library

This example program shows how to 

```scala
```

This program prints

```
```

### Executable

This next example shows how to use *Prolog* as an executable from the command line.

```bash
```

The above command prints



Usage
-----

### Library

Use the following definition to use Prolog in your Maven project:

```xml
<repository>
  <id>hyperreal</id>
  <url>https://dl.bintray.com/edadma/maven</url>
</repository>

<dependency>
  <groupId>xyz.hyperreal</groupId>
  <artifactId>prolog</artifactId>
  <version>0.1</version>
</dependency>
```

Add the following to your `build.sbt` file to use Prolog in your SBT project:

```sbt
resolvers += "Hyperreal Repository" at "https://dl.bintray.com/edadma/maven"

libraryDependencies += "xyz.hyperreal" %% "prolog" % "0.1"
```

### Executable

An executable can be downloaded from [here](https://dl.bintray.com/edadma/generic/prolog-0.1.jar). *You do not need* the Scala library for it to work because the JAR already contains all dependencies. You just need Java 8+ installed.

Run it as a normal Java executable JAR with the command `java -jar prolog-0.1.jar <template>` in the folder where you downloaded the file, where *template* is the name of the template file to be rendered.

Building
--------

### Requirements

- Java 11
- SBT 1.2.7+
- Scala 2.12.8+

### Clone and Assemble Executable

```bash
git clone git://github.com/edadma/prolog.git
cd prolog
sbt assembly
```

The command `sbt assembly` also runs all the unit tests.


License
-------

ISC © 2018 Edward A. Maxedon, Sr.