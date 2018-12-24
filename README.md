Prolog
======

[![Build Status](https://www.travis-ci.org/edadma/prolog.svg?branch=master)](https://www.travis-ci.org/edadma/prolog)
[![Build status](https://ci.appveyor.com/api/projects/status/h5b23n2vd0k4oh9q/branch/master?svg=true)](https://ci.appveyor.com/project/edadma/prolog/branch/master)
[![Coverage Status](https://coveralls.io/repos/github/edadma/prolog/badge.svg?branch=master)](https://coveralls.io/github/edadma/prolog?branch=master)
[![License](https://img.shields.io/badge/license-ISC-blue.svg)](https://github.com/edadma/prolog/blob/master/LICENSE)
[![Version](https://img.shields.io/badge/latest_release-v0.4.22-orange.svg)](https://github.com/edadma/prolog/releases/tag/v0.4.22)

*Prolog* is a processor for Prolog language written in [Scala](http://scala-lang.org).  It is a work in progress.  ISO compliance plus support for a string type is the goal.


Examples
--------

### Family Tree

Here's an example 

```
```

### Output

```
```

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
  <version>0.4.22</version>
</dependency>
```

Add the following to your `build.sbt` file to use Prolog in your SBT project:

```sbt
resolvers += "Hyperreal Repository" at "https://dl.bintray.com/edadma/maven"

libraryDependencies += "xyz.hyperreal" %% "prolog" % "0.1"
```

### Executable

An executable can be downloaded from [here](https://dl.bintray.com/edadma/generic/prolog-0.4.22.jar). *You do not need* the Scala library for it to work because the JAR already contains all dependencies. You just need Java 8+ installed.

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