Domain model, business logic and JSON codecs are in [sirius-takehome.scala](sirius-takehome.scala). Comments explain top-level concepts; `TODO`s are places with issues or where the design could be improved.

Tests are in [sirius-takehome.test.scala](sirius-takehome.test.scala).

I used [scala-cli](https://scala-cli.virtuslab.org/) to declare dependencies. You can run the tests via:

    $ scala-cli test .
    
which should output something like:
```
MyTests:
  + Query: Howard Stern 0.017s
  + Query: Howard Stern 24/7 0.001s
  + Query: SiriusXM NFL Radio 0.001s
  + Query: Sports 0.0s
  + Query: Elton John 0.001s
```
