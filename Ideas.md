## Idea:
- Based on: http://www.cl.cam.ac.uk/research/srg/netos/stud-projs/current/#QueryLayer
- Highly functional wrapper to provide typesafe graph operations on a relational database
- The premise is to provide the following:
    - An algebraic DSL in scala to allow construction of complex queries
    - A conversion from algebraic DSL to an core.intermediate tree of actions to be executed against the database
    - A Monadic results container type (to contain the result of a given expression), which represents the asynchronicity and ability to return a collection of results, along with the `view` that the reslts were generated against
        - This would likely be a monad transformer stack of the core.monads: Future (asynchronicity), State (to hold the view the query was executed with), Either (Typesafe and elegant core.error case handling), and List (a query may return a colleciton of results from the same view, eg "find all people who are related to John Smith")
    - A system of logical "`view`s" of the underlying database
        - A `view` is an immutable object representing the database at a logical point in time
            - For example, if we start with view `v` and execute an `insert (a -[r1]-> b)` and `insert (c -[r2]-> d)` against it, then we should end up with 3 total views, `v`, `v + (a -[r1]-> b)`, and `v + (c -[r2]-> d)`
            - Hence, `view`s essentially act like commits in git
        - we need to do garbage collection. when an in-memory `view` runs out of objects holding it, it should garbage collect itself from the SQL implementation - could be very tricky over a distributed system.
            - so distributed semantics are probably a major extension/out of current scope
    - An execution engine to compile the core.intermediate language to an SQL query that executes the graph level query represented against the internal database 
        - This should start out relatively trivial and if I have time left over, I might look at doing some simple optimisations, since we have some nice functional features to exploit
       
## Motivating example
- The above would hopefully make the scala code below a valid program:
```
  def main(): Unit = {

    /*
     * Define the schema we expect from the database
     * This might be loaded from a file or a library
     */

    object Schema {
      case class Actor(n: String) extends NodeDef
      case class Movie(n: String) extends NodeDef
      case object ActsIn extends RelationAttributes[Actor, Movie]

      /*
       * This is getting into category theory; we define a set of objects  by
       * a relation (morphism) to each member object from the singleton
       */

      case class Singleton() extends NodeDef
      case class LinkedToTomCruise(a: Pattern[Actor]) extends RelationalAttributes[Singleton, Actor]
    }

    import Schema._

    val tomCruise = Actor("Tom cruise")
    val point = Singleton()

    /*
     * Define some useful relations using the algebraic DSL
     */

    val coactor = RelationQuery[Actor, Actor] {
      (x: Pattern[Actor], y: Pattern[Actor]) => {
        val m = ?[Movie]
        (x ~ ActsIn ~> m) && (y ~ ActsIn ~> m)
      }
    }
    
    val getLinked = UnaryQuery[Actor] {
       (y: Pattern[Actor] ) => point ~ LinkedToTomCruise ~> y
    }
    
    /*
     * This is the interesting bit
     * a long, chained, expression that opens up a database instant, gets the default view, then finds all Actors with a chain of 4 unique coactor relationships to Tom Cruise
     * then filters the results for those whose names begin with "A" and then adds those passing the filter to the set
     * LinkedToTomCruise.
     * finally, from the same view, we collect all of the actors in the set LinkedToTomCruise usinga predefined query, and print their names
     */

    DBOpen("/path/to/sql/database", Schema)
      .rootView
      .find((coactor * 4)(tomCruise, !))
      .filter(actor => actor.name.beginsWith("A")
      .insert(actor => LinkedToTomCruise(actor))
      .find(getLinked(!))
      .foreach {
        case Actor(name) => println(name)
      }
  }
```

## Areas of Computer science
- Applies type theory + semantics (The API should be highly monadic)
- Concurrent and distributed systems
- Compilers
    - generating SQL transactions from graph oriented queries, and if I have time, some optimisations
    - Garbage collection of `database views`
- databases (both relational and graph)
- Some fairly hardcore functional programming
    - Immutability
    - type theory (core.monads)


## Risks/Drawbacks
- Too much?
    - Might be a little difficult to fully realise a releaseable system
    - But I think there is an achievable core project + extension idea
- Maybe a little hard to evaluate
- Potentially too far from Tim's expertise

## Plan of action
- Pick an underlying SQL library for JVM languages - 1-2 hours
- Build a views system (requires adding extra columns to SQL DB to manage the views an entry is a part of) - day or two of research
- Build a simple  reference counting garbage collector under the assumption of a single client (this is easy enough because due to immutability, the relation between views is a directed acyclic graph)
- Construct an appropriate monadic results container
- Construct an underlying core.intermediate representation as a step between the DSL and SQL
- Execution of core.intermediate representation of a query against a view to produce a result monad
- Construct an algebraic extensible DSL and translation to core.intermediate upon execution

## Extensions
- garbage collector under multiple clients (tough)
- Then correct distributed semantics, such as a way to merge two views to produce a new one
- Optimisations upon the core.intermediate representation, making queries as lazy as possible to allow cross query optimisation
- Store schemas as libraries within the database
-

## Evaluation Goals
- Something that works
- Makes a large set of transitive relation based queries more concise and clearer to write than the equivalent in SQL
- Try to limit performance slowdown due to the abstractions.
    - Endeavour to keep the functional database within an order of magnitude of speed compared to a pure SQL database evaluating the same queries
    - Ideally make it faster than this
    - If I get distributed semantics working, the system should be able to outperform raw SQL over a concurrent load, since the immutability means we can avoid the need for coarse grained locking
- Produce some nice operational semantics for the DSL and prove (via scala typesystem) that the database is typesafe?