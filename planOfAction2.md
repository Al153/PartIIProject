## Plan of action round 2
### Pick an underlying SQL library for JVM languages - *1-2 hours*
    
- likely to be `http://scalikejdbc.org/`
### Draw up interfaces for major components - *3 - 4 hours*
- View (the representation in memory as scala objects)
    - need to be able to apply DSL queries
    - Apply diffs
    - eventually a way to set values in a mutable views table, so that views are persistent (changes made now can be read the next time the database is accessed)
- Query components
    - ie the things that we read and write to the database
- (Diff - very simple class, consists of a list of PUT queries and a list of delete queries)
- Results container
    - Needs monadic methods (`flatMap` - scala for `bind`, map, point)
    - and other nice combinators (foreach, recover - recover core.error cases, recoverWith)
- Return types that are stored in the results container - can be simple things like generic classes, but some queries may want to return paths through the database, or other non-trivial datatypes
- A set of DSL syntax we want to allow

### Come up with motivating examples - *3-4 hours*
- Write a few dummy programs which show what we want the DSL to look like
    
### Build a views system  - *Several days to a week*
- (requires adding extra columns to SQL DB to manage the views an entry is a part of)
- This also fits in to how we want to define schema for objects in the database
- Build a simple  reference counting garbage collector under the assumption of a single client (this is easy enough because, due to immutability, the relation between views is a directed acyclic graph)
- My initial thinking is:
    - We tag each row in each user accessible table with the ID of the view that it is part of
    - Views then store a list of their dependencies
    - Views are stored in a special database table
    - When we read from the database, views aren't changed
    - If we insert new relations into a view, `A` then we add a new view `B` which is dependent on `A`, which contains the newly added relations
    - This presents an issue if we want to remove relation `r` from view `A` to produce a new view `C`. This would involve copying a large proportion of the information in `A` to `C`
    - A better solution might be store changes as commits, and have views as datastructures which show which commits they hold:
        - every time we make a write query to the database, the relations written belong to a `Commit` object (large writes may be chunked)
        - each row in the table contains the key of the `Commit` it belongs to
        - We store each view as an in-memory imutable set of `Commit`s, using an inbuilt scala `Set`
        - When we write a commit, we make a copy of the current view, but with the new `Commit` index added (this is fast)
        - when we make a query that deletes relations, we look up the commits that the deleted relations belong to, then subtract this set from the original view, and write new copies of those commits, without the deleted relations, to the database  
        - Eg View `A` contains commits `{C1, C2, C3}`
        - when we execute `A + a -(r)-> b`, we construct view `B` which is `{C1, C2, C3, C4}`, where `C4` contains the new relation
        - When we execute `B - {c -(r2)-> b, a -(r3)-> c}`, we look up the commits containing the relations to remove, which might be `C1`, `C2`, so we construct new copies: `C1'`, `C2'`, which remove those relations.
        - Finally we construct view `C` which is `{C1', C2', C3, C4}`
    
### Construct an appropriate monadic results container according to the above interface - *a day, plus continuous adding of helper methods*
- This could be done with a big monad transformer stack or be built in a more purposed style
- I've given the first a go, and it leaves me fighting with the scala type system slightly, but the issues I'm having seem to be something that can be stack-overflow-ed for help

### Construct an underlying core.intermediate representation as a step between the DSL and SQL - *A day to build the raw Algebraic datatype*
- This should act a bit like a bytecode
- Relatively easy to build in scala as an algebraic data type
- This would consist of write operations 
    - These are fairly simple: (PUT (and DELETE? - this is a little harder to do in an immutable views system)) which have fairly simple structure (i.e. PUT relation of type A from node B to Node C)
- And more complex read operations
    - We want some basic pattern matching, e.g. find Node A of type B with parameter "name" == "John"
    - We want to be able to follow relations (i.e. execute joins)
    - we want to do transitive relational queries
        - i.e. various breadth first searches, such as shortest path, all nodes within n relations from a start node
        - A task is to come up with a set of useful queries like this, which increase the expressive power of the query layer, but which are still easy enough to implement
        
### Execution of core.intermediate representation of a query against a view to produce a result monad - *A week, Open to extensions*
- This should be a translation of the query into a query on the underlying SQL database, with respect to the current view
- For read operations, which don't affect the database (i.e. they don't create any new views), this is fairly easy. We just make a query, and filter by the appropriate view
- Writes a little more complex:
    - Need to create a new commit entry
        
### Construct an algebraic extensible DSL and translation to core.intermediate upon execution - *open ended timescale*
- This will probably make heavy use of the scala "pimp-my-library" pattern (`https://coderwall.com/p/k_1jzw/scala-s-pimp-my-library-pattern-example`)
- We want to make it easy for users to build databases with schema matching their use cases.
- I propose to do this by letting users define simple `case class`es and then also defining implicit conversions to objects that do the things like pattern matching and which define the database schema
- Build some nice syntax to allow users to algebraically construct

### Testing of components
- This is a continuous processs
