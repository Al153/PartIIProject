package core.user

/**
  * Created by Al on 05/01/2018.
  *
  * Package object providing a single import for lots of syntax and methods
  */
package object dsl extends Commands
  with NodeSyntax
  with RelationSyntax
  with Opening
  with RepetitionOps
  with FindableSyntax
