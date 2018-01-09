package core.user.containers

import core.user.dsl.View

import scala.concurrent.ExecutionContext

/**
  * Created by Al on 09/10/2017.
  * A read operation is an operation that does not create any more views, so the [[ConstrainedFuture]]
  * can ignore the view parameter as it is trivial
  */
class ReadOperation[E, A](action: View => ConstrainedFuture[E, A])(implicit ec: ExecutionContext)
  extends Operation[E, A](view => action(view).map(x => (x, view)))

