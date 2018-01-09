package core.user.containers

import core.user.dsl.View

import scala.concurrent.ExecutionContext

/**
  * Created by Al on 09/10/2017.
  * A write operation is an Operation[Unit], so it can be constructed from a View => [[ConstrainedFuture]][View]
  */
class WriteOperation[E](action: View => ConstrainedFuture[E, View])(implicit ec: ExecutionContext)
  extends Operation[E, Unit](view => action(view).map(v => ((), v)))