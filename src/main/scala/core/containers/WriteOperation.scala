package core.containers

import view.View

import scala.concurrent.ExecutionContext

/**
  * Created by Al on 09/10/2017.
  */
class WriteOperation[E](action: View => ConstrainedFuture[E, View])(implicit ec: ExecutionContext) extends Operation[E, Unit](view => action(view).map(v => ((), v)))