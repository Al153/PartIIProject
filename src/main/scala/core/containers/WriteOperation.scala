package core.containers

import view.View

/**
  * Created by Al on 09/10/2017.
  */
class WriteOperation[E](action: View => ConstrainedFuture[E, View]) extends Operation[E, Unit](view => action(view).map(v => ((), v)))