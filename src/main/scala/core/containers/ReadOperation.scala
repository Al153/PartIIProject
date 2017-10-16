package core.containers

import view.View

/**
  * Created by Al on 09/10/2017.
  */
class ReadOperation[E, A](action: View => ConstrainedFuture[E, A]) extends Operation[E, A](view => action(view).map(x => (x, view)))

