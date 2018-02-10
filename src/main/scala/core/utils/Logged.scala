package core.utils

import org.slf4j.{Logger, LoggerFactory}

/**
  * Created by Al on 10/02/2018.
  */
trait Logged {
  lazy val logger: Logger = LoggerFactory.getLogger(this.getClass)

}
