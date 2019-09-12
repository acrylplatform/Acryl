package com.acrylplatform.lang.directives.values

import com.acrylplatform.lang.directives._

abstract class DirectiveValue(val text: String, val id: Int) {
  protected def resolveKey[V](implicit k: DirectiveKey { type Value = V }) = k
  def key: DirectiveKey

  lazy val unparsed: String = s"${DirectiveParser.start} ${key.text} $text ${DirectiveParser.end}"
  val value: Any
}
