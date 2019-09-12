package com.acrylplatform.lang

import com.acrylplatform.lang.v1.BaseGlobal

package object hacks {
  private[lang] val Global: BaseGlobal = com.acrylplatform.lang.Global // Hack for IDEA
}
