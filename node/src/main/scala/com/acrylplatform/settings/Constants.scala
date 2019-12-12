package com.acrylplatform.settings

import com.acrylplatform.Version
import com.acrylplatform.transaction.TransactionParsers
import com.acrylplatform.utils.ScorexLogging

/**
  * System constants here.
  */
object Constants extends ScorexLogging {
  val ApplicationName = "acryl"
  val AgentName       = s"Acryl v${Version.VersionString}"

  val UnitsInWave = 100000000L
  val TotalAcryl  = 100000000L

  lazy val TransactionNames: Map[Byte, String] =
    TransactionParsers.all.map {
      case ((typeId, _), builder) =>
        val txName =
          builder.getClass.getSimpleName.init
            .replace("V1", "")
            .replace("V2", "")

        typeId -> txName
    }
}
