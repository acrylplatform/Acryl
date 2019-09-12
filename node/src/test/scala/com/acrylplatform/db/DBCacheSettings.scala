package com.acrylplatform.db
import com.typesafe.config.ConfigFactory
import com.acrylplatform.settings.AcrylSettings

trait DBCacheSettings {
  lazy val dbSettings = AcrylSettings.fromRootConfig(ConfigFactory.load()).dbSettings
  lazy val maxCacheSize: Int = dbSettings.maxCacheSize
}
