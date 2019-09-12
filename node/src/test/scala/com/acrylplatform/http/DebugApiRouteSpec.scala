package com.acrylplatform.http

import com.acrylplatform.api.http.ApiError.ApiKeyNotValid
import com.acrylplatform.settings.AcrylSettings
import com.acrylplatform.{NTPTime, TestWallet}

class DebugApiRouteSpec extends RouteSpec("/debug") with RestAPISettingsHelper with TestWallet with NTPTime {
  private val sampleConfig  = com.typesafe.config.ConfigFactory.load()
  private val acrylSettings = AcrylSettings.fromRootConfig(sampleConfig)
  private val configObject  = sampleConfig.root()
  private val route =
    DebugApiRoute(acrylSettings, ntpTime, null, null, null, null, null, null, null, null, null, null, null, null, null, configObject).route

  routePath("/configInfo") - {
    "requires api-key header" in {
      Get(routePath("/configInfo?full=true")) ~> route should produce(ApiKeyNotValid)
      Get(routePath("/configInfo?full=false")) ~> route should produce(ApiKeyNotValid)
    }
  }
}
