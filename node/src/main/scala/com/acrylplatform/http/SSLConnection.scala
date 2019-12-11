package com.acrylplatform.http

import java.io.{FileInputStream, InputStream}
import java.security.{KeyStore, SecureRandom}

import javax.net.ssl.{KeyManagerFactory, SSLContext, TrustManagerFactory}
import akka.http.scaladsl.{ConnectionContext, HttpsConnectionContext}
import com.acrylplatform.settings.RestAPISettings

object SSLConnection {
  def getSSL(settings: RestAPISettings): Either[String, HttpsConnectionContext] =
    if (settings.https && settings.httpsPassword != "" && settings.certificateFile != "") {
      val password: Array[Char] = settings.httpsPassword.toCharArray
      val ks: KeyStore          = KeyStore.getInstance("PKCS12")
      val keystore: InputStream = new FileInputStream(settings.certificateFile)

      require(keystore != null, "Keystore required!")
      ks.load(keystore, password)

      val keyManagerFactory: KeyManagerFactory = KeyManagerFactory.getInstance("SunX509")
      keyManagerFactory.init(ks, password)

      val tmf: TrustManagerFactory = TrustManagerFactory.getInstance("SunX509")
      tmf.init(ks)

      val sslContext: SSLContext = SSLContext.getInstance("TLS")
      sslContext.init(keyManagerFactory.getKeyManagers, tmf.getTrustManagers, new SecureRandom)
      Right(ConnectionContext.https(sslContext))
    } else {
      Left("HTTPS disable")
    }
}
