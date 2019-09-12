package com.acrylplatform.db

import java.nio.file.Files

import com.typesafe.config.ConfigFactory
import com.acrylplatform.account.Address
import com.acrylplatform.database.LevelDBWriter
import com.acrylplatform.history.Domain
import com.acrylplatform.settings.{FunctionalitySettings, AcrylSettings, loadConfig}
import com.acrylplatform.state.BlockchainUpdaterImpl
import com.acrylplatform.transaction.Asset
import com.acrylplatform.utils.Implicits.SubjectOps
import com.acrylplatform.{NTPTime, TestHelpers}
import monix.reactive.subjects.Subject
import org.scalatest.Suite

trait WithState extends DBCacheSettings {
  protected val ignoreSpendableBalanceChanged: Subject[(Address, Asset), (Address, Asset)] = Subject.empty
  protected def withState[A](fs: FunctionalitySettings)(f: LevelDBWriter => A): A = {
    val path = Files.createTempDirectory("leveldb-test")
    val db   = openDB(path.toAbsolutePath.toString)
    try f(new LevelDBWriter(db, ignoreSpendableBalanceChanged, fs, dbSettings))
    finally {
      db.close()
      TestHelpers.deleteRecursively(path)
    }
  }

  def withStateAndHistory(fs: FunctionalitySettings)(test: LevelDBWriter => Any): Unit = withState(fs)(test)
}

trait WithDomain extends WithState with NTPTime {
  _: Suite =>

  def withDomain[A](settings: AcrylSettings = AcrylSettings.fromRootConfig(loadConfig(ConfigFactory.load())))(test: Domain => A): A = {
    try withState(settings.blockchainSettings.functionalitySettings) { blockchain =>
      val bcu = new BlockchainUpdaterImpl(blockchain, ignoreSpendableBalanceChanged, settings, ntpTime)
      try test(Domain(bcu))
      finally bcu.shutdown()
    } finally {}
  }
}
