package com.acrylplatform.it.sync.activation

import com.acrylplatform.features.BlockchainFeatureStatus
import com.acrylplatform.features.api.{FeatureActivationStatus, NodeFeatureStatus}
import org.scalactic.source.Position
import org.scalatest.Matchers

trait ActivationStatusRequest extends Matchers {
  def assertVotingStatus(fas: FeatureActivationStatus,
                         supportedBlocks: Int,
                         blockchainFeatureStatus: BlockchainFeatureStatus,
                         nodeFeatureStatus: NodeFeatureStatus)(implicit pos: Position): Unit = {
    withClue("supportedBlocks") {
      fas.supportingBlocks should contain(supportedBlocks)
    }
    withClue("blockchainStatus") {
      fas.blockchainStatus shouldBe blockchainFeatureStatus
    }
    withClue("nodeStatus") {
      fas.nodeStatus shouldBe nodeFeatureStatus
    }
  }

  def assertApprovedStatus(fas: FeatureActivationStatus, height: Int, nodeFeatureStatus: NodeFeatureStatus)(implicit pos: Position): Unit = {
    withClue("activationHeight") {
      fas.activationHeight should contain(height)
    }
    withClue("blockchainStatus") {
      fas.blockchainStatus shouldBe BlockchainFeatureStatus.Approved
    }
    withClue("nodeStatus") {
      fas.nodeStatus shouldBe nodeFeatureStatus
    }
  }

  def assertActivatedStatus(fas: FeatureActivationStatus, height: Int, nodeFeatureStatus: NodeFeatureStatus)(implicit pos: Position): Unit = {
    withClue("activationHeight") {
      fas.activationHeight should contain(height)
    }
    withClue("blockchainStatus") {
      fas.blockchainStatus shouldBe BlockchainFeatureStatus.Activated
    }
    withClue("nodeStatus") {
      fas.nodeStatus shouldBe nodeFeatureStatus
    }
  }
}
