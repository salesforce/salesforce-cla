/*
 * Copyright (c) 2018, salesforce.com, inc.
 * All rights reserved.
 * SPDX-License-Identifier: BSD-3-Clause
 * For full license text, see the LICENSE.txt file in the repo root or https://opensource.org/licenses/BSD-3-Clause
 */

package utils

import java.time.LocalDateTime

import models.ClaSignature
import modules.{Database, DatabaseMock}
import org.scalatest.BeforeAndAfterAll
import org.scalatestplus.play.PlaySpec
import pdi.jwt.{JwtClaim, JwtJson}
import play.api.i18n.MessagesApi
import play.api.inject._
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json._
import play.api.libs.ws.WSClient
import play.api.test.Helpers._
import play.api.{Configuration, Mode}
import utils.GitHub._

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Random, Success, Try}


class GitHubSpec extends PlaySpec with BeforeAndAfterAll {

  lazy val app = new GuiceApplicationBuilder()
    .overrides(bind[Database].to[DatabaseMock])
    .configure(
      Map(
        "play.modules.disabled" -> Seq("org.flywaydb.play.PlayModule", "modules.DatabaseModule")
      )
    )
    .in(Mode.Test)
    .build()

  lazy val wsClient = app.injector.instanceOf[WSClient]

  lazy val messagesApi = app.injector.instanceOf[MessagesApi]

  lazy val gitHub = new GitHub(app.configuration, wsClient, messagesApi)(ExecutionContext.global)

  val urlF = (_: OwnerRepo, _: Int) => "http://asdf.com"

  val testToken1 = sys.env("GITHUB_TEST_TOKEN1")
  val testToken2 = sys.env("GITHUB_TEST_TOKEN2")
  val testOrg = Owner(sys.env("GITHUB_TEST_ORG"))

  lazy val testUser1 = await(gitHub.userInfo(testToken1))
  lazy val testUser2 = await(gitHub.userInfo(testToken2))

  lazy val testIntegrationInstallationId: Int = {
    val integrationInstallations = await(gitHub.integrationInstallations())

    integrationInstallations.value.find { json =>
      (json \ "account" \ "login").asOpt[String].contains(testUser1.username)
    }.flatMap { json =>
      (json \ "id").asOpt[Int]
    }.getOrElse(throw new IllegalStateException(s"$testUser1 must have the integration ${gitHub.integrationId} installed"))
  }

  lazy val testIntegrationInstallationIdOrg: Int = {
    val integrationInstallations = await(gitHub.integrationInstallations())

    integrationInstallations.value.find { json =>
      (json \ "account" \ "login").asOpt[String].contains(testOrg.name)
    }.flatMap { json =>
      (json \ "id").asOpt[Int]
    }.getOrElse(throw new IllegalStateException(s"$testOrg must have the integration ${gitHub.integrationId} installed"))
  }

  lazy val testIntegrationToken = (await(gitHub.installationAccessTokens(testIntegrationInstallationId)) \ "token").as[String]
  lazy val testIntegrationTokenOrg = (await(gitHub.installationAccessTokens(testIntegrationInstallationIdOrg)) \ "token").as[String]

  val maxTries = 10

  // Poll until the repo has commits - So much gross
  @tailrec
  private def waitForRepo(ownerRepo: OwnerRepo, token: String, tryNum: Int = 0) {
    val repo = Try(await(gitHub.repo(ownerRepo)(token)))

    if (repo.isFailure && (tryNum < maxTries)) {
      Thread.sleep(1000)
      waitForRepo(ownerRepo, token, tryNum + 1)
    }
    else if (tryNum >= maxTries) {
      throw new Exception("max tries reached")
    }
  }

  @tailrec
  private def waitForCommits(ownerRepo: OwnerRepo, token: String, tryNum: Int = 0) {
    val repoCommits = Try(await(gitHub.repoCommits(ownerRepo, token))).getOrElse(JsArray())

    if (repoCommits.value.isEmpty && tryNum < maxTries) {
      Thread.sleep(1000)
      waitForCommits(ownerRepo, token, tryNum + 1)
    }
    else if (tryNum >= maxTries) {
      throw new Exception("max tries reached")
    }
  }

  @tailrec
  private def waitForCommit(ownerRepo: OwnerRepo, sha: String, token: String, tryNum: Int = 0) {
    val repoCommit = await(gitHub.repoCommits(ownerRepo, token)).value.find(_.\("sha").as[String] == sha)

    if (repoCommit.isEmpty && (tryNum < maxTries)) {
      Thread.sleep(1000)
      waitForCommit(ownerRepo, sha, token, tryNum + 1)
    }
    else if (tryNum >= maxTries) {
      throw new Exception("max tries reached")
    }
  }

  @tailrec
  private def waitForFileToBeReady(ownerRepo: OwnerRepo, path: String, ref: String, accessToken: String, tryNum: Int = 0): Unit = {
    val file = Try(await(gitHub.getFile(ownerRepo, path, Some(ref))(accessToken)))

    if (file.isFailure && (tryNum < maxTries)) {
      Thread.sleep(1000)
      waitForFileToBeReady(ownerRepo, path, ref, accessToken, tryNum + 1)
    }
    else if (tryNum >= maxTries) {
      throw new Exception("max tries reached")
    }
  }

  @tailrec
  private def waitForPullRequest(ownerRepo: OwnerRepo, prNumber: Int, accessToken: String, tryNum: Int = 0): Unit = {

    val pr = Try(await(gitHub.getPullRequest(ownerRepo, prNumber, accessToken)))

    if (pr.isFailure && (tryNum < maxTries)) {
      Thread.sleep(1000)
      waitForPullRequest(ownerRepo, prNumber, accessToken, tryNum + 1)
    }
    else if (tryNum >= maxTries) {
      throw new Exception("max tries reached")
    }
  }

  @tailrec
  private def waitForCommitState(ownerRepo: OwnerRepo, sha: String, state: String, accessToken: String, tryNum: Int = 0): Unit = {
    val status = Try(await(gitHub.commitStatus(ownerRepo, sha, accessToken))).getOrElse(Json.obj())

    if (!(status \ "state").asOpt[String].contains(state) && (tryNum < maxTries)) {
      Thread.sleep(1000)
      waitForCommitState(ownerRepo, sha, state, accessToken, tryNum + 1)
    }
    else if (tryNum >= maxTries) {
      throw new Exception("max tries reached")
    }
  }

  @tailrec
  private def waitForPullRequestToHaveSha(ownerRepo: OwnerRepo, prNumber: Int, sha: String, accessToken: String, tryNum: Int = 0): Unit = {
    val pullRequest = Try(await(gitHub.getPullRequest(ownerRepo, prNumber, accessToken))).getOrElse(Json.obj())

    val prSha = (pullRequest \ "head" \ "sha").as[String]

    if (prSha != sha && (tryNum < maxTries)) {
      Thread.sleep(1000)
      waitForPullRequestToHaveSha(ownerRepo, prNumber, sha, accessToken, tryNum + 1)
    }
    else if (tryNum >= maxTries) {
      throw new Exception("max tries reached")
    }
  }

  private def addUnknownCommit(ownerRepo: OwnerRepo): String = {
    val commit = await(gitHub.repoCommits(ownerRepo, testToken1)).value.head

    val sha = (commit \ "sha").as[String]
    val tree = (commit \ "commit" \ "tree" \ "sha").as[String]

    val author = unknownCommitter(ownerRepo) match {
      case UnknownCommitter(Some(name), Some(email)) => Some(name -> email)
      case _ => None
    }

    val newCommit = await(gitHub.commit(ownerRepo, "test non-github commit", tree, Set(sha), author, testToken1))

    (newCommit \ "sha").as[String]
  }

  private def createRepo(): OwnerRepo = {
    val repoName = Random.alphanumeric.take(8).mkString
    val ownerRepo = await(gitHub.createRepo(repoName, None, true)(testToken1))

    waitForCommits(ownerRepo, testToken1)
    waitForCommits(ownerRepo, testToken2)

    ownerRepo
  }

  private def createOrgRepo(owner: Owner): OwnerRepo = {
    val repoName = Random.alphanumeric.take(8).mkString
    val ownerRepo = await(gitHub.createRepo(repoName, Some(owner), true)(testToken1))

    waitForCommits(ownerRepo, testToken1)
    waitForCommits(ownerRepo, testToken2)

    val newSha = addUnknownCommit(ownerRepo)

    await(gitHub.updateGitRef(ownerRepo, newSha, "heads/master", testToken1))

    waitForCommit(ownerRepo, newSha, testToken1)

    ownerRepo
  }

  private def createFork(): OwnerRepo = {
    val forkResult = await(gitHub.forkRepo(testRepo1)(testToken2))
    val forkOwnerRepo = forkResult.as[OwnerRepo]

    waitForCommits(forkOwnerRepo, testToken1)
    waitForCommits(forkOwnerRepo, testToken2)

    forkOwnerRepo
  }

  lazy val testRepo1 = createRepo()
  lazy val testRepo2 = createRepo()
  lazy val testRepo3 = createRepo()
  lazy val testFork = createFork()
  lazy val testOrgRepo = createOrgRepo(testOrg)

  def unknownCommitter(ownerRepo: OwnerRepo) = {
    UnknownCommitter(Some(ownerRepo.repo.name), Some(s"${ownerRepo.repo.name}@$testOrg.com".toLowerCase))
  }

  def createTestExternalPullRequest() = {
    val testRepos2 = await(gitHub.userRepos(testUser2, testToken2))

    // make sure testToken2 does not have access to the upstream repo
    testRepos2 must not contain (testRepo1)

    val readme = await(gitHub.getFile(testFork, "README.md")(testToken2))

    val maybeReadmeSha = (readme \ "sha").asOpt[String]

    maybeReadmeSha must be ('defined)

    val readmeSha = maybeReadmeSha.get

    val commits = await(gitHub.repoCommits(testFork, testToken1))

    val sha = (commits.value.head \ "sha").as[String]

    waitForFileToBeReady(testFork, "README.md", sha, testToken1)
    waitForFileToBeReady(testFork, "README.md", sha, testToken2)

    val newContents = Random.alphanumeric.take(32).mkString

    // external pull request
    val editResult = await(gitHub.editFile(testFork, "README.md", newContents, "Updated", readmeSha)(testToken2))
    (editResult \ "commit").asOpt[JsObject] must be ('defined)
    val editSha = (editResult \ "commit" \ "sha").as[String]
    waitForCommit(testFork, editSha, testToken1)
    waitForCommit(testFork, editSha, testToken2)

    // testToken2 create PR to testRepo1
    val externalPullRequest = await(gitHub.createPullRequest(testRepo1, "Updates", s"${testUser2.username}:master", "master", testToken2))
    (externalPullRequest \ "id").asOpt[Int] must be ('defined)
    val prNumber = (externalPullRequest \ "number").as[Int]
    waitForPullRequest(testRepo1, prNumber, testToken1)
    waitForPullRequest(testRepo1, prNumber, testToken2)

    externalPullRequest
  }

  lazy val testExternalPullRequest = Json.obj("pull_request" -> createTestExternalPullRequest())


  def createTestInternalPullRequest() = {
    val newContents = Random.alphanumeric.take(32).mkString
    val newBranchName = Random.alphanumeric.take(8).mkString

    val readmeSha1 = (await(gitHub.getFile(testRepo1, "README.md")(testToken1)) \ "sha").as[String]

    val commits = await(gitHub.repoCommits(testRepo1, testToken1))

    val sha = (commits.value.head \ "sha").as[String]

    waitForFileToBeReady(testRepo1, "README.md", sha, testToken1)
    waitForFileToBeReady(testRepo1, "README.md", sha, testToken2)

    val newBranch = await(gitHub.createBranch(testRepo1, newBranchName, sha, testToken1))

    waitForFileToBeReady(testRepo1, "README.md", newBranchName, testToken1)
    waitForFileToBeReady(testRepo1, "README.md", newBranchName, testToken2)

    val internalEditResult1 = await(gitHub.editFile(testRepo1, "README.md", newContents, "Updated", readmeSha1, Some(newBranchName))(testToken1))
    (internalEditResult1 \ "commit").asOpt[JsObject] must be ('defined)
    val editSha1 = (internalEditResult1 \ "commit" \ "sha").as[String]

    waitForCommitState(testRepo1, editSha1, "pending", testToken1)

    val readmeSha2 = (await(gitHub.getFile(testRepo1, "README.md", Some(newBranchName))(testToken1)) \ "sha").as[String]

    val internalPullRequest = await(gitHub.createPullRequest(testRepo1, "Updates", newBranchName, "master", testToken1))
    (internalPullRequest \ "id").asOpt[Int] must be ('defined)
    val prNumber = (internalPullRequest \ "number").as[Int]

    waitForPullRequest(testRepo1, prNumber, testToken1)
    waitForPullRequest(testRepo1, prNumber, testToken2)

    val internalEditResult2 = await(gitHub.editFile(testRepo1, "README.md", "updated by bot", "updated by bot", readmeSha2, Some(newBranchName))(testIntegrationToken))
    val editSha2 = (internalEditResult2 \ "commit" \ "sha").as[String]

    waitForPullRequestToHaveSha(testRepo1, prNumber, editSha2, testToken1)

    await(gitHub.getPullRequest(testRepo1, prNumber, testToken1))
  }

  def createTestUnknownPullRequest() = {
    def sha = addUnknownCommit(testRepo1)
    val newBranchName = Random.alphanumeric.take(8).mkString

    await(gitHub.createBranch(testRepo1, newBranchName, sha, testToken1))

    val pullRequest = await(gitHub.createPullRequest(testRepo1, "Updates", newBranchName, "master", testToken1))
    (pullRequest \ "id").asOpt[Int] must be ('defined)
    val prNumber = (pullRequest \ "number").as[Int]

    waitForPullRequest(testRepo1, prNumber, testToken1)
    waitForPullRequest(testRepo1, prNumber, testToken2)

    await(gitHub.getPullRequest(testRepo1, prNumber, testToken1))
  }

  lazy val testInternalPullRequest = Json.obj("pull_request" -> createTestInternalPullRequest())


  lazy val testPullRequests = Map(testExternalPullRequest -> testToken2, testInternalPullRequest -> testToken1)

  lazy val testExternalPullRequestOwnerRepo = (testExternalPullRequest \ "pull_request" \ "base" \ "repo").as[OwnerRepo]
  lazy val testExternalPullRequestNum = (testExternalPullRequest \ "pull_request" \ "number").as[Int]
  lazy val testExternalPullRequestSha = (testExternalPullRequest \ "pull_request" \ "head" \ "sha").as[String]

  lazy val testInternalPullRequestOwnerRepo = (testInternalPullRequest \ "pull_request" \ "base" \ "repo").as[OwnerRepo]
  lazy val testInternalPullRequestNum = (testInternalPullRequest \ "pull_request" \ "number").as[Int]
  lazy val testInternalPullRequestSha = (testInternalPullRequest \ "pull_request" \ "head" \ "sha").as[String]

  lazy val testUnknownPullRequest = Json.obj("pull_request" -> createTestUnknownPullRequest())
  lazy val testUnknownPullRequestOwnerRepo = (testUnknownPullRequest \ "pull_request" \ "base" \ "repo").as[OwnerRepo]
  lazy val testUnknownPullRequestNum = (testUnknownPullRequest \ "pull_request" \ "number").as[Int]
  lazy val testUnknownPullRequestSha = (testUnknownPullRequest \ "pull_request" \ "head" \ "sha").as[String]

  override def beforeAll() = {
    testRepo1
    testRepo2
    testRepo3
    testFork
    testOrgRepo
    testPullRequests

    withClue(s"$testUser2 must not be a member on $testRepo1: ") {
      val testRepo1Collaborators = await(gitHub.orgMembers(testRepo1.owner, testToken1))
      testRepo1Collaborators must not contain testUser2
    }

    withClue(s"$testUser2 must be a private member of $testOrg: ") {
      val allOrgMembers = await(gitHub.orgMembers(testOrg, testToken1))
      allOrgMembers must contain (testUser2)
      val publicOrgMembers = await(gitHub.orgMembers(testOrg, testIntegrationToken))
      publicOrgMembers must not contain testUser2
    }

    withClue(s"the integration must be installed on $testOrg: ") {
      val integrationInstallations = await(gitHub.integrationInstallations())
      integrationInstallations.as[Seq[JsObject]].map(_.\("account").as[Owner]) must contain (testOrg)
    }

    withClue(s"the integration must be installed on $testUser1: ") {
      val integrationInstallations = await(gitHub.integrationInstallations())
      integrationInstallations.as[Seq[JsObject]].map(_.\("account").as[User]) must contain (testUser1)
    }
  }

  "GitHub.userRepos" must {
    "fetch all the repos with 10 pages" in {
      (testRepo1, testRepo2, testRepo3) // create 3 repos lazily
      val repos = await(gitHub.userRepos(testUser1, testToken1, 1))
      repos.size must be >= 3
    }
    "fetch all the repos without paging" in {
      val repos = await(gitHub.userRepos(testUser1, testToken1, Int.MaxValue))
      repos.size must be >= 3
    }
    "fetch all the repos with 2 pages" in {
      val repos = await(gitHub.userRepos(testUser1, testToken1, 2))
      repos.size must be >= 3
    }
  }

  "GitHub.userInfo" must {
    "fetch the userInfo" in {
      val user = await(gitHub.userInfo(testToken1))
      user must equal (testUser1)
    }
  }

  "GitHub.getPullRequest" must {
    "get a PR" in {
      val pr = await(gitHub.getPullRequest(testExternalPullRequestOwnerRepo, testExternalPullRequestNum, testToken1))
      (pr \ "id").as[Int] must equal ((testExternalPullRequest \ "pull_request" \ "id").as[Int])
    }
  }

  "GitHub.updatePullRequestStatus" must {
    "update a PR" in {
      val pr = await(gitHub.getPullRequest(testExternalPullRequestOwnerRepo, testExternalPullRequestNum, testToken1))
      val mergeState = (pr \ "mergeable_state").as[String]
      val sha = (pr \ "head" \ "sha").as[String]
      val state = if (mergeState == "clean") "pending" else "success"
      val statusCreate = await(gitHub.createStatus(testExternalPullRequestOwnerRepo, sha, state, "https://salesforce.com", "This is only a test", "salesforce-cla:GitHubSpec", testToken1))
      (statusCreate \ "state").as[String] must equal (state)
    }
  }

  "GitHub.pullRequestCommits" must {
    "get the commits on a PR" in {
      val commits = await(gitHub.pullRequestCommits(testExternalPullRequestOwnerRepo, testExternalPullRequestNum, testToken1))
      commits.value.length must be > 0
    }
  }

  "GitHub.commentOnIssue" must {
    "comment on an issue" in {
      val commentCreate = await(gitHub.commentOnIssue(testExternalPullRequestOwnerRepo, testExternalPullRequestNum, "This is only a test.", testToken1))
      (commentCreate \ "id").asOpt[Int] must be ('defined)
    }
  }

  "GitHub.pullRequests" must {
    "get the pull requests" in {
      val pullRequestsInRepo = await(gitHub.pullRequests(testExternalPullRequestOwnerRepo, testToken1))
      pullRequestsInRepo.value.length must be > 0
    }
    "be able to filter" in {
      val closedPullRequests = await(gitHub.pullRequests(testExternalPullRequestOwnerRepo, testToken1, Some("closed")))
      closedPullRequests.value.length must equal (0)
    }
  }

  "GitHub.pullRequestsToValidate" must {
    lazy val testPullRequest = (testInternalPullRequest \ "pull_request").as[JsObject]
    "work" in {
      val pullRequestsToValidate = await(gitHub.pullRequestsToValidate(testPullRequest, testIntegrationToken))
      pullRequestsToValidate must not be empty
    }
    "not include closed pull requests" in {
      val closedPullRequest = testPullRequest + ("state" -> JsString("closed"))
      val pullRequestsToValidate = await(gitHub.pullRequestsToValidate(closedPullRequest, testIntegrationToken))
      pullRequestsToValidate must be (empty)
    }
  }

  "GitHub.commitStatus" must {
    "get the commit status" in {
      val sha = (testExternalPullRequest \ "pull_request" \ "head" \ "sha").as[String]
      await(gitHub.createStatus(testExternalPullRequestOwnerRepo, sha, "failure", "http://asdf.com", "asdf", "salesforce-cla:GitHubSpec", testToken1))
      val commitStatus = await(gitHub.commitStatus(testExternalPullRequestOwnerRepo, sha, testToken1))
      (commitStatus \ "state").as[String] must equal ("failure")
    }
  }

  "GitHub.issueComments" must {
    "get the issue comments" in {
      await(gitHub.commentOnIssue(testExternalPullRequestOwnerRepo, testExternalPullRequestNum, "This is only a test.", testToken1))
      val issueComments = await(gitHub.issueComments(testExternalPullRequestOwnerRepo, testExternalPullRequestNum, testToken1))
      issueComments.value.length must be > 0
    }
  }

  "GitHub.applyLabel" must {
    "apply a label to issue" in {
      val appliedLabels = await(gitHub.applyLabel(testExternalPullRequestOwnerRepo, MissingLabel, testExternalPullRequestNum, testToken1))
      appliedLabels.value.map(_.\("name").as[String]) must contain (MissingLabel.name)
    }

    "have the right color" in {
      val appliedLabels = await(gitHub.applyLabel(testExternalPullRequestOwnerRepo, MissingLabel, testExternalPullRequestNum, testToken1))
      val appliedLabel = appliedLabels.value.find(_.\("name").as[String] == MissingLabel.name).get
      (appliedLabel \ "color").as[String] must equal (MissingLabel.color)
    }
  }

  "GitHub.getIssueLabels" must {
    "get labels on an issue" in {
      val issueLabels = await(gitHub.getIssueLabels(testExternalPullRequestOwnerRepo, testExternalPullRequestNum, testToken1))
      issueLabels.value.map(_.\("name").as[String]) must contain (MissingLabel.name)
    }
  }

  "GitHub.removeLabel" must {
    "remove a label from issue" in {
      val removedLabel = await(gitHub.removeLabel(testExternalPullRequestOwnerRepo, MissingLabel, testExternalPullRequestNum, testToken1))
      removedLabel must equal (())
    }
  }

  "GitHub.orgMembers" must {
    "get the org members" in {
      val members = await(gitHub.orgMembers(testOrg, testToken1))
      members.size must be > 0
    }
  }

  "GitHub.repoCommit" must {
    "get the repo commit" in {
      val commit = await(gitHub.repoCommit(testExternalPullRequestOwnerRepo, testExternalPullRequestSha, testToken1))
      (commit \ "sha").as[String] must equal (testExternalPullRequestSha)
    }
  }

  "GitHub.repoCommits" must {
    "get the repo commits" in {
      val commits = await(gitHub.repoCommits(testExternalPullRequestOwnerRepo, testToken1))
      commits.value.length must be > 0
    }
  }

  "GitHub.jwtEncode" must {
    "work" in {
      val claim = JwtClaim(subject = Some("test"))
      val encoded = gitHub.jwtEncode(claim)
      val Success(decoded) = JwtJson.decode(encoded, gitHub.integrationKeyPair.getPublic)

      decoded.subject must contain ("test")
    }
  }

  "GitHub.installationAccessTokens" must {
    "work" in {
      val result = await(gitHub.installationAccessTokens(testIntegrationInstallationId))
      (result \ "token").asOpt[String] must be ('defined)
    }
  }

  "GitHub.integrationInstallations" must {
    "work" in {
      val integrations = await(gitHub.integrationInstallations())
      integrations.value.length must be > 0
    }
  }

  "GitHub.installationRepositories" must {
    "work" in {
      val token = (await(gitHub.installationAccessTokens(testIntegrationInstallationId)) \ "token").as[String]
      val repos = await(gitHub.installationRepositories(token))
      repos.value.length must be > 0
    }
  }

  // note that ordering is important here because we validate the same PR multiple times
  "GitHub.pullRequestsToBeValidated" must {
    lazy val pullRequest = createTestInternalPullRequest()
    lazy val sha = (pullRequest \ "head" \ "sha").as[String]
    lazy val number = (pullRequest \ "number").as[Int]

    "work" in {
      val pullRequestsToBeValidated = await(gitHub.pullRequestsToBeValidated(testUser1))
      pullRequestsToBeValidated must be (empty)
    }
    "include failure state pull requests" in {
      await(gitHub.createStatus(testRepo1, sha, "failure", "http://foo.com", "testing", "salesforce-cla", testToken1))

      waitForCommitState(testRepo1, sha, "failure", testToken1)

      val pullRequestsToBeValidated = await(gitHub.pullRequestsToBeValidated(testUser1))
      pullRequestsToBeValidated must not be empty
    }
    "not include closed pull requests" in {
      await(gitHub.closePullRequest(testRepo1, number, testToken1))

      val pullRequestsToBeValidatedPostClose = await(gitHub.pullRequestsToBeValidated(testUser1))
      pullRequestsToBeValidatedPostClose must be (empty)
    }
  }

  "GitHub.pullRequestUserCommitters" must {
    "work" in {
      val pullRequestUserCommitters = await(gitHub.pullRequestUserCommitters(testInternalPullRequestOwnerRepo, testInternalPullRequestNum, testInternalPullRequestSha, testIntegrationToken))
      pullRequestUserCommitters must contain (testUser1)
    }
    "fail with non-github user contributors" in {
      val pullRequestUserCommitters = await(gitHub.pullRequestUserCommitters(testUnknownPullRequestOwnerRepo, testUnknownPullRequestNum, testUnknownPullRequestSha, testIntegrationToken))
      pullRequestUserCommitters must equal (Set(unknownCommitter(testUnknownPullRequestOwnerRepo)))
    }
  }

  "GitHub.externalContributorsForPullRequest" must {
    "not include repo collaborators" in {
      val externalContributors = await(gitHub.externalContributorsForPullRequest(testInternalPullRequestOwnerRepo, testInternalPullRequestNum, testInternalPullRequestSha, testIntegrationToken))
      externalContributors must be ('empty) // todo: not include the internal user
    }
  }

  // note that ordering is important here because we validate the same PR multiple times
  "GitHub.validatePullRequests" must {
    "work with integrations for pull requests with only internal contributors and/or bots" in {
      val pullRequestsViaIntegration = Map(testInternalPullRequest -> testIntegrationToken)

      val validationResultsFuture = gitHub.validatePullRequests(pullRequestsViaIntegration, urlF, urlF) { _ =>
        Future.successful(Set.empty[ClaSignature])
      }

      val validationResults = await(validationResultsFuture)

      validationResults.size must equal (1)
      (validationResults.head._3 \ "creator" \ "login").as[String].endsWith("[bot]") must be (true)
      (validationResults.head._3 \ "state").as[String] must equal ("success")

      val labels = await(gitHub.getIssueLabels(testInternalPullRequestOwnerRepo, testInternalPullRequestNum, testIntegrationToken))
      labels.value must be ('empty)

      val issueComments = await(gitHub.issueComments(testInternalPullRequestOwnerRepo, testInternalPullRequestNum, testIntegrationToken))
      issueComments.value must be ('empty)
    }
    "not comment on a pull request when the external contributors have signed the CLA" in {
      val pullRequestsViaIntegration = Map(testExternalPullRequest -> testIntegrationToken)

      val validationResultsFuture = gitHub.validatePullRequests(pullRequestsViaIntegration, urlF, urlF) { _ =>
        Future.successful(Set(ClaSignature(1, testUser2.username, LocalDateTime.now(), "1.0")))
      }

      val validationResults = await(validationResultsFuture)
      validationResults.size must equal (1)
      (validationResults.head._3 \ "state").as[String] must equal ("success")

      val labels = await(gitHub.getIssueLabels(testExternalPullRequestOwnerRepo, testExternalPullRequestNum, testIntegrationToken))
      labels.value.exists(_.\("name").as[String] == "cla:signed") must be (true)

      val issueComments = await(gitHub.issueComments(testExternalPullRequestOwnerRepo, testExternalPullRequestNum, testIntegrationToken))
      issueComments.value.count(_.\("user").\("login").as[String].endsWith("[bot]")) must equal (0)
    }
    "work with integrations for pull requests with external contributors" in {
      val pullRequestsViaIntegration = Map(testExternalPullRequest -> testIntegrationToken)

      val validationResultsFuture = gitHub.validatePullRequests(pullRequestsViaIntegration, urlF, urlF) { _ =>
        Future.successful(Set.empty[ClaSignature])
      }

      val validationResults = await(validationResultsFuture)
      validationResults.size must equal (1)
      (validationResults.head._3 \ "creator" \ "login").as[String].endsWith("[bot]") must be (true)
      (validationResults.head._3 \ "state").as[String] must equal ("failure")

      val labels = await(gitHub.getIssueLabels(testExternalPullRequestOwnerRepo, testExternalPullRequestNum, testIntegrationToken))
      labels.value.exists(_.\("name").as[String] == "cla:missing") must be (true)

      val issueComments = await(gitHub.issueComments(testExternalPullRequestOwnerRepo, testExternalPullRequestNum, testIntegrationToken))
      issueComments.value.count(_.\("user").\("login").as[String].endsWith("[bot]")) must equal (1)
    }
    "not comment twice on the same pull request" in {
      val pullRequestsViaIntegration = Map(testExternalPullRequest -> testIntegrationToken)

      await(gitHub.validatePullRequests(pullRequestsViaIntegration, urlF, urlF)(_ => Future.successful(Set.empty[ClaSignature])))
      await(gitHub.validatePullRequests(pullRequestsViaIntegration, urlF, urlF)(_ => Future.successful(Set.empty[ClaSignature])))

      val issueComments = await(gitHub.issueComments(testExternalPullRequestOwnerRepo, testExternalPullRequestNum, testToken1))
      issueComments.value.count(_.\("user").\("login").as[String].endsWith("[bot]")) must equal (1)
    }
    "comment for internal committer" in {
      val pullRequestWithCommits = await(gitHub.pullRequestWithCommitsAndStatus(testToken1)((testExternalPullRequest \ "pull_request").as[JsObject]))

      val commit = (pullRequestWithCommits \ "commits").as[Seq[JsObject]].head

      val user = commit.as[Contributor].asInstanceOf[User]

      val contributorDomain = user.maybeEmail.get.split("@").last

      val instructionsUrl = "http://foo.com"

      val config = app.configuration ++ Configuration(
        "app.organization.domain" -> contributorDomain,
        "app.organization.internal-instructions-url" -> instructionsUrl
      )
      val gitHubWithInternalConfig = new GitHub(config, wsClient, messagesApi)(ExecutionContext.global)

      val externalPullRequestWithToken = Map(testExternalPullRequest -> testToken1)
      await(gitHubWithInternalConfig.validatePullRequests(externalPullRequestWithToken, urlF, urlF)(_ => Future.successful(Set.empty[ClaSignature])))
      val issueComments = await(gitHub.issueComments(testExternalPullRequestOwnerRepo, testExternalPullRequestNum, testToken1)).value.map(_.\("body").as[String])
      issueComments.exists(_.contains(s"It looks like @${user.username} is an internal user")) must be (true)
    }
  }

  "repoContributors" should {
    "work" in {
      val repoContributors = await(gitHub.repoContributors(testRepo1, testIntegrationToken)).map(_.contributor)
      repoContributors contains testUser1
      repoContributors contains unknownCommitter(testRepo1)
    }
  }

  "UnknownCommitter.toStringOpt" should {
    "obfuscate emails" in {
      UnknownCommitter(None, Some("asdf@foobar.com")).toStringOpt() must contain ("a***@f***.com")
      UnknownCommitter(None, Some("asdf@foo.bar.com")).toStringOpt() must contain ("a***@f***.b***.com")
    }
  }

  override def afterAll() = {
    if (sys.env.get("DO_NOT_CLEANUP").isEmpty) {
      await(gitHub.deleteRepo(testFork)(testToken2))
      await(gitHub.deleteRepo(testRepo1)(testToken1))
      await(gitHub.deleteRepo(testRepo2)(testToken1))
      await(gitHub.deleteRepo(testRepo3)(testToken1))
      await(gitHub.deleteRepo(testOrgRepo)(testToken1))
    }

    await(app.stop())
  }

}
